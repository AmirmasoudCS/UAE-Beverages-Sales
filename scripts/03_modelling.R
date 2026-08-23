source("scripts/00_setup.R")

# ==============================================================================
# 1. Load cleaned data
# ==============================================================================

df <- readRDS(file.path(data_dir, "clean_sales.rds"))

message("Loaded cleaned dataset: ", nrow(df), " rows x ", ncol(df), " columns")

theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", size = 10.5),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

theme_set(theme_report)

color_main <- "steelblue"
color_accent <- "#e07b39"

# ==============================================================================
# 2. Clustering: does Category naturally split into price/revenue tiers?
# ==============================================================================

# This directly tests the "premium vs everyday" hypothesis we noticed visually
# in the price boxplot during EDA, but did not encode as a real grouping since
# it wasn't confirmed by the data at the time. K-means lets the data decide.

category_features <- df %>%
  group_by(Category) %>%
  summarise(
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE),
    Total_Net_Sales = sum(Net_Sales, na.rm = TRUE),
    .groups = "drop"
  )

# Scale features so Price (small numbers) and Net_Sales (large numbers) don't
# distort distance calculations purely due to their different units/scales.
cluster_input <- category_features %>%
  select(Avg_Price, Avg_Quantity, Total_Net_Sales) %>%
  scale()

rownames(cluster_input) <- category_features$Category

# Check how many clusters the data actually supports before picking one,
# using the elbow method on within-cluster sum of squares.
wss <- map_dbl(1:6, function(k) {
  kmeans(cluster_input, centers = k, nstart = 25)$tot.withinss
})

elbow_df <- tibble(k = 1:6, wss = wss)

p_elbow <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = color_main, linewidth = 0.8) +
  geom_point(color = color_main, size = 2.5) +
  scale_x_continuous(breaks = 1:6) +
  labs(
    title = "K-Means Elbow Plot",
    subtitle = "Choosing the number of clusters for category segmentation",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  )

ggsave(
  file.path(plots_dir, "cluster_elbow_plot.png"),
  p_elbow, width = 7, height = 5, dpi = 300
)

# With only 7 categories, k = 2 is the most defensible choice: it matches the
# "premium vs everyday" pattern seen visually in EDA without over-fragmenting
# a very small dataset (7 points) into clusters of size 1-2.
set.seed(42)
category_kmeans <- kmeans(cluster_input, centers = 2, nstart = 25)

category_features <- category_features %>%
  mutate(Cluster = factor(category_kmeans$cluster))

print(category_features %>% arrange(Cluster, desc(Avg_Price)))

p_cluster_scatter <- ggplot(
  category_features,
  aes(x = Avg_Price, y = Total_Net_Sales, color = Cluster, label = Category)
) +
  geom_point(size = 4) +
  ggrepel::geom_text_repel(size = 3.5, show.legend = FALSE) +
  scale_x_continuous(labels = dollar_format(prefix = "AED ")) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c(color_main, color_accent)) +
  labs(
    title = "Category Clusters by Price and Revenue",
    subtitle = "K-means (k = 2) on average price, average quantity, and total revenue",
    x = "Average Price",
    y = "Total Net Sales (AED)"
  )

ggsave(
  file.path(plots_dir, "category_clusters.png"),
  p_cluster_scatter, width = 8, height = 6, dpi = 300
)

# ==============================================================================
# 3. Classification: how well does Price alone predict Category?
# ==============================================================================

# Quantifies what the EDA boxplot showed visually: category price ranges
# overlap but are not identical. A simple multinomial model tells us exactly
# how separable categories are using price alone.

model_data <- df %>%
  select(Category, Price) %>%
  mutate(Category = factor(Category))

set.seed(42)
train_idx <- sample(seq_len(nrow(model_data)), size = 0.75 * nrow(model_data))
train_data <- model_data[train_idx, ]
test_data <- model_data[-train_idx, ]

category_model <- nnet::multinom(Category ~ Price, data = train_data, trace = FALSE)

predicted <- predict(category_model, newdata = test_data)
actual <- test_data$Category

accuracy <- mean(predicted == actual)
message("Classification accuracy (Category from Price alone): ", round(accuracy * 100, 1), "%")

confusion <- table(Predicted = predicted, Actual = actual)
print(confusion)

confusion_df <- as.data.frame(confusion) %>%
  group_by(Actual) %>%
  mutate(Share = Freq / sum(Freq)) %>%
  ungroup()

p_confusion <- ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Share)) +
  geom_tile(color = "white") +
  geom_text(aes(label = percent(Share, accuracy = 1)), size = 3.2) +
  scale_fill_gradient(low = "white", high = color_main, labels = percent) +
  labs(
    title = "Confusion Matrix: Predicting Category from Price",
    subtitle = paste0("Overall accuracy: ", round(accuracy * 100, 1), "%. Each column sums to 100%."),
    x = "Actual Category",
    y = "Predicted Category",
    fill = "Share"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

ggsave(
  file.path(plots_dir, "category_prediction_confusion_matrix.png"),
  p_confusion, width = 8, height = 7, dpi = 300
)

# ==============================================================================
# 4. Final message
# ==============================================================================

message("Modeling complete: plots saved to ", plots_dir)