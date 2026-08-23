
source("scripts/00_setup.R")

# ==============================================================================
# 1. Load cleaned data
# ==============================================================================

df <- readRDS(
  file.path(data_dir, "clean_sales.rds")
)

message(
  "Loaded cleaned dataset: ",
  nrow(df),
  " rows × ",
  ncol(df),
  " columns"
)


# ==============================================================================
# 2. Dataset overview
# ==============================================================================

# Basic descriptive statistics for the main numerical variables.

overview_stats <- df %>%
  summarise(
    Transactions = n(),

    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Net_Sales = sum(Net_Sales, na.rm = TRUE),
    Total_Discount = sum(Discount_Amount, na.rm = TRUE),

    Avg_Price = mean(Price, na.rm = TRUE),
    Median_Price = median(Price, na.rm = TRUE),

    Avg_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),

    Avg_Net_Sales = mean(Net_Sales, na.rm = TRUE),
    Median_Net_Sales = median(Net_Sales, na.rm = TRUE),

    Avg_Rating = mean(Rating, na.rm = TRUE)
  )

print(overview_stats)


# Detailed numerical summary
numeric_summary <- df %>%
  select(
    Price,
    Quantity,
    Sales,
    `Discount_%`,
    Discount_Amount,
    Net_Sales,
    Rating
  ) %>%
  summary()

print(numeric_summary)


# ==============================================================================
# 3. Dataset composition
# ==============================================================================

# These summaries help us understand whether the categorical groups are
# reasonably balanced before comparing them.

category_counts <- df %>%
  count(Category, sort = TRUE)

city_counts <- df %>%
  count(City, sort = TRUE)

store_counts <- df %>%
  count(Store_Type, sort = TRUE)

gender_counts <- df %>%
  count(Gender, sort = TRUE)

print(category_counts)
print(city_counts)
print(store_counts)
print(gender_counts)


# ==============================================================================
# 4. Net Sales by Category 
# ==============================================================================

p_category_sales <- category_sales %>%
  ggplot(aes(
    x = reorder(Category, Total_Net_Sales),
    y = Total_Net_Sales
  )) +
  # Geometry
  geom_col(fill = "steelblue") +
  # Coordinates
  coord_flip() +
  # Aesthetics / labels
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Product Category",
    subtitle = "Total revenue after discounts",
    x = "Category",
    y = "Net Sales (AED)"
  ) +
  # Theme
  theme_minimal(base_size = 12)

ggsave(
  file.path(plots_dir, "net_sales_by_category.png"),
  p_category_sales,
  width = 8,
  height = 5,
  dpi = 150
)


# ==============================================================================
# 5. Quantity Sold by Category
# ==============================================================================

category_quantity <- df %>%
  group_by(Category) %>%
  summarise(
    Total_Quantity = sum(Quantity, na.rm = TRUE),
    Avg_Quantity = mean(Quantity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Total_Quantity)

p_category_quantity <- ggplot(
  category_quantity,
  aes(
    x = reorder(Category, Total_Quantity),
    y = Total_Quantity
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Quantity Sold by Product Category",
    x = "Category",
    y = "Quantity (units)"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "quantity_by_category.png"),
  p_category_quantity,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 6. Net Sales by Store Type
# ==============================================================================

store_sales <- df %>%
  group_by(Store_Type) %>%
  summarise(
    Transactions = n(),
    Total_Net_Sales = sum(Net_Sales, na.rm = TRUE),
    Avg_Net_Sales = mean(Net_Sales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Total_Net_Sales)

p_store_sales <- ggplot(
  store_sales,
  aes(
    x = reorder(Store_Type, Total_Net_Sales),
    y = Total_Net_Sales
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Store Type",
    subtitle = "Total revenue after discounts",
    x = "Store Type",
    y = "Net Sales (AED)"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "net_sales_by_store_type.png"),
  p_store_sales,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 7. Net Sales by City 
# ==============================================================================

p_city_sales <- city_sales %>%
  ggplot(aes(
    x = reorder(City, Total_Net_Sales),
    y = Total_Net_Sales
  )) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by City",
    subtitle = "Revenue is fairly evenly distributed across cities",
    x = "City",
    y = "Net Sales (AED)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(plots_dir, "net_sales_by_city.png"),
  p_city_sales,
  width = 8,
  height = 5,
  dpi = 150
)


# ==============================================================================
# 8. Price Range by Category
# ==============================================================================

p_box_price_category <- ggplot(
  df,
  aes(
    x = reorder(Category, Price, median),
    y = Price
  )
) +
  # Geometry: raw points first (so box sits on top), then the box itself
  geom_jitter(
    width = 0.15,
    alpha = 0.15,
    color = "steelblue",
    size = 0.8
  ) +
  geom_boxplot(
    fill = "steelblue",
    alpha = 0.5,
    outlier.shape = NA,
    color = "black",
    width = 0.5
  ) +
  # Coordinates
  coord_flip() +
  # Aesthetics / axis formatting
  scale_y_continuous(
    labels = dollar_format(prefix = "AED ")
  ) +
  labs(
    title = "Price Range by Product Category",
    subtitle = "Box shows median and interquartile range; points show individual transactions",
    x = "Category",
    y = "Price"
  ) +
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank()
  )

ggsave(
  file.path(plots_dir, "box_price_by_category.png"),
  p_box_price_category,
  width = 9,
  height = 6,
  dpi = 300
)


# ==============================================================================
# 9. Correlation Matrix
# ==============================================================================

num_vars <- df %>%
  select(Price, Quantity, Sales, `Discount_%`, Net_Sales, Rating)

corr_matrix <- cor(num_vars, use = "complete.obs")

# Mask upper triangle (including diagonal) with NA
corr_matrix[upper.tri(corr_matrix, diag = TRUE)] <- NA

corr_df <- as.data.frame(as.table(corr_matrix)) %>%
  filter(!is.na(Freq))
names(corr_df) <- c("Var1", "Var2", "Correlation")

p_corr_heatmap <- ggplot(
  corr_df,
  aes(x = Var1, y = Var2, fill = Correlation)
) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Correlation, 2)),
    size = 3.5,
    color = "black"
  ) +
  scale_fill_gradient2(
    low = "firebrick",
    mid = "white",
    high = "steelblue",
    midpoint = 0,
    limits = c(-1, 1),
    na.value = "transparent"
  ) +
  coord_fixed() +
  labs(
    title = "Correlation Between Numeric Variables",
    subtitle = "Pearson correlation coefficient (diagonal and duplicate pairs removed)",
    x = NULL,
    y = NULL,
    fill = "Corr"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

ggsave(
  file.path(plots_dir, "correlation_heatmap.png"),
  p_corr_heatmap,
  width = 7,
  height = 6,
  dpi = 300
)


# ==============================================================================
# 10. Net Sales Distribution
# ==============================================================================

median_net_sales <- median(df$Net_Sales, na.rm = TRUE)

p_hist_net_sales <- ggplot(
  df,
  aes(x = Net_Sales)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept = median_net_sales,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = median_net_sales,
    y = Inf,
    label = paste0("Median: AED ", round(median_net_sales, 0)),
    color = "firebrick",
    vjust = 1.5,
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_log10(
    labels = dollar_format(prefix = "AED ")
  ) +
  labs(
    title = "Distribution of Net Sales per Transaction",
    subtitle = "Revenue after discounts (log scale)",
    x = "Net Sales",
    y = "Number of Transactions"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "distribution_net_sales.png"),
  p_hist_net_sales,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 12. Dataset composition plots
# ==============================================================================

# Category distribution

p_category_count <- ggplot(
  category_counts,
  aes(
    x = reorder(Category, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Number of Transactions by Category",
    x = "Category",
    y = "Transactions"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "transactions_by_category.png"),
  p_category_count,
  width = 8,
  height = 5,
  dpi = 300
)


# Store type distribution

p_store_count <- ggplot(
  store_counts,
  aes(
    x = reorder(Store_Type, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Number of Transactions by Store Type",
    x = "Store Type",
    y = "Transactions"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "transactions_by_store_type.png"),
  p_store_count,
  width = 8,
  height = 5,
  dpi = 300
)


# City distribution

p_city_count <- ggplot(
  city_counts,
  aes(
    x = reorder(City, n),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Number of Transactions by City",
    x = "City",
    y = "Transactions"
  ) +
  theme_minimal()

ggsave(
  file.path(plots_dir, "transactions_by_city.png"),
  p_city_count,
  width = 8,
  height = 5,
  dpi = 300
)


# ==============================================================================
# 13. Final message
# ==============================================================================

message(
  "EDA overview complete: plots saved to ",
  plots_dir
)