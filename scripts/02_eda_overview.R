
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
# Revenue vs Quantity Rank 
# ==============================================================================

rank_comparison <- category_sales %>%
  select(Category, Total_Net_Sales) %>%
  left_join(
    category_quantity %>% select(Category, Total_Quantity),
    by = "Category"
  ) %>%
  mutate(
    Revenue_Rank = rank(-Total_Net_Sales),
    Quantity_Rank = rank(-Total_Quantity)
  )

rank_long <- rank_comparison %>%
  select(Category, Revenue_Rank, Quantity_Rank) %>%
  pivot_longer(
    cols = c(Quantity_Rank, Revenue_Rank),
    names_to = "Metric",
    values_to = "Rank"
  ) %>%
  mutate(
    Metric = factor(
      recode(Metric, Quantity_Rank = "Quantity", Revenue_Rank = "Revenue"),
      levels = c("Quantity", "Revenue")
    )
  )

p_rank_slope <- ggplot(
  rank_long,
  aes(x = Metric, y = Rank, group = Category, color = Category)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    data = rank_long %>% filter(Metric == "Quantity"),
    aes(label = Category),
    hjust = 1.1,
    size = 3.5,
    show.legend = FALSE
  ) +
  geom_text(
    data = rank_long %>% filter(Metric == "Revenue"),
    aes(label = Category),
    hjust = -0.1,
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:7) +
  labs(
    title = "Does Category Rank Differ by Revenue vs Quantity Sold?",
    subtitle = "Rank 1 = highest. Upward lines = earns more than volume alone predicts.",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  expand_limits(x = c(0.4, 2.6))

ggsave(
  file.path(plots_dir, "revenue_vs_quantity_rank.png"),
  p_rank_slope,
  width = 9,
  height = 7,
  dpi = 150
)

# Companion table
rank_table <- rank_comparison %>%
  mutate(Rank_Change = Quantity_Rank - Revenue_Rank) %>%
  arrange(desc(Rank_Change)) %>%
  select(Category, Quantity_Rank, Revenue_Rank, Rank_Change)

print(rank_table)


# ==============================================================================
# 6. Net Sales by Store Type 
# ==============================================================================

store_stats <- store_sales %>%
  summarise(
    Mean_Sales = mean(Total_Net_Sales),
    Min_Sales = min(Total_Net_Sales),
    Max_Sales = max(Total_Net_Sales),
    CV_Pct = sd(Total_Net_Sales) / mean(Total_Net_Sales) * 100
  )

store_spread_label <- paste0(
  "Range: AED ", comma(round(store_stats$Min_Sales, 0)),
  " – ", comma(round(store_stats$Max_Sales, 0)),
  "  (", round(store_stats$CV_Pct, 1), "% variation)"
)

p_store_sales <- store_sales %>%
  ggplot(aes(
    x = reorder(Store_Type, Total_Net_Sales),
    y = Total_Net_Sales
  )) +
  geom_col(fill = "steelblue") +
  geom_hline(
    yintercept = store_stats$Mean_Sales,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.6
  ) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Store Type",
    subtitle = paste0("Store types perform almost identically — ", store_spread_label),
    x = "Store Type",
    y = "Net Sales (AED)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(plots_dir, "net_sales_by_store_type.png"),
  p_store_sales,
  width = 9,
  height = 5,
  dpi = 150
)


# ==============================================================================
# 7. Net Sales by City 
# ==============================================================================

city_stats <- city_sales %>%
  summarise(
    Mean_Sales = mean(Total_Net_Sales),
    Min_Sales = min(Total_Net_Sales),
    Max_Sales = max(Total_Net_Sales),
    CV_Pct = sd(Total_Net_Sales) / mean(Total_Net_Sales) * 100
  )

spread_label <- paste0(
  "Range: AED ", comma(round(city_stats$Min_Sales, 0)),
  " – ", comma(round(city_stats$Max_Sales, 0)),
  "  (", round(city_stats$CV_Pct, 1), "% variation)"
)

p_city_sales <- city_sales %>%
  ggplot(aes(
    x = reorder(City, Total_Net_Sales),
    y = Total_Net_Sales
  )) +
  # Geometry
  geom_col(fill = "steelblue") +
  # Statistics: mean reference line across all cities
  geom_hline(
    yintercept = city_stats$Mean_Sales,
    linetype = "dashed",
    color = "firebrick",
    linewidth = 0.6
  ) +
  # Coordinates: start axis at 0 so bar lengths aren't visually misleading,
  # but zoom the label region so the annotation is legible
  coord_flip() +
  # Aesthetics / labels
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by City",
    subtitle = paste0("Cities perform almost identically — ", spread_label),
    x = "City",
    y = "Net Sales (AED)"
  ) +
  # Theme
  theme_minimal(base_size = 12)

ggsave(
  file.path(plots_dir, "net_sales_by_city.png"),
  p_city_sales,
  width = 9,
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
# 11b. Net Sales Trend by Category — smoothed trends only, overlaid
# ==============================================================================

monthly_category_sales <- df %>%
  group_by(Category, Year_Month) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

p_trend_by_category <- ggplot(
  monthly_category_sales,
  aes(x = Year_Month, y = Total_Net_Sales, color = Category)
) +
  # Statistics only — smoothed trend, no raw noisy points
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.9
  ) +
  # Coordinates
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  # Aesthetics / labels
  labs(
    title = "Revenue Trend by Category Over Time",
    subtitle = "Smoothed trends only — checking whether any category is growing or declining",
    x = NULL,
    y = "Net Sales (AED)",
    color = "Category"
  ) +
  # Theme
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

ggsave(
  file.path(plots_dir, "revenue_trend_by_category.png"),
  p_trend_by_category,
  width = 10,
  height = 6,
  dpi = 150
)

ggsave(
  file.path(plots_dir, "net_sales_trend_monthly.png"),
  p_trend_monthly,
  width = 11,
  height = 6,
  dpi = 150
)


# ==============================================================================
# 12b. Seasonality by Category — top 3 months per category highlighted
# ==============================================================================

seasonal_category <- df %>%
  group_by(Category, Month) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

category_month_means <- seasonal_category %>%
  group_by(Category) %>%
  summarise(
    Mean_Sales = mean(Total_Net_Sales),
    Category_Total = sum(Total_Net_Sales),
    .groups = "drop"
  )

seasonal_category <- seasonal_category %>%
  left_join(category_month_means, by = "Category") %>%
  group_by(Category) %>%
  mutate(Is_Top3 = rank(-Total_Net_Sales) <= 3) %>%
  ungroup() %>%
  mutate(Category = fct_reorder(Category, -Category_Total))

p_seasonality_by_category <- ggplot(
  seasonal_category,
  aes(x = Month, y = Total_Net_Sales, fill = Is_Top3)
) +
  geom_col() +
  geom_hline(
    aes(yintercept = Mean_Sales),
    linetype = "dashed",
    color = "grey30",
    linewidth = 0.5
  ) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_fill_manual(
    values = c(`TRUE` = "#e07b39", `FALSE` = "#a8c5dc"),
    labels = c(`TRUE` = "Top 3 months", `FALSE` = "Other months"),
    name = NULL
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Month, Faceted by Category",
    subtitle = "Each category's own top 3 months highlighted — peak timing differs by category",
    x = NULL,
    y = "Net Sales (AED)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
    strip.text = element_text(face = "bold", size = 9.5),
    panel.spacing = unit(1.1, "lines"),
    legend.position = "top",
    legend.margin = margin(b = 5)
  )

ggsave(
  file.path(plots_dir, "seasonality_by_category.png"),
  p_seasonality_by_category,
  width = 12,
  height = 8,
  dpi = 150
)


# ==============================================================================
# 13. Final message
# ==============================================================================

message(
  "EDA overview complete: plots saved to ",
  plots_dir
)