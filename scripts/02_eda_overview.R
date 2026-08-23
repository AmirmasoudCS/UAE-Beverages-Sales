source("scripts/00_setup.R")

# ==============================================================================
# 1. Load cleaned data
# ==============================================================================

df <- readRDS(file.path(data_dir, "clean_sales.rds"))

message(
  "Loaded cleaned dataset: ", nrow(df), " rows x ", ncol(df), " columns"
)

# ==============================================================================
# 2. Shared theme
# ==============================================================================

# One consistent theme for every chart in this script. Applied via theme_set()
# in 00_setup.R already, but redefined here so this script is self-contained.

theme_report <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", size = 10.5),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

theme_set(theme_report)

# Consistent color palette used across the whole script
color_main <- "steelblue"
color_accent <- "#e07b39"

# ==============================================================================
# 3. Dataset overview
# ==============================================================================

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

numeric_summary <- df %>%
  select(Price, Quantity, Sales, `Discount_%`, Discount_Amount, Net_Sales, Rating) %>%
  summary()

print(numeric_summary)

# ==============================================================================
# 4. Dataset composition (reference only, not plotted, see conversation notes
#    on why category/city/store transaction counts were cut from the deck)
# ==============================================================================

category_counts <- df %>% count(Category, sort = TRUE)
city_counts <- df %>% count(City, sort = TRUE)
store_counts <- df %>% count(Store_Type, sort = TRUE)
gender_counts <- df %>% count(Gender, sort = TRUE)

print(category_counts)
print(city_counts)
print(store_counts)
print(gender_counts)

# ==============================================================================
# 5. Aggregations used by multiple charts below
# ==============================================================================

category_sales <- df %>%
  group_by(Category) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

category_quantity <- df %>%
  group_by(Category) %>%
  summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = "drop")

store_sales <- df %>%
  group_by(Store_Type) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

city_sales <- df %>%
  group_by(City) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

# ==============================================================================
# 6. Net Sales by Category
# ==============================================================================

p_category_sales <- category_sales %>%
  ggplot(aes(x = reorder(Category, Total_Net_Sales), y = Total_Net_Sales)) +
  geom_col(fill = color_main) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Product Category",
    subtitle = "Total revenue after discounts",
    x = "Category",
    y = "Net Sales (AED)"
  )

ggsave(
  file.path(plots_dir, "net_sales_by_category.png"),
  p_category_sales, width = 8, height = 5, dpi = 300
)

# ==============================================================================
# 7. Revenue vs Quantity Rank (slope chart)
# ==============================================================================

rank_comparison <- category_sales %>%
  select(Category, Total_Net_Sales) %>%
  left_join(category_quantity %>% select(Category, Total_Quantity), by = "Category") %>%
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
    aes(label = Category), hjust = 1.1, size = 3.5, show.legend = FALSE
  ) +
  geom_text(
    data = rank_long %>% filter(Metric == "Revenue"),
    aes(label = Category), hjust = -0.1, size = 3.5, show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:7) +
  labs(
    title = "Does Category Rank Differ by Revenue vs Quantity Sold?",
    subtitle = "Rank 1 is highest. Upward lines mean a category earns more than its volume alone predicts.",
    x = NULL,
    y = "Rank"
  ) +
  theme(legend.position = "none") +
  expand_limits(x = c(0.4, 2.6))

ggsave(
  file.path(plots_dir, "revenue_vs_quantity_rank.png"),
  p_rank_slope, width = 9, height = 7, dpi = 300
)

rank_table <- rank_comparison %>%
  mutate(Rank_Change = Quantity_Rank - Revenue_Rank) %>%
  arrange(desc(Rank_Change)) %>%
  select(Category, Quantity_Rank, Revenue_Rank, Rank_Change)

print(rank_table)

# ==============================================================================
# 8. Net Sales by Store Type
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
  " to ", comma(round(store_stats$Max_Sales, 0)),
  " (", round(store_stats$CV_Pct, 1), "% variation)"
)

p_store_sales <- store_sales %>%
  ggplot(aes(x = reorder(Store_Type, Total_Net_Sales), y = Total_Net_Sales)) +
  geom_col(fill = color_main) +
  geom_hline(
    yintercept = store_stats$Mean_Sales,
    linetype = "dashed", color = color_accent, linewidth = 0.6
  ) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Store Type",
    subtitle = paste0("Store types perform almost identically. ", store_spread_label),
    x = "Store Type",
    y = "Net Sales (AED)"
  )

ggsave(
  file.path(plots_dir, "net_sales_by_store_type.png"),
  p_store_sales, width = 9, height = 5, dpi = 300
)

# ==============================================================================
# 9. Net Sales by City
# ==============================================================================

city_stats <- city_sales %>%
  summarise(
    Mean_Sales = mean(Total_Net_Sales),
    Min_Sales = min(Total_Net_Sales),
    Max_Sales = max(Total_Net_Sales),
    CV_Pct = sd(Total_Net_Sales) / mean(Total_Net_Sales) * 100
  )

city_spread_label <- paste0(
  "Range: AED ", comma(round(city_stats$Min_Sales, 0)),
  " to ", comma(round(city_stats$Max_Sales, 0)),
  " (", round(city_stats$CV_Pct, 1), "% variation)"
)

p_city_sales <- city_sales %>%
  ggplot(aes(x = reorder(City, Total_Net_Sales), y = Total_Net_Sales)) +
  geom_col(fill = color_main) +
  geom_hline(
    yintercept = city_stats$Mean_Sales,
    linetype = "dashed", color = color_accent, linewidth = 0.6
  ) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by City",
    subtitle = paste0("Cities perform almost identically. ", city_spread_label),
    x = "City",
    y = "Net Sales (AED)"
  )

ggsave(
  file.path(plots_dir, "net_sales_by_city.png"),
  p_city_sales, width = 9, height = 5, dpi = 300
)

# ==============================================================================
# 10. Price Range by Category
# ==============================================================================

p_box_price_category <- ggplot(df, aes(x = reorder(Category, Price, median), y = Price)) +
  geom_jitter(width = 0.15, alpha = 0.15, color = color_main, size = 0.8) +
  geom_boxplot(
    fill = color_main, alpha = 0.5, outlier.shape = NA, color = "black", width = 0.5
  ) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "AED ")) +
  labs(
    title = "Price Range by Product Category",
    subtitle = "Box shows median and interquartile range; points show individual transactions",
    x = "Category",
    y = "Price"
  ) +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(plots_dir, "box_price_by_category.png"),
  p_box_price_category, width = 9, height = 6, dpi = 300
)

# ==============================================================================
# 11. Correlation Matrix
# ==============================================================================

num_vars <- df %>%
  select(Price, Quantity, Sales, `Discount_%`, Net_Sales, Rating)

corr_matrix <- cor(num_vars, use = "complete.obs")
corr_matrix[upper.tri(corr_matrix, diag = TRUE)] <- NA

corr_df <- as.data.frame(as.table(corr_matrix)) %>%
  filter(!is.na(Freq))
names(corr_df) <- c("Var1", "Var2", "Correlation")

p_corr_heatmap <- ggplot(corr_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3.5, color = "black") +
  scale_fill_gradient2(
    low = "firebrick", mid = "white", high = color_main,
    midpoint = 0, limits = c(-1, 1), na.value = "transparent"
  ) +
  coord_fixed() +
  labs(
    title = "Correlation Between Numeric Variables",
    subtitle = "Pearson correlation coefficient (diagonal and duplicate pairs removed)",
    x = NULL,
    y = NULL,
    fill = "Corr"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right"
  )

ggsave(
  file.path(plots_dir, "correlation_heatmap.png"),
  p_corr_heatmap, width = 7, height = 6, dpi = 300
)

# ==============================================================================
# 12. Net Sales Distribution
# ==============================================================================

median_net_sales <- median(df$Net_Sales, na.rm = TRUE)

p_hist_net_sales <- ggplot(df, aes(x = Net_Sales)) +
  geom_histogram(bins = 30, fill = color_main, color = "white") +
  geom_vline(
    xintercept = median_net_sales,
    linetype = "dashed", color = color_accent, linewidth = 0.7
  ) +
  annotate(
    "text", x = median_net_sales, y = Inf,
    label = paste0("Median: AED ", round(median_net_sales, 0)),
    color = color_accent, vjust = 1.5, hjust = -0.1, size = 3.5
  ) +
  scale_x_log10(labels = dollar_format(prefix = "AED ")) +
  labs(
    title = "Distribution of Net Sales per Transaction",
    subtitle = "Revenue after discounts, log scale",
    x = "Net Sales",
    y = "Number of Transactions"
  )

ggsave(
  file.path(plots_dir, "distribution_net_sales.png"),
  p_hist_net_sales, width = 8, height = 5, dpi = 300
)

# ==============================================================================
# 13. Net Sales Trend Over Time (overall)
# ==============================================================================

monthly_sales <- df %>%
  group_by(Year_Month) %>%
  summarise(
    Total_Net_Sales = sum(Net_Sales, na.rm = TRUE),
    Transactions = n(),
    .groups = "drop"
  )

p_trend_monthly <- ggplot(monthly_sales, aes(x = Year_Month, y = Total_Net_Sales)) +
  geom_line(color = color_main, linewidth = 0.8) +
  geom_point(color = color_main, size = 1.5) +
  geom_smooth(
    method = "loess", se = FALSE,
    color = color_accent, linewidth = 0.8, linetype = "dashed"
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales Trend Over Time",
    subtitle = "Monthly revenue is volatile but shows no long-term growth or decline (loess trend)",
    x = NULL,
    y = "Net Sales (AED)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(plots_dir, "net_sales_trend_monthly.png"),
  p_trend_monthly, width = 11, height = 6, dpi = 300
)

# ==============================================================================
# 14. Net Sales Trend by Category (smoothed trends only, overlaid)
# ==============================================================================

monthly_category_sales <- df %>%
  group_by(Category, Year_Month) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop")

p_trend_by_category <- ggplot(
  monthly_category_sales,
  aes(x = Year_Month, y = Total_Net_Sales, color = Category)
) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.9) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Revenue Trend by Category Over Time",
    subtitle = "Smoothed trends only, checking whether any category is growing or declining",
    x = NULL,
    y = "Net Sales (AED)",
    color = "Category"
  ) +
  theme(legend.position = "right")

ggsave(
  file.path(plots_dir, "revenue_trend_by_category.png"),
  p_trend_by_category, width = 10, height = 6, dpi = 300
)

# ==============================================================================
# 15. Seasonality by Category (each category's own top 3 months highlighted)
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
    linetype = "dashed", color = "grey30", linewidth = 0.5
  ) +
  facet_wrap(~ Category, scales = "free_y") +
  scale_fill_manual(
    values = c(`TRUE` = color_accent, `FALSE` = "#a8c5dc"),
    labels = c(`TRUE` = "Top 3 months", `FALSE` = "Other months"),
    name = NULL
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Net Sales by Month, Faceted by Category",
    subtitle = "Each category's own top 3 months highlighted; peak timing differs by category",
    x = NULL,
    y = "Net Sales (AED)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7.5),
    strip.text = element_text(face = "bold", size = 9.5),
    panel.spacing = unit(1.1, "lines"),
    legend.margin = margin(b = 5)
  )

ggsave(
  file.path(plots_dir, "seasonality_by_category.png"),
  p_seasonality_by_category, width = 12, height = 8, dpi = 300
)

# ==============================================================================
# 16. Net Sales by Gender
# ==============================================================================

gender_stats <- df %>%
  group_by(Gender) %>%
  summarise(
    Median_Net_Sales = median(Net_Sales, na.rm = TRUE),
    .groups = "drop"
  )

p_box_sales_gender <- ggplot(df, aes(x = Gender, y = Net_Sales)) +
  geom_jitter(width = 0.2, alpha = 0.08, color = color_main, size = 0.6) +
  geom_boxplot(
    fill = color_main, alpha = 0.5, outlier.shape = NA, color = "black", width = 0.4
  ) +
  scale_y_log10(labels = dollar_format(prefix = "AED ")) +
  labs(
    title = "Net Sales per Transaction by Gender",
    subtitle = "Log scale. Checking whether transaction value differs by gender",
    x = "Gender",
    y = "Net Sales"
  )

ggsave(
  file.path(plots_dir, "net_sales_by_gender.png"),
  p_box_sales_gender, width = 7, height = 5, dpi = 300
)

print(gender_stats)

# ==============================================================================
# 17. Category Mix by Gender
# ==============================================================================

# Proportion (not raw count) so the two genders are compared on equal footing
# even if their overall transaction counts differ.

category_gender <- df %>%
  count(Gender, Category) %>%
  group_by(Gender) %>%
  mutate(Share = n / sum(n)) %>%
  ungroup()

category_order_gender <- category_gender %>%
  group_by(Category) %>%
  summarise(Total = sum(n), .groups = "drop") %>%
  arrange(Total) %>%
  pull(Category)

category_gender <- category_gender %>%
  mutate(Category = factor(Category, levels = category_order_gender))

p_category_by_gender <- ggplot(
  category_gender,
  aes(x = Category, y = Share, fill = Gender)
) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(color_main, color_accent)) +
  labs(
    title = "Product Category Mix by Gender",
    subtitle = "Share of each gender's transactions going to each category",
    x = "Category",
    y = "Share of Transactions"
  )

ggsave(
  file.path(plots_dir, "category_mix_by_gender.png"),
  p_category_by_gender, width = 9, height = 6, dpi = 300
)

# ==============================================================================
# 18. Final message
# ==============================================================================

message("EDA overview complete: plots saved to ", plots_dir)