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
color_flag <- "firebrick"

# ==============================================================================
# 2. Time-based anomalies: which months did the business over- or under-perform
#    relative to what the ARIMA model expected?
# ==============================================================================

monthly_sales <- df %>%
  group_by(Year_Month) %>%
  summarise(Total_Net_Sales = sum(Net_Sales, na.rm = TRUE), .groups = "drop") %>%
  arrange(Year_Month)

sales_ts <- ts(
  monthly_sales$Total_Net_Sales,
  start = c(year(min(monthly_sales$Year_Month)), month(min(monthly_sales$Year_Month))),
  frequency = 12
)

arima_model <- auto.arima(sales_ts)

# Residuals: actual minus what the model expected for that same month,
# using one-step-ahead fitted values so each month is judged against a
# genuine prediction, not information from the future.
monthly_sales <- monthly_sales %>%
  mutate(
    Fitted = as.numeric(fitted(arima_model)),
    Residual = Total_Net_Sales - Fitted,
    Residual_Z = as.numeric(scale(Residual))
  )

anomaly_threshold <- 2

monthly_sales <- monthly_sales %>%
  mutate(Is_Anomaly = abs(Residual_Z) > anomaly_threshold)

anomalous_months <- monthly_sales %>%
  filter(Is_Anomaly) %>%
  select(Year_Month, Total_Net_Sales, Fitted, Residual, Residual_Z) %>%
  arrange(desc(abs(Residual_Z)))

message(nrow(anomalous_months), " anomalous months found (|z| > ", anomaly_threshold, ")")
print(anomalous_months)

p_time_anomalies <- ggplot(monthly_sales, aes(x = Year_Month, y = Total_Net_Sales)) +
  geom_line(color = "grey60", linewidth = 0.6) +
  geom_point(
    aes(color = Is_Anomaly, size = Is_Anomaly),
    alpha = 0.9
  ) +
  scale_color_manual(
    values = c(`TRUE` = color_flag, `FALSE` = color_main),
    labels = c(`TRUE` = "Anomalous month", `FALSE` = "Normal month"),
    name = NULL
  ) +
  scale_size_manual(values = c(`TRUE` = 3, `FALSE` = 1.3), guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Anomalous Months in Net Sales",
    subtitle = paste0(
      "Months where actual revenue differed from the ARIMA forecast by more than ",
      anomaly_threshold, " standard deviations"
    ),
    x = NULL,
    y = "Net Sales (AED)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  file.path(plots_dir, "time_anomalies.png"),
  p_time_anomalies, width = 11, height = 6, dpi = 300
)

# ==============================================================================
# 3. Transaction-level anomalies: outliers within each category's own price
#    and revenue range, using the standard 1.5x IQR rule.
# ==============================================================================

flag_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

transaction_outliers <- df %>%
  group_by(Category) %>%
  mutate(Is_Outlier = flag_outliers(Net_Sales)) %>%
  ungroup()

outlier_summary <- transaction_outliers %>%
  group_by(Category) %>%
  summarise(
    Transactions = n(),
    Outliers = sum(Is_Outlier),
    Outlier_Share = Outliers / Transactions,
    .groups = "drop"
  ) %>%
  arrange(desc(Outlier_Share))

print(outlier_summary)

p_outlier_share <- ggplot(
  outlier_summary,
  aes(x = reorder(Category, Outlier_Share), y = Outlier_Share)
) +
  geom_col(fill = color_main) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Share of Outlier Transactions by Category",
    subtitle = "Outliers defined per category using the 1.5x IQR rule on Net Sales",
    x = "Category",
    y = "Share of Transactions Flagged as Outliers"
  )

ggsave(
  file.path(plots_dir, "outlier_share_by_category.png"),
  p_outlier_share, width = 8, height = 5, dpi = 300
)

p_outlier_scatter <- ggplot(
  transaction_outliers,
  aes(x = reorder(Category, Net_Sales, median), y = Net_Sales, color = Is_Outlier)
) +
  geom_jitter(
    data = transaction_outliers %>% filter(!Is_Outlier),
    width = 0.2, alpha = 0.1, size = 0.6
  ) +
  geom_jitter(
    data = transaction_outliers %>% filter(Is_Outlier),
    width = 0.2, alpha = 0.6, size = 1
  ) +
  scale_color_manual(
    values = c(`TRUE` = color_flag, `FALSE` = color_main),
    labels = c(`TRUE` = "Outlier", `FALSE` = "Normal"),
    name = NULL
  ) +
  scale_y_log10(labels = dollar_format(prefix = "AED ")) +
  coord_flip() +
  labs(
    title = "Transaction-Level Outliers by Category",
    subtitle = "Log scale. Each category's outliers are defined relative to its own range",
    x = "Category",
    y = "Net Sales"
  )

ggsave(
  file.path(plots_dir, "outlier_transactions_by_category.png"),
  p_outlier_scatter, width = 9, height = 6, dpi = 300
)

# ==============================================================================
# 4. Final message
# ==============================================================================

message("Anomaly detection complete: plots saved to ", plots_dir)