
source("scripts/00_setup.R")
df <- readRDS(file.path(data_dir, "clean_sales.rds"))

# --- Summary statistics -------------------------------------------------
df %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Net_Sales = sum(Net_Sales),
    Avg_Price = mean(Price),
    Avg_Quantity = mean(Quantity),
    Avg_Rating = mean(Rating),
    Avg_Discount_Pct = mean(`Discount_%`)
  )

df %>%
  select(Price, Quantity, Sales, Net_Sales, `Discount_%`, Rating) %>%
  summary()

# --- Sales by Category ---------------------------------------------------
p_category_sales <- df %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(Category, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Sales by Category", x = "Category", y = "Total Sales (AED)") +
  scale_y_continuous(labels = comma)
ggsave(file.path(plots_dir, "sales_by_category.png"), p_category_sales, width = 8, height = 5)

# --- Quantity by Category -------------------------------------------------
p_category_qty <- df %>%
  group_by(Category) %>%
  summarise(Total_Qty = sum(Quantity)) %>%
  ggplot(aes(x = reorder(Category, Total_Qty), y = Total_Qty)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Total Quantity Sold by Category", x = "Category", y = "Total Quantity (units)") +
  scale_y_continuous(labels = comma)
ggsave(file.path(plots_dir, "quantity_by_category.png"), p_category_qty, width = 8, height = 5)

# --- Sales by Store_Type -------------------------------------------------
p_store_sales <- df %>%
  group_by(Store_Type) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(Store_Type, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(title = "Total Sales by Store Type", x = "Store Type", y = "Total Sales (AED)") +
  scale_y_continuous(labels = comma)
ggsave(file.path(plots_dir, "sales_by_store_type.png"), p_store_sales, width = 8, height = 5)

# --- Sales by City ----------------------------------------------
p_city_sales <- df %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(City, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Total Sales by City", x = "City", y = "Total Sales (AED)") +
  scale_y_continuous(labels = comma)
ggsave(file.path(plots_dir, "sales_by_city.png"), p_city_sales, width = 8, height = 5)

# --- Histograms: distribution of key numeric variables ---------------
p_hist_price <- ggplot(df, aes(x = Price)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Price", x = "Price (AED)", y = "Count")
ggsave(file.path(plots_dir, "hist_price.png"), p_hist_price, width = 8, height = 5)

p_hist_qty <- ggplot(df, aes(x = Quantity)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Quantity", x = "Quantity (units)", y = "Count")
ggsave(file.path(plots_dir, "hist_quantity.png"), p_hist_qty, width = 8, height = 5)

p_hist_sales <- ggplot(df, aes(x = Sales)) +
  geom_histogram(bins = 30, fill = "seagreen", color = "white") +
  labs(title = "Distribution of Sales", x = "Sales (AED)", y = "Count")
ggsave(file.path(plots_dir, "hist_sales.png"), p_hist_sales, width = 8, height = 5)

p_hist_rating <- ggplot(df, aes(x = Rating)) +
  geom_histogram(bins = 20, fill = "purple", color = "white") +
  labs(title = "Distribution of Rating", x = "Rating (out of 5)", y = "Count")
ggsave(file.path(plots_dir, "hist_rating.png"), p_hist_rating, width = 8, height = 5)

p_hist_discount <- ggplot(df, aes(x = `Discount_%`)) +
  geom_histogram(bins = 20, fill = "firebrick", color = "white") +
  labs(title = "Distribution of Discount %", x = "Discount (%)", y = "Count")
ggsave(file.path(plots_dir, "hist_discount_pct.png"), p_hist_discount, width = 8, height = 5)

# --- Boxplots: numeric variable spread across groups -----------------
p_box_price_category <- ggplot(df, aes(x = reorder(Category, Price, median), y = Price)) +
  geom_boxplot(fill = "steelblue", outlier.alpha = 0.4) +
  coord_flip() +
  labs(title = "Price Distribution by Category", x = "Category", y = "Price (AED)")
ggsave(file.path(plots_dir, "box_price_by_category.png"), p_box_price_category, width = 8, height = 6)

p_box_sales_store <- ggplot(df, aes(x = Store_Type, y = Sales)) +
  geom_boxplot(fill = "seagreen", outlier.alpha = 0.4) +
  labs(title = "Sales Distribution by Store Type", x = "Store Type", y = "Sales (AED)")
ggsave(file.path(plots_dir, "box_sales_by_store_type.png"), p_box_sales_store, width = 8, height = 5)

p_box_rating_gender <- ggplot(df, aes(x = Gender, y = Rating)) +
  geom_boxplot(fill = "darkorange", outlier.alpha = 0.4) +
  labs(title = "Rating Distribution by Gender", x = "Gender", y = "Rating (out of 5)")
ggsave(file.path(plots_dir, "box_rating_by_gender.png"), p_box_rating_gender, width = 6, height = 5)

# --- Scatter plots: relationships between numeric variables -----------------
p_scatter_price_qty <- ggplot(df, aes(x = Price, y = Quantity, color = Category)) +
  geom_point(alpha = 0.6) +
  labs(title = "Price vs Quantity", x = "Price (AED)", y = "Quantity (units)")
ggsave(file.path(plots_dir, "scatter_price_vs_quantity.png"), p_scatter_price_qty, width = 9, height = 6)

p_scatter_discount_qty <- ggplot(df, aes(x = `Discount_%`, y = Quantity)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(title = "Discount % vs Quantity", x = "Discount (%)", y = "Quantity (units)")
ggsave(file.path(plots_dir, "scatter_discount_vs_quantity.png"), p_scatter_discount_qty, width = 8, height = 5)

p_scatter_price_rating <- ggplot(df, aes(x = Price, y = Rating)) +
  geom_point(alpha = 0.5, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  labs(title = "Price vs Rating", x = "Price (AED)", y = "Rating (out of 5)")
ggsave(file.path(plots_dir, "scatter_price_vs_rating.png"), p_scatter_price_rating, width = 8, height = 5)

# --- Correlation heatmap: numeric variables -----------------
num_vars <- df %>%
  select(Price, Quantity, Sales, `Discount_%`, Net_Sales, Rating)

corr_matrix <- cor(num_vars, use = "complete.obs")
corr_df <- as.data.frame(as.table(corr_matrix))

p_corr_heatmap <- ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3.5) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Heatmap of Numeric Variables", x = NULL, y = NULL, fill = "Corr") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(plots_dir, "correlation_heatmap.png"), p_corr_heatmap, width = 7, height = 6)

message("EDA overview complete: plots saved to outputs/plots/")