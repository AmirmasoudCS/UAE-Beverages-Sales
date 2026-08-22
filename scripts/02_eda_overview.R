
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
  labs(title = "Total Sales by Category", x = NULL, y = "Total Sales") +
  scale_y_continuous(labels = comma)

print(p_category_sales)
ggsave(file.path(plots_dir, "sales_by_category.png"), p_category_sales, width = 8, height = 5)

# --- Quantity by Category -------------------------------------------------
p_category_qty <- df %>%
  group_by(Category) %>%
  summarise(Total_Qty = sum(Quantity)) %>%
  ggplot(aes(x = reorder(Category, Total_Qty), y = Total_Qty)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Total Quantity Sold by Category", x = NULL, y = "Total Quantity") +
  scale_y_continuous(labels = comma)

print(p_category_qty)
ggsave(file.path(plots_dir, "quantity_by_category.png"), p_category_qty, width = 8, height = 5)

# --- Sales by Store_Type -------------------------------------------------
p_store_sales <- df %>%
  group_by(Store_Type) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(Store_Type, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(title = "Total Sales by Store Type", x = NULL, y = "Total Sales") +
  scale_y_continuous(labels = comma)

print(p_store_sales)
ggsave(file.path(plots_dir, "sales_by_store_type.png"), p_store_sales, width = 8, height = 5)

# --- Sales by City / Country ----------------------------------------------
p_city_sales <- df %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(City, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Total Sales by City", x = NULL, y = "Total Sales") +
  scale_y_continuous(labels = comma)

print(p_city_sales)
ggsave(file.path(plots_dir, "sales_by_city.png"), p_city_sales, width = 8, height = 5)

p_country_sales <- df %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(x = reorder(Country, Total_Sales), y = Total_Sales)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(title = "Total Sales by Country", x = NULL, y = "Total Sales") +
  scale_y_continuous(labels = comma)

print(p_country_sales)
ggsave(file.path(plots_dir, "sales_by_country.png"), p_country_sales, width = 8, height = 5)

message("EDA overview complete: plots saved to outputs/plots/")