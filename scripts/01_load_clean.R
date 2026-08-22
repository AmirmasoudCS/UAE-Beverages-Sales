
source("scripts/00_setup.R")

# --- Load raw data ---------------------------------------------------
raw_path <- file.path(data_dir, "sales.csv")
df <- read_csv(raw_path, show_col_types = FALSE)

# --- Inspect structure -------------------------------------------------
glimpse(df)
summary(df)

# --- Check missing values & duplicates ----------------------------
colSums(is.na(df))
sum(duplicated(df))

# --- Clean & transform -------------------------------------------------
df_clean <- df %>%
  distinct() %>%
  mutate(
    Date = ymd(Date),
    Category = as.factor(Category),
    Country = as.factor(Country),
    City = as.factor(City),
    Store_Type = as.factor(Store_Type),
    Gender = as.factor(Gender),
    Discount_Amount = Sales - Net_Sales,
    Year = year(Date),
    Month = month(Date, label = TRUE),
    Year_Month = floor_date(Date, "month")
  )

# --- Drop rows with missing critical values -----
df_clean <- df_clean %>%
  filter(!is.na(Date), !is.na(Sales), !is.na(Net_Sales))

# --- Save cleaned data -------------------------------------------------
saveRDS(df_clean, file.path(data_dir, "clean_sales.rds"))
write_csv(df_clean, file.path(data_dir, "clean_sales.csv"))

message("Cleaning complete: ", nrow(df_clean), " rows saved to data/clean_sales.rds")