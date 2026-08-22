
source("scripts/00_setup.R")

# ==============================================================================
# 1. Load raw data
# ==============================================================================

raw_path <- file.path(data_dir, "sales.csv")

df_raw <- read_csv(
  raw_path,
  show_col_types = FALSE
)

message("Raw dataset loaded: ", nrow(df_raw), " rows × ", ncol(df_raw), " columns")


# ==============================================================================
# 2. Initial data inspection
# ==============================================================================

glimpse(df_raw)
summary(df_raw)


# ==============================================================================
# 3. Data quality checks
# ==============================================================================

# --- Missing values ------------------------------------------------------------

missing_values <- colSums(is.na(df_raw))

if (any(missing_values > 0)) {
  message("Missing values detected:")
  print(missing_values[missing_values > 0])
} else {
  message("No missing values detected.")
}


# --- Duplicate rows ------------------------------------------------------------

duplicate_rows <- sum(duplicated(df_raw))

message("Duplicate rows: ", duplicate_rows)


# --- Duplicate transaction IDs -------------------------------------------------

duplicate_ids <- sum(duplicated(df_raw$Sale_Id))

if (duplicate_ids > 0) {
  warning("Duplicate Sale_Id values detected: ", duplicate_ids)
} else {
  message("Sale_Id values are unique.")
}


# ==============================================================================
# 4. Remove exact duplicate rows
# ==============================================================================

rows_before <- nrow(df_raw)

df_clean <- df_raw %>%
  distinct()

rows_after <- nrow(df_clean)

rows_removed <- rows_before - rows_after

message("Exact duplicate rows removed: ", rows_removed)


# ==============================================================================
# 5. Convert data types
# ==============================================================================

df_clean <- df_clean %>%
  mutate(
    Date = ymd(Date),

    Category = factor(Category),
    Country = factor(Country),
    City = factor(City),
    Store_Type = factor(Store_Type),
    Gender = factor(Gender),

    Year = as.integer(year(Date)),

    Month = month(
      Date,
      label = TRUE,
      abbr = TRUE
    ),

    Year_Month = floor_date(Date, unit = "month")
  )


# ==============================================================================
# 6. Create derived variables
# ==============================================================================

# Discount amount represents the monetary value discounted
# from the gross sales amount.

df_clean <- df_clean %>%
  mutate(
    Discount_Amount = Sales - Net_Sales
  )


# ==============================================================================
# 7. Validate critical values
# ==============================================================================

# --- Price ---------------------------------------------------------------------

invalid_price <- sum(
  !is.na(df_clean$Price) &
    df_clean$Price <= 0
)

if (invalid_price > 0) {
  warning(
    "Found ", invalid_price,
    " rows with Price <= 0."
  )
}


# --- Quantity ------------------------------------------------------------------

invalid_quantity <- sum(
  !is.na(df_clean$Quantity) &
    df_clean$Quantity <= 0
)

if (invalid_quantity > 0) {
  warning(
    "Found ", invalid_quantity,
    " rows with Quantity <= 0."
  )
}


# --- Discount ------------------------------------------------------------------

invalid_discount <- sum(
  !is.na(df_clean$`Discount_%`) &
    (df_clean$`Discount_%` < 0 | df_clean$`Discount_%` > 100)
)

if (invalid_discount > 0) {
  warning(
    "Found ", invalid_discount,
    " rows with invalid Discount_% values."
  )
}


# --- Rating --------------------------------------------------------------------

invalid_rating <- sum(
  !is.na(df_clean$Rating) &
    (df_clean$Rating < 1 | df_clean$Rating > 5)
)

if (invalid_rating > 0) {
  warning(
    "Found ", invalid_rating,
    " rows with Rating outside the expected 1–5 range."
  )
}


# --- Date ----------------------------------------------------------------------

invalid_dates <- sum(is.na(df_clean$Date))

if (invalid_dates > 0) {
  warning(
    "Found ", invalid_dates,
    " rows with invalid or missing dates."
  )
}


# ==============================================================================
# 8. Validate calculated sales values
# ==============================================================================

# Sales should approximately equal Price × Quantity.
# A small tolerance is allowed because of decimal rounding.

sales_error <- abs(
  df_clean$Sales -
    (df_clean$Price * df_clean$Quantity)
)

max_sales_error <- max(
  sales_error,
  na.rm = TRUE
)

message(
  "Maximum Sales calculation difference: ",
  round(max_sales_error, 4)
)


# Net Sales should approximately equal
# Sales × (1 - Discount_% / 100).

net_sales_expected <-
  df_clean$Sales *
  (1 - df_clean$`Discount_%` / 100)

net_sales_error <- abs(
  df_clean$Net_Sales -
    net_sales_expected
)

max_net_sales_error <- max(
  net_sales_error,
  na.rm = TRUE
)

message(
  "Maximum Net_Sales calculation difference: ",
  round(max_net_sales_error, 4)
)


# ==============================================================================
# 9. Validate Discount_Amount
# ==============================================================================

discount_error <- abs(
  df_clean$Discount_Amount -
    (df_clean$Sales - df_clean$Net_Sales)
)

max_discount_error <- max(
  discount_error,
  na.rm = TRUE
)

if (max_discount_error > 0.01) {
  warning(
    "Discount_Amount does not consistently match Sales - Net_Sales."
  )
}


# ==============================================================================
# 10. Handle missing critical values
# ==============================================================================

# Only remove observations that cannot be used for sales/time analysis.
# Other missing values are retained so they can be investigated during EDA.

critical_missing <- is.na(df_clean$Date) |
  is.na(df_clean$Sales) |
  is.na(df_clean$Net_Sales)

rows_with_missing_critical <- sum(critical_missing)

if (rows_with_missing_critical > 0) {

  message(
    "Rows removed due to missing critical values: ",
    rows_with_missing_critical
  )

  df_clean <- df_clean %>%
    filter(!critical_missing)

} else {

  message("No rows removed for missing critical values.")

}


# ==============================================================================
# 11. Final data validation
# ==============================================================================

message("\n--- Final dataset summary ---")

message(
  "Rows: ",
  nrow(df_clean)
)

message(
  "Columns: ",
  ncol(df_clean)
)

message(
  "Date range: ",
  format(min(df_clean$Date, na.rm = TRUE), "%Y-%m-%d"),
  " → ",
  format(max(df_clean$Date, na.rm = TRUE), "%Y-%m-%d")
)

message(
  "Remaining missing values: ",
  sum(is.na(df_clean))
)

message(
  "Remaining duplicate rows: ",
  sum(duplicated(df_clean))
)

message(
  "Unique Sale_Id values: ",
  n_distinct(df_clean$Sale_Id)
)


# ==============================================================================
# 12. Save cleaned dataset
# ==============================================================================

rds_path <- file.path(data_dir, "clean_sales.rds")
csv_path <- file.path(data_dir, "clean_sales.csv")

saveRDS(
  df_clean,
  rds_path
)

write_csv(
  df_clean,
  csv_path
)


# ==============================================================================
# 13. Completion message
# ==============================================================================

message("\nCleaning complete.")
message("Clean dataset: ", nrow(df_clean), " rows × ", ncol(df_clean), " columns")
message("Saved RDS: ", rds_path)
message("Saved CSV: ", csv_path)