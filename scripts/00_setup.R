
# --- Packages -----------------------------------------------------
required_packages <- c(
  "tidyverse",      # dplyr, ggplot2, tidyr, readr, etc.
  "lubridate",      # date handling
  "scales",         # axis/label formatting (currency, percent, etc.)
  "plotly",         # interactive plots
  "shiny"           # build the dashboard
)

installed <- rownames(installed.packages())
missing_pkgs <- setdiff(required_packages, installed)

if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# --- Global options -------------------------------------------------
options(
  scipen = 999,      # avoid scientific notation in plots/summaries
  stringsAsFactors = FALSE
)

# --- Global paths -----------------------------------------------------
data_dir    <- "data"
output_dir  <- "outputs"
plots_dir   <- file.path(output_dir, "plots")

dir.create(data_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir,  showWarnings = FALSE, recursive = TRUE)

# --- Global ggplot theme --------------------------------------------
theme_sales <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 2),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

theme_set(theme_sales())

message("Setup complete: packages loaded, options set, theme applied.")