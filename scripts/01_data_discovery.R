# ==========================================================
# Script: 01_data_discovery.R
# Purpose: Initial exploration of raw screening datasets 
#          prior to cleaning and harmonisation.
# Author: Nikolaos Samperis
# Date: 20/02/2026
# ==========================================================

# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------
# tidyverse: data manipulation and iteration
# stringr:   numeric-aware sorting of file names

library(tidyverse)
library(stringr)

# ----------------------------------------------------------
# 2. Identify and order raw CSV files
# ----------------------------------------------------------
# Assumes all raw datasets are stored in a folder named 
# "raw_data". Files are sorted numerically to preserve the 
# intended dataset order (e.g., DN01, DN02, ...).

files <- list.files("raw_data",
                    pattern = "\\.csv$",
                    full.names = TRUE) |>
  str_sort(numeric = TRUE)

# ----------------------------------------------------------
# 3. Import all datasets into a named list
# ----------------------------------------------------------
# Each dataset is read as a tibble.
# Names are assigned using the file names for traceability.

datasets <- set_names(files, basename(files)) |>
  map(~ read_csv(.x, show_col_types = FALSE))

# ----------------------------------------------------------
# 4. Structural inspection of each dataset
# ----------------------------------------------------------
# Displays variable names, types, and preview of values.
# Used to detect inconsistencies in structure across files.

walk2(datasets, names(datasets), ~ {
  cat("\n\n=========================\n")
  cat("Dataset:", .y, "\n")
  cat("=========================\n")
  glimpse(.x)
})

# ----------------------------------------------------------
# 5. Check variable names across datasets
# ----------------------------------------------------------
# Used to identify naming inconsistencies prior to cleaning.

map(datasets, names)

# ----------------------------------------------------------
# 6. Check variable data types
# ----------------------------------------------------------
# Identifies potential inconsistencies (e.g., numeric vs character)
# across datasets that may require harmonisation.

map_dfr(datasets,
        ~ summarise_all(.x, class),
        .id = "dataset")

# ----------------------------------------------------------
# 7. Dataset-level dimensions and missingness
# ----------------------------------------------------------
# For each dataset:
#   - number of rows
#   - number of columns
#   - total number of missing values
#   - percentage of missing values
# This provides an overview of completeness by file.

map_dfr(datasets, ~ tibble(
  rows = nrow(.x),
  cols = ncol(.x),
  total_missing = sum(is.na(.x)),
  missing_pct = mean(is.na(as.matrix(.x))) * 100
), .id = "dataset")

# ----------------------------------------------------------
# 8. Missing values per variable within each dataset
# ----------------------------------------------------------
# Summarises number of missing values per column for each dataset.
# Used to detect systematic missingness patterns.

map_dfr(datasets, ~
          summarise(.x,
                    across(everything(),
                           ~ sum(is.na(.)))),
        .id = "dataset")

# ----------------------------------------------------------
# 9. Identify duplicated rows within each dataset
# ----------------------------------------------------------
# Flags fully duplicated records (forward or backward).
# No removal is performed at this stage — this is exploratory.

walk2(datasets, names(datasets), ~ {
  
  dup_rows <- .x[duplicated(.x) | duplicated(.x, fromLast = TRUE), ]
  
  if(nrow(dup_rows) > 0){
    cat("\nDuplicated rows in:", .y, "\n")
    print(dup_rows)
  }
})

# ----------------------------------------------------------
# 10. Summary statistics for numeric variables
# ----------------------------------------------------------
# Calculates min, median, mean, and max for numeric variables
# in each dataset.
# Used to detect:
#   - Implausible values
#   - Data entry errors
#   - Outliers
#   - Distribution differences between datasets

map_dfr(datasets, ~
          summarise(.x,
                    across(where(is.numeric),
                           list(
                             min = ~ min(.x, na.rm = TRUE),
                             median = ~ median(.x, na.rm = TRUE),
                             mean = ~ mean(.x, na.rm = TRUE),
                             max = ~ max(.x, na.rm = TRUE)
                           ))),
        .id = "dataset")




