# ==========================================================
# Script: 02_data_cleaning.R
# Purpose: Clean, harmonise, and combine raw screening datasets
#          into a single analysis-ready dataset.
# Author: Nikolaos Samperis
# Date: 20/02/2026
# ==========================================================


# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------
# tidyverse: data manipulation
# stringr:   file name processing

library(tidyverse)
library(stringr)

# ----------------------------------------------------------
# 2. Load and order raw datasets
# ----------------------------------------------------------
# Ensures consistent dataset ordering (DN01–DN012).

files <- list.files("raw_data",
                    pattern = "\\.csv$",
                    full.names = TRUE) |>
  str_sort(numeric = TRUE)

datasets <- set_names(files, basename(files)) |>
  map(~ read_csv(.x, show_col_types = FALSE))

# ----------------------------------------------------------
# 3. Remove auto-generated index column
# ----------------------------------------------------------
# Some CSV files contain an export index column ("...1").
# This is not analytically meaningful and is removed.

datasets <- map(datasets, ~ {
  if(names(.x)[1] == "...1") {
    select(.x, -1)
  } else {
    .x
  }
})

# ----------------------------------------------------------
# 4. Standardise variable names
# ----------------------------------------------------------
# Variable names are harmonised across datasets to ensure
# consistent naming prior to merging.

datasets <- map(datasets, ~ {
  
  df <- .x
  names_lower <- tolower(names(df))
  
  # Rename based on lowercase comparison
  if("screening" %in% names_lower)
    df <- rename(df, screening_decision = !!names(df)[which(names_lower == "screening")])
  
  if("age" %in% names_lower)
    df <- rename(df, age = !!names(df)[which(names_lower == "age")])
  
  if("weight" %in% names_lower)
    df <- rename(df, weight = !!names(df)[which(names_lower == "weight")])
  
  if("height" %in% names_lower)
    df <- rename(df, height = !!names(df)[which(names_lower == "height")])
  
  if("alcohol" %in% names_lower)
    df <- rename(df, alcohol_units = !!names(df)[which(names_lower == "alcohol")])
  
  if("es" %in% names_lower)
    df <- rename(df, employment_status = !!names(df)[which(names_lower == "es")])
  
  if("deps" %in% names_lower)
    df <- rename(df, n_dependents = !!names(df)[which(names_lower == "deps")])
  
  df
})

# ----------------------------------------------------------
# 5. Ensure structural consistency (DN01 issue)
# ----------------------------------------------------------
# DN01 does not contain employment_status.
# Add missing column to preserve consistent structure.

datasets <- map(datasets, ~ {
  if(!"employment_status" %in% names(.x)) {
    .x$employment_status <- NA
  }
  .x
})

# ----------------------------------------------------------
# 6. Standardise column order
# ----------------------------------------------------------
# Enforces identical column ordering across datasets.

standard_cols <- c(
  "age",
  "weight",
  "height",
  "alcohol_units",
  "employment_status",
  "n_dependents",
  "screening_decision"
)

datasets <- map(datasets, ~ .x[, standard_cols])

# ----------------------------------------------------------
# 7. Type conversion and unit harmonisation
# ----------------------------------------------------------
# Convert variables to appropriate types.
# Height converted from centimetres to metres for analysis.

datasets <- map(datasets, ~ {
  
  .x |>
    mutate(
      age = as.numeric(age),
      weight = as.numeric(weight),
      height = as.numeric(height) / 100,   # cm → meters (no rounding)
      alcohol_units = as.numeric(alcohol_units),
      n_dependents = as.integer(n_dependents),
      employment_status = as.factor(employment_status),
      screening_decision = as.factor(screening_decision)
    )
})

# ----------------------------------------------------------
# 8. Add clinical team identifier and merge
# ----------------------------------------------------------
# Adds dataset source identifier before row-binding.


combined_data <- imap_dfr(
  datasets,
  ~ mutate(.x,
           clinical_team = str_remove(.y, "\\.csv$"))
)


combined_data$clinical_team <- factor(combined_data$clinical_team)

# ----------------------------------------------------------
# 9. Final validation checks
# ----------------------------------------------------------
# Verify structure and basic summaries of merged dataset.

glimpse(combined_data)
summary(combined_data)

# Confirm height now expressed in metres
summary(combined_data$height)

# ----------------------------------------------------------
# 10. Export cleaned dataset
# ----------------------------------------------------------
# Round numeric display precision at export stage only
# (avoids floating-point artefacts while preserving accuracy).

combined_data <- combined_data |>
  mutate(
    height = round(height, 3),
    weight = round(weight, 3)
  )

if(!dir.exists("output")) {
  dir.create("output")
}

write_csv(combined_data,
          file = "output/breast_screening_cleaned.csv")
