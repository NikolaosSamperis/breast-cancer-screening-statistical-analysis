# ==========================================================
# Script: 03_validation.R
# Purpose: Perform structural and logical validation checks
#          on the cleaned breast screening dataset.
# Author: Nikolaos Samperis
# Date: 20/02/2026
# ==========================================================

# ----------------------------------------------------------
# 1. Load required libraries and dataset
# ----------------------------------------------------------
# tidyverse: data manipulation
# janitor:   column name validation
# skimr:     structured dataset summary

library(tidyverse)
library(janitor)
library(skimr)

# Import cleaned dataset with explicit column types
# (ensures structural consistency at validation stage)

data <- read_csv(
  "output/breast_screening_cleaned.csv",
  col_types = cols(
    age = col_double(),
    weight = col_double(),
    height = col_double(),
    alcohol_units = col_double(),
    n_dependents = col_integer(),
    employment_status = col_factor(),
    screening_decision = col_factor(),
    clinical_team = col_factor()
  )
)

cat("Validation started...\n\n")

# ----------------------------------------------------------
# 2. Structural validation
# ----------------------------------------------------------
# Confirm dataset dimensions and column integrity.

cat("===== STRUCTURE =====\n")
cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n\n")

# Ensure column names follow clean naming conventions
if(any(names(data) != make_clean_names(names(data)))) {
  warning("Column names are not fully standardised.")
}

# ----------------------------------------------------------
# 3. Duplicate record assessment
# ----------------------------------------------------------
# Identify fully duplicated rows (including first occurrence).

cat("===== DUPLICATE CHECK =====\n")

duplicate_rows <- data[
  duplicated(data) | duplicated(data, fromLast = TRUE),
]

duplicate_count <- nrow(duplicate_rows)

cat("Number of duplicated rows:", duplicate_count, "\n")

if(duplicate_count == 0){
  cat("No duplicated rows found.\n\n")
} else {
  cat("Duplicated rows detected. Displaying all duplicates:\n")
  print(duplicate_rows)
  cat("\n")
}

# ----------------------------------------------------------
# 4. Missing data validation
# ----------------------------------------------------------
# Assess missingness overall and by clinical team
# to detect potential systematic patterns.

if("clinical_team" %in% names(data)){
  
  cat("===== MISSINGNESS BY CLINICAL TEAM =====\n")
  
  vars_to_check <- setdiff(names(data), "clinical_team")
  
  missing_by_team <- data %>%
    group_by(clinical_team) %>%
    summarise(
      across(
        all_of(vars_to_check),
        list(
          missing_count  = ~ sum(is.na(.)),
          missing_percent = ~ round(mean(is.na(.)) * 100, 2)
        )
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -clinical_team,
      names_to = c("variable", ".value"),
      names_pattern = "(.+)_(missing_count|missing_percent)"
    ) %>%
    filter(missing_count > 0) %>%
    arrange(desc(missing_percent))
  
  if(nrow(missing_by_team) == 0){
    cat("No missing values detected across clinical teams.\n\n")
  } else {
    print(missing_by_team, n = Inf)
    cat("\n")
  }
}

# ----------------------------------------------------------
# 5. Logical plausibility checks
# ----------------------------------------------------------
# Evaluate whether key numeric variables fall within
# clinically plausible ranges.

cat("===== LOGICAL CHECKS =====\n")

# Age plausibility (adult screening population)
if("age" %in% names(data)){
  implausible_age <- sum(data$age < 18 | data$age > 100, na.rm = TRUE)
  cat("Implausible age values (<18 or >100):", implausible_age, "\n")
}

# Height plausibility (in metres)
if("height" %in% names(data)){
  implausible_height <- sum(data$height < 1.3 | data$height > 2.2, na.rm = TRUE)
  cat("Implausible height values (<1.3m or >2.2m):", implausible_height, "\n")
}

# Weight plausibility (in kg)
if("weight" %in% names(data)){
  implausible_weight <- sum(data$weight < 30 | data$weight > 250, na.rm = TRUE)
  cat("Implausible weight values (<30kg or >250kg):", implausible_weight, "\n")
}

cat("\n")

# ----------------------------------------------------------
# 6. Overall dataset profile
# ----------------------------------------------------------
# Generate structured summary of variable distributions
# to support exploratory interpretation.

cat("===== SKIM SUMMARY =====\n")
skim(data)

cat("\nValidation completed successfully.\n")
