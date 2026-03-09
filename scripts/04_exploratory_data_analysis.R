# ==========================================================
# Script: 04_exploratory_analysis.R
# Purpose: Perform exploratory and descriptive analysis of
#          the cleaned breast screening dataset.
# Author: Nikolaos Samperis
# Date: 20/02/2026
# ==========================================================


# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------
# tidyverse: data manipulation & visualisation
# naniar: missing data visualisation
# ComplexUpset: missingness pattern plots
# effectsize: effect size calculations
# binom: binomial confidence intervals
# broom: model tidying

library(tidyverse)
library(naniar)
library(ComplexUpset)
library(effectsize)
library(binom)
library(broom)

# ----------------------------------------------------------
# 2. Load cleaned dataset
# ----------------------------------------------------------

# Ensure categorical variables are correctly specified
data <- data |>
  mutate(
    clinical_team = factor(clinical_team),
    screening_decision = factor(screening_decision),
    employment_status = factor(employment_status)
  )

# ----------------------------------------------------------
# 3. Overall missingness assessment
# ----------------------------------------------------------
# Summarise total missing values per variable.

missing_summary <- data.frame(
  variable = names(data),
  missing_count = colSums(is.na(data)),
  missing_percent = round(colMeans(is.na(data)) * 100, 2)
) |>
  arrange(desc(missing_percent))

print("===== OVERALL MISSINGNESS =====")
print(missing_summary)

# Visualisation of missingness per variable
p <- gg_miss_var(data)

for (i in seq_along(p$layers)) {
  g <- class(p$layers[[i]]$geom)[1]
  
  if (g == "GeomPoint") {
    p$layers[[i]]$aes_params$size <- 3
  }
  
  if (g == "GeomBar") {
    p$layers[[i]]$aes_params$linewidth <- 1.5
    p$layers[[i]]$aes_params$size <- 1.5
  }
}

print(
  p +
    labs(
      title = "Missing Values per Recorded Patient Characteristic",
      y = "Number of Missing Observations",
      x = NULL
    ) +
    theme(
      plot.title = element_text(face = "bold", margin = margin(b = 20), hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 20)),
      legend.position = "none"
    )
)

# ----------------------------------------------------------
# 4. Missingness pattern (UpSet)
# ----------------------------------------------------------
# Examine joint missingness patterns across key variables.

data_upset <- data |>
  mutate(
    missing_alcohol_units     = is.na(alcohol_units),
    missing_employment_status = is.na(employment_status),
    missing_height            = is.na(height)
  )

upset(
  data_upset,
  intersect = c(
    "missing_alcohol_units",
    "missing_employment_status",
    "missing_height"
  ),
  name = "Missing Pattern",
  
  base_annotations = list(
    'Intersection size' =
      intersection_size(text = list(size = 3)) +
      labs(y = "Intersection size") +
      ggtitle("Patterns of Missing Data Across Patient Characteristics") +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = 0.5,
          margin = margin(b = 15)
        ),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 0)
      )
  ),
  
  set_sizes =
    upset_set_size() +
    labs(caption = "Total missing per variable") +
    theme(
      plot.caption = element_text(
        hjust = 0.5,
        vjust = 7,
        size = 11
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
) +
  labs(x = "Missingness Combination") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15))
  )

# ----------------------------------------------------------
# 5. Missingness by clinical team
# ----------------------------------------------------------
# Calculate percentage missing per variable within each team.

missing_by_team <- data %>%
  group_by(clinical_team) %>%
  summarise(
    alcohol_missing = mean(is.na(alcohol_units)) * 100,
    height_missing  = mean(is.na(height)) * 100,
    weight_missing  = mean(is.na(weight)) * 100,
    employment_missing = mean(is.na(employment_status)) * 100
  )

print("===== MISSINGNESS BY CLINICAL TEAM (%) =====")
print(missing_by_team)

# Set clinical_team levels to numerical order (DN01–DN12)
# Ensures consistent ordering in plots and summaries.

data$clinical_team <- factor(
  data$clinical_team,
  levels = c("DN01","DN02","DN03","DN04","DN05","DN06",
             "DN07","DN08","DN09","DN010","DN011","DN012")
)


# Plot alcohol missingness proportion by clinical team
ggplot(data, aes(x = clinical_team, fill = is.na(alcohol_units))) +
  geom_bar(position = "fill") +
  labs(
    title = "Alcohol Intake Missingness by Clinical Team",
    x = "Clinical Team",
    y = "Proportion",
    fill = "Alcohol Missing"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,               # center title
      margin = margin(b = 15)    # space below title
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)    # space between x label and plot
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)    # space between y label and plot
    )
  )

# ----------------------------------------------------------
# 6. Assessment of missingness mechanism
# ----------------------------------------------------------
# Evaluate associations between missingness indicators and
# screening behaviour or demographic variables.

# --------- Alcohol missing indicator ----------
data$alcohol_missing <- is.na(data$alcohol_units)

# Association with screening decision
print("===== CHI-SQUARE: Alcohol Missing vs Screening =====")
print(table(data$screening_decision, data$alcohol_missing))
print(chisq.test(data$screening_decision, data$alcohol_missing))

# Association with age
print("===== T-TEST: Age vs Alcohol Missing =====")
print(t.test(age ~ alcohol_missing, data = data))

# ------ Height missing indicator ------
data$height_missing <- is.na(data$height)

print("===== FISHER'S EXACT TEST: Height Missing vs Screening decision =====")
tab_height <- table(data$screening_decision, data$height_missing)
print(tab_height)
print(fisher.test(tab_height))

print("===== T-TEST: Age vs Height Missing =====")
print(t.test(age ~ height_missing, data = data))

# ---------- Employment missing indicator -------------
data$employment_missing <- is.na(data$employment_status)

print("===== CHI-SQUARE: Employment Missing vs Screening decision =====")
print(table(data$screening_decision, data$employment_missing))
print(chisq.test(data$screening_decision, data$employment_missing))

# ----------------------------------------------------------
# 7. Visual comparison of age distribution by missingness
# ----------------------------------------------------------
# Compare age density distributions between individuals
# with and without missing alcohol intake data.

ggplot(data, aes(x = age, fill = alcohol_missing)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Age Distribution by Alcohol Missingness",
    x = "Age (years)",
    y = "Density",
    fill = "Alcohol Missing"
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,              # center title
      margin = margin(b = 15)   # space below title
    ),
    axis.title.x = element_text(
      margin = margin(t = 15)   # space between x label and panel
    ),
    axis.title.y = element_text(
      margin = margin(r = 15)   # space between y label and panel
    )
  )


# ----------------------------------------------------------
# 8. Association Between Missingness and Clinical Team
# ----------------------------------------------------------
# Test whether missingness varies systematically across
# clinical teams using chi-square tests.

test_missing_vs_team <- function(var_name) {
  
  missing_indicator <- is.na(data[[var_name]])
  
  cat("\n====================================\n")
  cat("Missingness vs Clinical Team:", var_name, "\n")
  cat("====================================\n")
  
  tab <- table(data$clinical_team, missing_indicator)
  print(tab)
  
  # Chi-square test for 12x2 contingency table
  print(chisq.test(tab))
}


# Run tests for selected variables
test_missing_vs_team("alcohol_units")
test_missing_vs_team("height")
test_missing_vs_team("employment_status")

# ----------------------------------------------------------
# 9. Summary Statistics by Clinical Team (Continuous Variables)
# ----------------------------------------------------------
# Calculate mean, SD, median, and IQR for numeric variables
# within each clinical team.

team_summary <- data %>%
  group_by(clinical_team) %>%
  summarise(
    n = n(),
    across(
      c(age, weight, height, alcohol_units, n_dependents),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        IQR = ~IQR(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )

team_summary

# ----------------------------------------------------------
# 10. Distribution of Continuous Variables by Clinical Team
# ----------------------------------------------------------
# Visual inspection of within-team distributions using
# faceted histograms.

plot_histogram_by_team_facet <- function(df, var_name, bins = 20) {
  
  ggplot(df, aes(x = .data[[var_name]])) +
    geom_histogram(
      bins = bins,
      fill = "steelblue",
      colour = "black",
      na.rm = TRUE
    ) +
    facet_wrap(~ clinical_team) +
    labs(
      title = paste("Distribution of", var_name, "by Clinical Team"),
      x = var_name,
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

numeric_vars <- c("age", "weight", "height", "alcohol_units")

for (var in numeric_vars) {
  print(plot_histogram_by_team_facet(data, var))
}

# ------------------------------------------------------------
# 11. Descriptive Analysis of Categorical & Discrete Variables
# ------------------------------------------------------------

# Employment status distribution within each team
employment_by_team <- data %>%
  count(clinical_team, employment_status) %>%
  group_by(clinical_team) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

employment_by_team

ggplot(data, aes(x = employment_status)) +
  geom_bar(fill = "steelblue", colour = "black") +
  facet_wrap(~ clinical_team) +
  labs(
    title = "Employment Status Distribution by Clinical Team",
    x = "Employment Status",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Screening decision distribution within each team
screening_by_team <- data %>%
  count(clinical_team, screening_decision) %>%
  group_by(clinical_team) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

screening_by_team

ggplot(data, aes(x = screening_decision)) +
  geom_bar(fill = "steelblue", colour = "black") +
  facet_wrap(~ clinical_team) +
  labs(
    title = "Screening Decision by Clinical Team",
    x = "Screening Decision",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# Number of dependents distribution within each team
dependents_by_team <- data %>%
  count(clinical_team, n_dependents) %>%
  group_by(clinical_team) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  ungroup()

dependents_by_team

# Bar chart: Distribution of number of dependents by clinical team
# Treated as categorical for clearer visual comparison
ggplot(data, aes(x = factor(n_dependents))) +
  geom_bar(fill = "steelblue", colour = "black") +
  facet_wrap(~ clinical_team) +
  labs(
    title = "Number of Dependents by Clinical Team",
    x = "Number of Dependents",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# ----------------------------------------------------------
# 12. Between-Team Heterogeneity (Continuous Variables)
# ----------------------------------------------------------
# Assess differences across clinical teams using ANOVA
# and non-parametric testing where appropriate.

continuous_vars <- c("age", "weight", "height", "alcohol_units")

anova_results <- lapply(continuous_vars, function(var) {
  formula <- as.formula(paste(var, "~ clinical_team"))
  summary(aov(formula, data = data))
})

names(anova_results) <- continuous_vars
anova_results

# Kruskal–Wallis test for alcohol intake across clinical teams
# as it appears moderately right-skewed
kruskal_alcohol <- kruskal.test(alcohol_units ~ clinical_team, data = data)

kruskal_alcohol

# Height showed significant between-team heterogeneity (ANOVA p < 0.001).
# DN012 had a notably higher mean height compared to other teams (~1.75m vs ~1.60m),
# raising the possibility of a unit or scaling inconsistency.
# Raw DN012 values are inspected to verify measurement units 
# and rule out harmonisation errors.

# Raw DN012 heights
summary(read.csv("raw_data/DN012.csv")$Height)

# Cleaned DN012 heights
data %>%
  filter(clinical_team == "DN012") %>%
  summarise(min = min(height, na.rm = TRUE),
            max = max(height, na.rm = TRUE),
            mean = mean(height, na.rm = TRUE))

# Raw DN012 heights were recorded in centimetres and correctly converted 
# to metres during cleaning. No scaling inconsistencies detected.
# Observed between-team heterogeneity in height reflects genuine 
# demographic variation rather than a data processing error.

# Effect size (eta-squared) for height differences
model_height <- aov(height ~ clinical_team, data = data)
eta_height <- eta_squared(model_height)
eta_height

# ----------------------------------------------------------
# 13. Between-Team Heterogeneity (Categorical Variables)
# ----------------------------------------------------------
# Use chi-square tests to assess variation across teams.

# Exclude DN01 (employment not recorded) and drop unused factor levels
# to ensure valid chi-square testing.
data_noDN01 <- droplevels(
  data[data$clinical_team != "DN01", ]
)

# Contingency table
table_employment <- table(
  data_noDN01$employment_status,
  data_noDN01$clinical_team
)
table_employment

# Chi-square test
chisq.test(table_employment)

# Contingency table
table_screening <- table(data$screening_decision, data$clinical_team)
table_screening

# Chi-square test
chisq.test(table_screening)


# Contingency table
table_dependents <- table(data$n_dependents, data$clinical_team)
table_dependents

# Chi-square test
chisq.test(table_dependents)


# Although screening uptake showed statistical heterogeneity across teams,
# effect size is required to assess the practical magnitude of this association.
# Cramér’s V provides a standardized measure of association for categorical data.
cramers_v(table_screening)

# Calculate within-team screening proportions to examine differences
# in uptake rates across clinical teams.
prop.table(table_screening, margin = 2)

# ----------------------------------------------------------
# 14. Screening Uptake by Clinical Team
# ----------------------------------------------------------
# Estimate screening proportions with Wilson 95% confidence intervals.

screening_plot_data <- data %>%
  group_by(clinical_team) %>%
  summarise(
    total_n = n(),
    n_positive = sum(screening_decision == "Y"),
    prop_screened = n_positive / total_n,
    .groups = "drop"
  )

ci <- binom.confint(
  screening_plot_data$n_positive,
  screening_plot_data$total_n,
  methods = "wilson"
)

screening_plot_data$lower_ci <- ci$lower
screening_plot_data$upper_ci <- ci$upper

# Calculate overall mean screening acceptance across teams
overall_mean <- mean(screening_plot_data$prop_screened)

# Preserve ascending order and include sample size in x-axis label
screening_plot_data <- screening_plot_data %>%
  arrange(clinical_team) %>%
  mutate(
    team_label = paste0(clinical_team, "\n(n=", total_n, ")"),
    team_label = factor(team_label, levels = team_label)
  )

# Create bar plot of screening uptake by clinical team
ggplot(screening_plot_data, aes(x = team_label, y = prop_screened)) +
  
  # Bars representing screening acceptance per team
  geom_col(fill = "steelblue") +
  
  # Add 95% binomial confidence intervals
  geom_errorbar(
    aes(ymin = lower_ci, ymax = upper_ci),
    width = 0.2,
    linewidth = 0.4
  ) +
  
  # Add percentage labels above bars
  geom_text(
    aes(label = paste0(round(prop_screened * 100, 1), "%")),
    vjust = -0.3,
    size = 3.5
  ) +
  
  # Add dashed horizontal line for overall mean acceptance
  geom_hline(
    yintercept = overall_mean,
    linetype = "dashed",
    colour = "darkred",
    linewidth = 0.8
  ) +
  
  annotate(
    "text",
    x = -Inf,
    y = overall_mean,
    label = paste0("Mean: ",
                   round(overall_mean * 100, 1), "%"),
    hjust = 0,
    vjust = -0.5,
    colour = "darkred",
    size = 3.5
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  
  labs(
    title = "Screening Acceptance Across Clinical Teams",
    x = "Clinical Team (Sample Size)",
    y = "Screening Acceptance (%)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12))
  )














