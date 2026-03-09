# ==========================================================
# Script: 05_statistical_modelling.R
# Purpose: Examine associations between patient
#          characteristics and breast cancer screening uptake
#          using univariate tests and multivariable logistic regression.
# Author: Nikolaos Samperis
# Date: 20/02/2026
# ==========================================================

# ----------------------------------------------------------
# 1. Load required libraries
# ----------------------------------------------------------
# tidyverse: data manipulation and visualisation
# broom: tidy model outputs for plotting
# car: variance inflation factor (VIF) for multicollinearity assessment

library(tidyverse)
library(broom)
library(car)

# -------------------------------------------------------------------------
# 2. Unadjusted Associations Between Patient Characteristics and Screening
# -------------------------------------------------------------------------

# Objective:
# Examine crude (unadjusted) associations between individual
# patient-level variables and screening uptake.
# Continuous variables are tested using t-tests (or Wilcoxon for skewed data),
# and categorical variables using chi-square tests.

# Null hypothesis (for each test):
# No association between the variable and screening uptake.

t.test(age ~ screening_decision, data = data)

# Non-parametric alternative due to right-skewed alcohol distribution
wilcox.test(alcohol_units ~ screening_decision, data = data)

chisq.test(table(data$employment_status, data$screening_decision))
chisq.test(table(data$n_dependents, data$screening_decision))

t.test(weight ~ screening_decision, data = data)
t.test(height ~ screening_decision, data = data)

# ------------------------------------------------------------------------
# 3. Multivariable Logistic Regression: Factors Associated with Screening
# ------------------------------------------------------------------------

# Objective: assess independent predictors of screening uptake while adjusting
# for key patient characteristics and clinical team. A parsimonious model is
# specified a priori based on plausibility and interpretability, then a more
# comprehensive model is explored as a sensitivity analysis.

# Set reference level for screening outcome (optional but recommended)
# Here, "N" (not screened) is treated as the reference category
data$screening_decision <- relevel(data$screening_decision, ref = "N")

# ---------------------------------
# 3.1 Primary (parsimonious) model
# ---------------------------------
# Core covariates were selected based on their plausible relationship with
# screening behaviour and potential to confound team comparisons (age,
# employment status, number of dependents, alcohol intake),
# alongside clinical team.

model_primary <- glm(
  screening_decision ~
    age +
    alcohol_units +
    employment_status +
    n_dependents +
    clinical_team,
  data = data,
  family = binomial
)

summary(model_primary)

# Convert log-odds coefficients to Odds Ratios with 95% CI
exp(cbind(
  OR = coef(model_primary),
  confint(model_primary)
))

# ---- Multicollinearity Check ----
# VIF > 5 indicates moderate concern
# VIF > 10 indicates serious multicollinearity

vif_primary <- vif(model_primary)
print(vif_primary)

# -------------------------------------------------------------------
# 3.2 Sensitivity analysis: extended model including weight & height
# -------------------------------------------------------------------
# Weight and height were associated with screening in univariate analyses.
# They are added here to assess whether these crude associations persist after
# adjustment, and whether inclusion materially changes the primary model estimates.

model_extended <- glm(
  screening_decision ~
    age +
    weight +
    height +
    alcohol_units +
    employment_status +
    n_dependents +
    clinical_team,
  data = data,
  family = binomial
)

summary(model_extended)

exp(cbind(
  OR = coef(model_extended),
  confint(model_extended)
))

# If the extended model shows inflated standard errors/unstable estimates
# (suggesting multicollinearity) and does not improve interpretability, the
# primary model is retained for final inference.

# ---- Multicollinearity Check (Extended Model) ----

vif_extended <- vif(model_extended)
print(vif_extended)

# ------------------------------------------------------------------------
# 3.3 Forest Plot – Adjusted Odds Ratios (Primary Model)
# ------------------------------------------------------------------------

# Tidy model output and exponentiate coefficients to OR scale
forest_data <- tidy(model_primary, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_clean = case_when(
      term == "age" ~ "Age (per year increase)",
      term == "alcohol_units" ~ "Alcohol intake (per additional unit)",
      term == "n_dependents" ~ "Number of dependents",
      str_detect(term, "^employment_status") ~
        paste0(str_remove(term, "^employment_status"), " vs Employed"),
      str_detect(term, "^clinical_team") ~
        paste0(str_remove(term, "^clinical_team"),
               " vs DN01 (reference)"),
      TRUE ~ term
    ),
    group = if_else(str_detect(term, "^clinical_team"),
                    "Clinical Team",
                    "Patient Characteristics")
  )

# Order clinical teams numerically
teams_df <- forest_data %>%
  filter(group == "Clinical Team") %>%
  mutate(
    team_number = as.integer(str_extract(term_clean, "(?<=DN)\\d+"))
  ) %>%
  arrange(team_number) %>%
  mutate(term_clean = factor(term_clean, levels = rev(term_clean))) %>%
  select(-team_number)

# Order Patient Characteristics (keep logical order)
patients_df <- forest_data %>%
  filter(group == "Patient Characteristics") %>%
  mutate(term_clean = factor(term_clean, levels = rev(term_clean)))

# Combine
forest_data_final <- bind_rows(teams_df, patients_df)

# Forest plot of adjusted ORs (log scale)
ggplot(forest_data_final,
       aes(x = estimate, y = term_clean)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 linewidth = 0.8) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             colour = "grey40") +
  scale_x_log10() +
  facet_wrap(~ group, scales = "free_y") +
  labs(
    x = "Adjusted Odds Ratio",
    y = "",
    title = "Factors Associated with Breast Cancer Screening Uptake",
    subtitle = "Values to the right of 1 indicate higher screening uptake"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 15)),
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(margin = margin(t = 15))
  )