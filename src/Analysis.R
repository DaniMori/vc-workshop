## ----load-packages----
library(readr)
library(dplyr)
library(labelled)
# library(gtsummary) ## TODO: Install gtsummary if necessary
library(survival)

## ----file-system-objects----
DATASET_DIR <- "dat"

DATASET_PATH      <- file.path(DATASET_DIR, "breslow_chatterjee_1999.csv")

## ----main----

# Data preprocessing: ----

## Read dataset:
study_data <- read_csv(
  DATASET_PATH,
  col_types      = cols(
    instit       = col_factor(),
    histol       = col_factor(),
    stage        = col_factor(levels = as.character(1:4), ordered = TRUE),
    study        = col_factor(),
    in.subcohort = col_logical(),
    .default     = col_integer()
  )
)

## Recode values:
study_data <- study_data |> mutate(
  instit = instit |>
    factor(levels = 1:2, labels = c("Favourable", "Unfavourable")),
  histol = histol |>
    factor(levels = 1:2, labels = c("Favourable", "Unfavourable")),
  stage  = stage  |>
    factor(levels = 1:4, labels = c('I', 'II', 'III', 'IV'))
)

## Assign labels:
# (see https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/nwtco
#   for more info):
study_data <- study_data |> set_variable_labels(
  instit = "Histology (from local institution)",
  histol = "Histology (from central lab)",
  stage  = "Disease stage",
  study  = "Study",
  rel    = "Relapse", # As a 0/1 integer for compatibility with {survival}
  edrel  = "Time to relapse (days)",
  age    = "Age (months)"
  ## TODO: Transform `edrel` and `age` to years?
)


## Filter only cases in the example in Breslow & Chatterjee (1999):
study_data <- study_data |>
  filter(in.subcohort)   |>
  select(-in.subcohort)


# Descriptive analysis: ----

# NOTE: Descriptives not computed, as `gtsummary` package is too heavy

## Create descriptive statistics table:
# descriptive_table <- study_data |> tbl_summary(include = -seqno)

## Create contingency table of the histologies:
# contingency_table <- study_data |> tbl_cross(row = instit, col = histol)

# Statistical modeling and inference: ----

survival_fit <- coxph(Surv(edrel,rel) ~ histol + instit, data = study_data)
## TODO: Add covariates?

# NOTE: Output table not created, as `gtsummary` package is too heavy
# survival_coef_table <- survival_fit |> tbl_regression()
