## ----load-packages----
library(readr)
library(dplyr)
library(forcats)
library(labelled)
library(gtsummary)
library(gt)
library(survival)
library(survminer)

## ----file-system-objects----
OUTPUT_DIR  <- "output"
DATASET_DIR <- "dat"

DATASET_PATH      <- file.path(DATASET_DIR, "breslow_chatterjee_1999.csv")
DESC_TAB_OUT_PATH <- file.path(OUTPUT_DIR,  "descriptives.docx")
CONT_TAB_OUT_PATH <- file.path(OUTPUT_DIR,  "contingency.docx")
MODEL_OUT_PATH    <- file.path(OUTPUT_DIR,  "model_coefficients.docx")
FIGURE_OUT_PATH   <- file.path(OUTPUT_DIR,  "adjusted_curves.png")

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
  instit = instit |> fct_recode(Favourable = '1', Unfavourable = '2'),
  histol = histol |> fct_recode(Favourable = '1', Unfavourable = '2'),
  stage  = stage  |> fct_recode(I = '1', II = '2', III = '3', IV = '4')
)

## Assign labels:
# (see https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/nwtco
#   for more info):
study_data <- study_data |> set_variable_labels(
  instit = "Histology (from local institution)",
  histol = "Histology (from central lab)",
  stage  = "Disease stage"
  ## TODO: Complete labels for: `study`, `rel`, `edrel`, `age`
  ## TODO: Transform `edrel` and `age` to years
)


## Filter only cases in the example in Breslow & Chatterjee (1999):
study_data <- study_data |>
  filter(in.subcohort)   |>
  select(-in.subcohort)


# Descriptive analysis: ----

## Create descriptive statistics table:
descriptive_table <- study_data |> tbl_summary(include = -seqno)
descriptive_table |> as_gt() |> gtsave(DESC_TAB_OUT_PATH)

## Create contingency table of the histologies:
contingency_table <- study_data |> tbl_cross(row = instit, col = histol)
contingency_table |> as_gt() |> gtsave(CONT_TAB_OUT_PATH)

# Statistical modeling and inference: ----

survival_fit <- coxph(Surv(edrel,rel) ~ histol + instit, data = study_data)
## TODO: Add covariates?

survival_coef_table <- survival_fit |> tbl_regression()
survival_coef_table |> as_gt() |> gtsave(MODEL_OUT_PATH)

surv_plot <- survival_fit |> ggadjustedcurves(
  variable     = "histol",
  data         = as.data.frame(study_data),
  method       = "average",
  legend.title = study_data |> dplyr::pull(histol) |> var_label()
) +
  theme_survminer(base_family = "serif")
## TODO: Update theme / format / labels for journal?

surv_plot |> ggsave(filename = FIGURE_OUT_PATH)
