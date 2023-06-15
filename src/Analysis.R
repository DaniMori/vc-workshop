library(tidyverse)
library(gtsummary)
library(survival)

DATASET_PATH <- "dat/breslow_chatterjee_1999.csv"

study_data <- read_csv(DATASET_PATH)

study_data |> tbl_summary()

with(nwtco, table(instit,histol))
anova(coxph(Surv(edrel,rel)~histol+instit,data=nwtco))
anova(coxph(Surv(edrel,rel)~instit+histol,data=nwtco))
