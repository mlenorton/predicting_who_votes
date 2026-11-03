# Independent practice: Fitting logistic regression models with voter turnout 
# data from GSSR

# Load libraries
library(magrittr)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(knitr)

# Install gss package (only need to do once)
install.packages("gssr", repos = c("https://kjhealy.r-universe.dev", "https://cloud.r-project.org"))

# Save data once
if (!file.exists("gss_turnout.csv")) {
  library(gssr)
  data(gss_all)
  gss_all %>%
    haven::zap_labels() %>%
    filter(year %in% c(2022, 2024), vote20 %in% 1:2) %>%   # 1 = voted, 2 = did not vote (ineligible dropped)
    select(year, vote20, age, educ, degree, sex, marital, partyid, coninc, race, wtssps) %>%
    write_csv("gss_turnout.csv")
}

# Recode some variables and select only a subset of variables [modify as desired]
recode_gss <- function(df) {
  df %>%
    mutate(
      voted   = as.integer(vote20 == 1),
      age10   = age / 10,
      married = as.integer(marital == 1),
      inc10k  = coninc / 10000,
      degree  = factor(degree, 0:4, c("Less than HS", "High school", "Associate", "Bachelor's", "Graduate")),
      # strength of party identification, folded (which party doesn't matter here)
      pstrength = case_when(partyid == 3 ~ 0, partyid %in% c(2, 4) ~ 1,
                            partyid %in% c(1, 5) ~ 2, partyid %in% c(0, 6) ~ 3)
    ) %>%
    select(voted, age, age10, educ, degree, inc10k, married, pstrength) %>%
    drop_na()
}

gss    <- read_csv("gss_turnout.csv", show_col_types = FALSE)
vote   <- gss %>% filter(year == 2024) %>% recode_gss()
vote22 <- gss %>% filter(year == 2022) %>% recode_gss()

# Your turn to explore data and fit logistic regression models
