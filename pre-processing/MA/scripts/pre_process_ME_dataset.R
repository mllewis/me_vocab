# this scripts takes the raw inputted MA data from google sheets, caches it,
# and saves a second sheet with inputted ES values
library(tidyverse)
library(googlesheets)
library(here)

GOOGLE_SHEETS_WS_NAME <- "ME metalab tidy"
RAW_OUT <- here("pre-processing/MA/data/raw_MA_data.csv")
TIDY_OUT <- here("data/0_metaanalysis_data.csv")

# load ES script
source(here("pre-processing/MA/scripts/compute_es.R"))

# cache raw inputted data from google sheets
raw_me_data <- gs_title(GOOGLE_SHEETS_WS_NAME) %>%
  gs_read()

write_csv(raw_me_data, RAW_OUT)

data_with_es <- raw_me_data %>%
  mutate(row = 1:n()) %>%
  split(.$row) %>%
  map_df(~bind_cols(
    .x, compute_es(
      .x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1, .x$SD_2,
      .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d, .x$d_var, .x$corr,
      .x$corr_imputed, .x$r, .x$study_ID, .x$expt_num,
      .x$special_cases_measures, .x$contrast_sampa
    ))) %>%
  select(-row)

write_csv(data_with_es, TIDY_OUT)


