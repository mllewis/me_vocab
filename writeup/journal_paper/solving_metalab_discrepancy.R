
library(tidyverse)

GSHEETS_DATA_METALAB <-  "/Users/mollylewis/Downloads/ME_tabular_data - Sheet3.csv"
METALAB_DATA_PATH <- "/Users/mollylewis/Downloads/Mutual\ exclusivity.csv"
GSHEETS_DATA_PAPER <-   "/Users/mollylewis/Downloads/ME_tabular_data - Sheet3.csv"
PAPER_PROCESSED_DATA <- "/Users/mollylewis/Documents/research/Projects/1_in_progress/me_vocab/data/0_metaanalysis_data.csv"

bad_conditions <- c("demarchena2011", "yow2017b")
# yow is missing because of d_var_calc logic, not clear why demarchena is missing
gsheets_data_metalab <- read_csv(GSHEETS_DATA_METALAB) %>%
  #  select(1:4, 6:16, 18, 19, 21, 27:29, 31:34, 48:50) %>%
  arrange(study_ID, expt_num) %>%
  mutate(gsheets_metalab = 1)%>%
  filter(study_ID %in% bad_conditions)


metalab_data <- read_csv(METALAB_DATA_PATH) %>%
#  select(study_ID, expt_num, mean_age_1, n_1, d_calc, es_method, x_1, same_infant, SD_1) %>%
  arrange(study_ID, expt_num) %>%
  mutate(expt_num = as.character(expt_num)) %>%
  mutate(metalab = 1) %>%
  filter(study_ID %in% bad_conditions)



gsheets_data_paper <- read_csv(GSHEETS_DATA_PAPER) %>%
 # select(study_ID, expt_num, n_1) %>%
  arrange(study_ID, expt_num) %>%
  mutate(gsheets_paper = 1)%>%
  filter(study_ID %in% bad_conditions)


paper_processed <- read_csv(PAPER_PROCESSED_DATA) %>%
  #select(study_ID, expt_num, mean_age_1, n_1, d_calc, es_method, x_1, same_infant, SD_1) %>%
  rename(n1p = n_1, d_calcp = d_calc, es_methodp = es_method) %>%
  arrange(study_ID, expt_num) %>%
  mutate(paper_processed = 1)%>%
  filter(study_ID %in% bad_conditions)



both <- left_join(paper_processed, metalab_data, by = c("study_ID", "expt_num", "mean_age_1",
                                                        "x_1", "same_infant", "SD_1")) %>%
select(1:4, 6:16, 18, 19, 21, 27:29, 31:34, 48:50, 59)

both %>% filter(is.na(metalab))

source("/Users/mollylewis/Documents/research/Projects/2_published/metalab/metalab2/scripts/compute_es.R")
dataset_data_calc <- gsheets_data_metalab %>%
  mutate(short_name = "x") %>%
  mutate(row = 1:n()) %>%
  split(.$row) %>%
  map_df(~bind_cols(
    .x, compute_es(
      .x$participant_design, .x$x_1, .x$x_2, .x$x_dif, .x$SD_1, .x$SD_2,
      .x$SD_dif, .x$n_1, .x$n_2, .x$t, .x$F, .x$d, .x$d_var, .x$corr,
      .x$corr_imputed, .x$r, .x$r_var, .x$study_ID, .x$expt_num,
      .x$special_cases_measures, .x$contrast_sampa, .x$short_name
    ))) %>%
  select(-row)


###### MODEL ####
metalab_data <- read_csv(METALAB_DATA_PATH) %>%
  mutate(unique_row = 1:n())

paper_model <- rma.mv(d_calc, V = d_var_calc,
                random = ~ 1 | same_infant/short_cite,
                method = "REML",
                data = metalab_data)

metalab_model <-
  paper_model <- rma.mv(d_calc, V = d_var_calc,
                        random = ~ 1 | short_cite/same_infant_calc/unique_row,
                        method = "REML",
                        data = metalab_data)

metafor::rma.mv(yi = data()[[es()]], V = data()[[es_var()]],
                                       #random = ~ 1 | data()[["short_cite"]],
                                       random = ~ 1 | data()[["short_cite"]] / data()[["same_infant_calc"]] / data()[["unique_row"]],
                                       slab = make.unique(data()[["short_cite"]]),
                                       method = "REML")
