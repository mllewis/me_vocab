# get dois 

library(tidyverse)
library(RefManageR)
library(feather)

OUTFILE <- "ME_MA_DOIS.csv"
BIBOUTFILE <-  "MA_bib1.bib"
AVG_MONTH <- 30.43688

ma_raw <- read_feather("../data/mutual_exclusivity") %>%
  select(c(1:3, 6, 9, 12, 14, 15, 18, 20, 22:24, 26,
           29:30, 49, 51:53, 95:99, 100, 102:103)) %>%
  mutate_if(is.character, as.factor)


ma_c <- ma_raw %>%
  filter(!is.na(d_calc)) %>%
  mutate(mean_age = mean_age_1/AVG_MONTH,
         year = as.numeric(str_sub(short_cite, -5, -2)),
         condition_type = as.factor(ifelse(infant_type == "typical" & ME_trial_type == "FN", "TFN", 
                                           ifelse(infant_type == "typical" & ME_trial_type == "NN", "TNN", 
                                                  as.character(infant_type))))) %>%
  filter(mean_age < 150)  # exclude MR population?

ma_citation_data <- ma_raw %>%
  distinct(study_ID, .keep_all = T) %>%
  select(study_ID, short_cite, long_cite)%>%
  rowwise()%>%
  mutate(year = str_c(str_extract_all(short_cite, "[[:digit:]]")[[1]], collapse = ""))


get_bibtext_citation <- function(row_num, ma_data, outdfname) {
  cat("\n=== TARGET CITATION ===\n\n")
  
  print(as.character(ma_data$long_cite[row_num]))
  refs <- ReadCrossRef(as.character(ma_data$long_cite[row_num]), 
                       year = ma_data$year[row_num])
  
  cat("\n=== CITATION GUESS ===\n\n", "AUTHOR: ", str_c(refs[1]$author, collapse = " "), "\n",
      "TITLE: ", refs[1]$title, "\n",
      "JOURNAL: ", refs[1]$journal, "\n",
      "YEAR: ", refs[1]$year, "\n ")
  
  correct_citation_id <- readline(prompt = "Enter 1 if correct, 2 if incorrect: ") %>%
    as.numeric()
  
  if (correct_citation_id == 1) {
    correct_citation_doi <- refs[1]$doi
  } else {
    correct_citation_doi <- NA
  }
  
  out_df <- data.frame(study_ID = ma_data$study_ID[row_num],
                       bib_doi = correct_citation_doi)
  
  write_csv(out_df, outdfname, append = T)
  
}

walk(1:nrow(ma_citation_data),
     get_bibtext_citation,
     ma_citation_data,
     OUTFILE)


# Write bib file
dois <- read_csv(OUTFILE, col_names = c("study_ID", "doi")) %>%
  filter(!is.na(doi))

walk(dois$doi, ~ WriteBib(GetBibEntryWithDOI(.), BIBOUTFILE ,append = T))

bib2 <- read_csv(OUTFILE, col_names = c("study_ID", "doi")) %>%
  filter(is.na(doi)) %>%
  distinct(study_ID, .keep_all = T) %>%
  left_join(ma_citation_data) %>%
  select(1,4) %>%
  arrange(study_ID)

