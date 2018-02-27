# Merge pre-proccessed Exp-1 data (disambiguation, vocab, and demographics)
# saves file with all complete data

library(tidyverse)

#### MERGE DEMOGRAPHICS AND VOCAB DATA BY PARTICIPANT #####
# which participants to exclude? we have 226 with complete data. 

# demographics
d <- read_csv("processed/exp_1_processed_demographic.csv") 
d_clean <- d %>%
           mutate(sub_id = ifelse(sub_id_issue, sub_id_replacement, sub_id),
                  gender = fct_recode(gender, female = "F", male = "M"),
                  age_months = Age * 12) %>%
           distinct(sub_id, .keep_all = TRUE)  %>%
           rename(exclude2 = `Exclude (Tammy added based on notes- Veronica, please check)`) %>%
           mutate_if(is.character, as.factor) %>%
           select(sub_id, age_months, gender, english, exclude2) # these are the only demos we care about

# vocab
v <- read_csv("processed/exp_1_processed_vocab.csv") 
v_clean <- v %>%
  mutate(sub_id = replace(sub_id, sub_id == "01051704", "sub_r2"), # this subj doesn't exist
         sub_id = replace(sub_id, sub_id %in% c("4111702","04111702"), "sub_r4"),
         sub_id = replace(sub_id, sub_id == "02101702", "sub_r3"),
         sub_id = ifelse(substr(sub_id, 1, 1) == "0", # get rid of leading 0s so can join with demographics 
                         substring(sub_id, 2, last = 1000000L), sub_id))

# get proportion correct by participant
v_clean_prop <- v_clean %>%
  group_by(sub_id) %>%
  summarize(prop_correct_vocab = sum(Correct == "Yes")/n()) 

dem_with_vocab <- inner_join(d_clean, v_clean_prop)

### MERGE DISAMBIGUATION TASK DATA ###
e <- read_csv("processed/exp_1_processed_events.csv")
e_clean <- e %>%
  mutate(sub_id = replace(sub_id, sub_id == "01051704", "sub_r1"), # this subj doesn't exist
         sub_id = replace(sub_id, sub_id %in% c("4111702","04111702"), "sub_r4"),
         sub_id = replace(sub_id, sub_id == "01051703", "sub_r2"),
         sub_id = replace(sub_id, sub_id == "02101701", "sub_r3"),
         sub_id = ifelse(substr(sub_id, 1, 1) == "0", # get rid of leading 0s so can join with demographics 
                  substring(sub_id, 2, last = 1000000L), sub_id),
         correct = ifelse(correct == "Y", 1, 0))  %>%
  filter(!is.na(trial_type))
  
all_data <- left_join(dem_with_vocab, e_clean) %>%
  mutate_if(is.character, as.factor) 
  
# write to csv
write_csv(all_data, "processed/all_exp1_data_complete.csv")
