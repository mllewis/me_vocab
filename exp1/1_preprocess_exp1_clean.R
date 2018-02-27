# Pre-process raw Exp-1 data (disambiguation, vocab, and demographics)

library(tidyverse)
library(lubridate)
library(stringr)

### disambiguation task data ###

# read in data
e1 <- read_csv("raw/fendle-12-9-16.csv") #7097
e2 <- read_csv("raw/fendle-07-12-17.csv") # 78576 
e3 <- read_csv("raw/fendle-06-12-17.csv") #61599 

events <- list(e1, e2, e3) %>%
  bind_rows() %>%
  distinct()

# helper
parse_params <- function(x) {
  json <- str_sub(x$Params, 3, -2)
  splits <- str_split(json, pattern = ";..")[[1]] 
  
  dx <- splits %>%
    map_df(function(y) {
      ys <- str_split(y, pattern = " : ")[[1]]
      return(data_frame(key = ys[1], value = ys[2]))
    }) %>%
    spread(key, value) %>%
    mutate(event = x$Event,
           version = x$Version)
  
  return(dx)
}

# parse
d_parsed <- events %>% 
  mutate(event_num = 1:n()) %>%
  split(.$event_num) %>%
  map_df(parse_params)

# tidy 
d <- d_parsed %>%
  rename(sub_id = `Subject ID`,
         user_id = `User ID`,
         end_time = `End Time`,
         start_time = `Start Time`,
         resp_start_time = `Responsive Start Time`,
         stage_id = `Stage ID`,
         stage_type = `Stage Type`,
         correct = `Correct First Try?`,
         help = `Fox Assistance`,
         object = Object,
         category = Category,
         object1 = `Object`,
         time_start = `Time Start`,
         type = Type, 
         trial_type = `Trial Type`) %>%
  select(-user_id, -stage_id) %>%
  mutate(start_time = parse_date_time(str_sub(start_time, 1, -7), "db HMS"),
         end_time = parse_date_time(str_sub(end_time, 1, -7), "db HMS"),
         resp_start_time = parse_date_time(str_sub(resp_start_time, 1, -7), "db HMS")) 
# write to csv
write_csv(d, "processed/exp_1_processed_events.csv")
 
### vocabulary data ###
# (this data seems to have been pre-procssed- this is the rawest form of the data I can find)
# read in data
v1 <- read_csv("raw/vocab-device-results-06-12-17.csv") 
v2 <- read_csv("raw/vocab-device-results-07-12-17.csv") 
v3 <- read_csv("raw/vocab-processed-12-9-16.csv")

vocab <- list(v1, v2, v3) %>%
  bind_rows() %>%
  distinct() %>%
  rename(sub_id = SubjectID)

# write to csv
write_csv(vocab, "processed/exp_1_processed_vocab.csv")

### demographic data ###
# read in data
a1 <- read_csv("raw/ages-06-12-17.csv") 
a2 <- read_csv("raw/ages-07-12-17.csv") 
a3 <- read_csv("raw/ages-12-9-16.csv") %>% 
  mutate(SubjectID = as.character(SubjectID))

demographic <- list(a1, a2, a3) %>%
  bind_rows() %>%
  distinct() %>%
  rename(sub_id = SubjectID)

# write to csv
write_csv(demographic, "processed/exp_1_processed_demographic.csv")
