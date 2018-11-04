```{r, eval = F, include = F}
d <- read_csv("../../../exp1/raw/Fendle\ Subject\ Log.csv") %>%
  filter(`Exclusion codes` != "P" | is.na(`Exclusion codes`)) %>%
  mutate(SID = ifelse(substr(SID, 1, 1) == "0", # get rid of leading 0s so can join with demographics 
                      substring(SID, 2, last = 1000000L), SID))

age_m <- exp1 %>%
  distinct(sub_id, .keep_all = T) %>%
  filter(age_months < 24 | age_months > 48) %>%
  select(sub_id) %>%
  arrange(sub_id) %>%
  pull() # matches below

age_v <- d %>%
  filter(`Exclusion codes` %in% c("AGE", "DF/AGE", "NE/AGE", "DF/NE/AGE")) %>%
  select(SID, age) %>%
  mutate(age_months = age*12) %>%
  arrange(SID) %>%
  pull()

setdiff(age_v, age_m)
setdiff(age_m, age_v) # "2071706" "2201710"

good_counts <- exp1 %>%
  count(sub_id) %>%
  filter(n == 19)

df_m <- exp1 %>%
  count(sub_id) %>%
  filter(n < NUM_TRIALS)  %>%
  select(sub_id) %>%
  arrange(sub_id) %>%
  pull()

df_v <- d %>%
  filter(`Exclusion codes` %in% c("DF", "DF/AGE", "DF/NE/AGE", "DF/NE")) %>%
  select(SID) %>%
  arrange(SID) %>%
  pull()

setdiff(df_v, df_m) # "1031705" (don't have this one - no data)
setdiff(df_m, df_v) #"11161604" (AGE), "1121707" (NE/AGE)  "1171704" (NA) , "12131602" "1261606"  (AGE) "1311701"  (NE) "4181701"  (NE) "5241704" (NE/AGE) "6201706" (NE) "6221701"  (NA)

ne_m <- exp1 %>%
  distinct(sub_id, .keep_all = T) %>%
  filter(english < 75) %>%
  select(sub_id) %>%
  arrange(sub_id) %>%
  pull()

ne_v <- d %>%
  filter(`Exclusion codes` %in% c("NE", "NE/AGE", "DF/NE", "DF/NE/AGE")) %>%
  select(SID) %>%
  arrange(SID) %>%
  pull()


setdiff(ne_v, ne_m) # "6201705" # say 100 english
setdiff(ne_m, ne_v) # "1061701" "1061703" "1191606" "1271701" "2201708" "2201709" "2201712" "2281703" "3021701" "3211701" "6021701" "6201707" # all these kids didnt' finish
```
