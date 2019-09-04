## mervis and bertrand 1994b

n1 = 16
n2 = 16
m1 = .9
m2 = .11
sd1 = .13
sd2 = .13

sampled_df <- data.frame(sample_id = 1:10000) %>%
  rowwise() %>%
  mutate(simulated_data = list(c(rnorm(n1, m1, sd1), rnorm(n2, m2, sd2))),
         mean = mean(simulated_data),
         sd = sd(simulated_data))

mean(sampled_df$mean)
mean(sampled_df$sd)

  
## Preissler and carey 2005

estimate_descriptives <- function(sample, t1, t2){
  simulated_data <- data.frame(sampled_trial1 = sample(t1),
                               sampled_trial2 = sample(t2)) %>%
    rowwise() %>%
    mutate(mean = mean(c(sampled_trial1, sampled_trial2), na.rm = T)) 
  
  data.frame(sample_id = sample,
             overall_mean = mean(simulated_data$mean),
             overall_sd = sd(simulated_data$mean))
  
}

# autism data
trial1 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0)
trial2 = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,NA)

sampled_data <- map_df(1:10000, estimate_descriptives, trial1, trial2) 

mean(sampled_data$overall_mean) #0.8186575
mean(sampled_data$overall_sd) #0.2817753


## Lederberg data
library(foreign)

dataset = read.spss("/Users/mollylewis/Downloads/LongN3CDRpassfailT1-4.sav", to.data.frame=TRUE)

lbrg2008 <- read.spss("/Users/mollylewis/Downloads/n3ct1allsubj.sav", to.data.frame=TRUE)  %>%
  janitor::clean_names() %>%
  filter(!is.na(drt1nc)) %>%
  mutate(age_years  = floor(aget1/12)) %>% # this matches the data in table 2
  select(subjno, aget1, cdit1, age_years, n3ct1nc, n3ct1pf) %>%
  rename(age_months = aget1,
         me_n_correct = n3ct1nc,
         me_pass_fail= n3ct1pf)

lbrg2008 %>%
  mutate(prop_correct = me_n_correct/4) %>%
  group_by(age_years) %>%
  summarize(mean_age_day.99s = mean(age_months) * 30.42,
            mean_cdi = mean(cdit1),
            n = n(),
            mean_prop_correct = mean(prop_correct),
            sd_prop_correct = sd(prop_correct))

# Scofield 2007 SD calculation
scofield2007_data <- c(rep(1,41), rep(0, 8))

sd(scofield2007_data) %>%
  round(2)

# Horst 2008, Exp 1b
horst2008_data1b <- c(rep(1,22), rep(0, 10))
sd(horst2008_data1b) %>%
  round(2)


# Horst 2008, Exp 1c
horst2008_data1c <- c(rep(1,12), rep(0, 8))
sd(horst2008_data1c) %>%
  round(2)