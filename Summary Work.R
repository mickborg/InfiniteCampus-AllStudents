setwd("C:/Users/mebor003/OneDrive - Minneapolis Public Schools/Documents/GitHub/InfiniteCampus-AllStudents")

library(tidyverse)

df <- read.csv("10082024.studentlistingtest.csv")

school_summary <- df %>% mutate(school = paste(sch_no, sch_na))

school_summary <- distinct(school_summary,school,sch_config,sch_type)

ohe <- df %>% mutate(
  school                             = paste(sch_no, sch_na),
  
  gender_M_flag                      = ifelse(gender == "M", 1, 0),
  gender_F_flag                      = ifelse(gender == "F", 1, 0),
  gender_O_flag                      = ifelse(gender == "O", 1, 0),
  gender_X_flag                      = ifelse(gender == "X", 1, 0), 
  
  eth_african_american_flag          = ifelse(ethnicity == "African American", 1, 0),
  eth_american_indian_flag           = ifelse(ethnicity == "American Indian", 1, 0),
  eth_asian_flag                     = ifelse(ethnicity == "Asian", 1, 0),
  eth_hawaiian_pacific_islander_flag = ifelse(ethnicity == "Hawaiian/Pacific Islander", 1, 0),
  eth_hispanic_flag                  = ifelse(ethnicity == "Hispanic", 1, 0),
  eth_two_or_more_flag               = ifelse(ethnicity == "Two or More", 1, 0),
  eth_white_flag                     = ifelse(ethnicity == "White", 1, 0),
  
  home_lang_english_flag             = ifelse(home_lang_cat == "English", 1, 0),
  home_lang_hmong_flag               = ifelse(home_lang_cat == " Hmong", 1, 0),
  home_lang_other_flag               = ifelse(home_lang_cat == "Other", 1, 0),
  home_lang_somali_flag              = ifelse(home_lang_cat == "Somali", 1, 0),
  home_lang_spanish_flag             = ifelse(home_lang_cat == "Spanish", 1, 0)
)

by_school <- ohe %>%  group_by(school)

summary <- by_school %>% summarise(
  n                             = n(), 
  frl                           = sum(frl_flag) / n, 
  hhm                           = sum(hhm_flag) / n, 
  sped                          = sum(sped_flag) / n,
  
  gender_F                      = sum(gender_F_flag) / n,
  gender_O                      = sum(gender_O_flag) / n,
  gender_M                      = sum(gender_M_flag) / n,
  gender_X                      = sum(gender_X_flag) / n,
  
  eth_african_american          = sum(eth_african_american_flag) / n,
  eth_american_indian           = sum(eth_american_indian_flag) / n,
  eth_asian                     = sum(eth_asian_flag) / n,
  eth_hawaiian_pacific_islander = sum(eth_hawaiian_pacific_islander_flag) / n,
  eth_hispanic                  = sum(eth_hispanic_flag) / n,
  eth_two_or_more               = sum(eth_two_or_more_flag) / n,
  eth_white                     = sum(eth_white_flag) / n,
  
  home_lang_english             = sum(home_lang_english_flag)/n,
  home_lang_hmong               = sum(home_lang_hmong_flag)/n,
  home_lang_other               = sum(home_lang_other_flag)/n,
  home_lang_somali              = sum(home_lang_somali_flag)/n,
  home_lang_spanish             = sum(home_lang_spanish_flag)/n
  ) 

summary <- summary %>% mutate_if(is.numeric, round, digits = 2)

joined_set = left_join(school_summary, summary)

write.csv(summary,"10082024.studentlistingtest.summary.csv")
