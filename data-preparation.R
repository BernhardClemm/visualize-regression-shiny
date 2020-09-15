############################
### Prepare BES data set ###
############################

library(magrittr)
library(tidyr)
library(dplyr)
library(foreign)

# Read in data

data <- read.dta("./bes_f2f_2017_v1.3.dta")

# Recode data
##Check: https://www.britishelectionstudy.com/wp-content/uploads/2019/01/BES-2017-F2F-codebook.pdf

data %<>% mutate(political_interest = case_when(a03 == "Not at all interested" ~ 1,
                                                a03 == "Not very interested" ~ 2,
                                                a03 == "Fairly interested" ~ 3,
                                                a03 == "Very interested" ~ 4)) %>%
  mutate(political_trust = case_when(n03 == "0 No trust" ~ 0,
                                     n03 == "1" ~ 1,
                                     n03 == "2" ~ 2,
                                     n03 == "3" ~ 3,
                                     n03 == "4" ~ 4,
                                     n03 == "5" ~ 5,
                                     n03 == "6" ~ 6,
                                     n03 == "7" ~ 7,
                                     n03 == "8" ~ 8,
                                     n03 == "9" ~ 9,
                                     n03 == "10 A great deal of trust" ~ 10)) %>%
  # mutate(read_newspapers = factor(k02, ordered = TRUE, levels = c("No", "Yes"))) %>%
  mutate(read_newspapers = case_when(k02 == "No" ~ 1,
                                     k02 == "Yes" ~ 2)) %>%
  mutate(canvassed = case_when(k11 == "No" ~ 1,
                               k11 == "Yes" ~ 2)) %>%
  mutate(education_level = case_when(edlevel == "No qualifications" ~ 1,
                                     edlevel == "Below GCSE" ~ 2,
                                     edlevel == "GCSE" ~ 3,
                                     edlevel == "A-level" ~ 4,
                                     edlevel == "Undergraduate" ~ 5,
                                     edlevel == "Postgrad" ~ 6)) %>%
  mutate(age = as.numeric(Age)) %>%
  mutate(age = ifelse(age == -2, NA, age)) %>%
  mutate(ideology = as.character(e01)) %>%
  mutate(ideology = case_when(ideology == "Not stated" |
                                ideology == "Refused" |
                                ideology == "Don`t know" ~ NA_character_,
                              ideology == "0 Left"  ~ "0",
                              ideology == "10 Right" ~ "10",
                              TRUE ~ as.character(ideology))) %>%
  mutate(ideology = as.numeric(as.character(ideology))) %>%
  select(political_interest, political_trust, read_newspapers,
         canvassed, education_level, age, ideology)

# Save data

write.csv(data, "./bes.csv", row.names = F)
