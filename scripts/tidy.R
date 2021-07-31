# Examining injury and mortality 
# rene dario 
# my email 
# 31 July 2021 

library(here)
library(RSocrata)
library(tidyverse)
library(janitor)

nchs_injury_mortality <- read.socrata("https://data.cdc.gov/resource/vc9m-u7tv.json")

nchs_injury_mortality <- nchs_injury_mortality %>%
  mutate(
    deaths = as.numeric(deaths),
    population = as.numeric(population),
    age_specific_rate = as.numeric(age_specific_rate),
    age_adjusted_rate = as.numeric(age_adjusted_rate)
  )

glimpse(nchs_injury_mortality)

nchs_injury_mortality %>%
  distinct(year)

nchs_injury_mortality %>%
  distinct(sex)

nchs_injury_mortality %>%
  distinct(age_group_years)

nchs_injury_mortality %>%
  distinct(race)

nchs_injury_mortality %>%
  distinct(injury_mechanism)

nchs_injury_mortality %>%
  distinct(injury_intent)

summary(nchs_injury_mortality$deaths)

summary(nchs_injury_mortality$population)

summary(nchs_injury_mortality$age_specific_rate)

nchs_injury_mortality %>%
  summary(age_adjusted_rate)
