# Examining injury and mortality 
# rene dario 
# my email 
# 31 July 2021 

# set up 
# packages 
library(here)
library(RSocrata)
library(tidyverse)
library(janitor)

# read data 
nchs_injury_mortality <- read.socrata("https://data.cdc.gov/resource/vc9m-u7tv.json")

# change from character to numeric 
nchs_injury_mortality <- nchs_injury_mortality %>%
  mutate(
    deaths = as.numeric(deaths),
    population = as.numeric(population),
    age_specific_rate = as.numeric(age_specific_rate),
    age_adjusted_rate = as.numeric(age_adjusted_rate)
  )

# inspect
glimpse(nchs_injury_mortality)
str(nchs_injury_mortality)
class(nchs_injury_mortality)

nchs_injury_mortality <- as_tibble(nchs_injury_mortality)

# year 
nchs_injury_mortality %>%
  distinct(year)

# sex 
nchs_injury_mortality %>%
  distinct(sex)

# which age groups 
nchs_injury_mortality %>%
  distinct(age_group_years)

# race
nchs_injury_mortality %>%
  distinct(race)

# injury mechanism 
nchs_injury_mortality %>%
  distinct(injury_mechanism)

# injury intent 
nchs_injury_mortality %>%
  distinct(injury_intent)

nchs_injury_mortality <- nchs_injury_mortality %>%
  mutate(suicide = if_else(injury_intent == "Suicide", 1, 0))

# suicide?
nchs_injury_mortality %>%
  distinct(suicide)

summary(nchs_injury_mortality$suicide)

# deaths
summary(nchs_injury_mortality$deaths)

# number of suicide deaths 
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years == "All Ages",
         race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = deaths)) +
  ylim(c(0,46000))

# number of suicide deaths per population * 100000
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years == "All Ages",
         race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = 100000*(deaths/population))) +
  ylim(c(0,15))

# age adjusted rate of suicide deaths
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years == "All Ages",
         race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = age_adjusted_rate)) +
  ylim(c(0,15))

# age adjusted rate of suicide deaths by sex
nchs_injury_mortality %>%
  filter(suicide == 1,
         #sex == "Both sexes",
         age_group_years == "All Ages",
         race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = age_adjusted_rate, color = sex)) +
  ylim(c(0,25))

# age adjusted rate of suicide deaths by age group
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years != "All Ages",
         race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = age_specific_rate, color = age_group_years)) +
  ylim(c(0,20))

# age adjusted rate of suicide deaths by race
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years == "All Ages",
         # race == "All races",
         injury_mechanism == "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = age_adjusted_rate, color = race)) +
  ylim(c(0,20))

# age adjusted rate of suicide deaths by injury mechanism
nchs_injury_mortality %>%
  filter(suicide == 1,
         sex == "Both sexes",
         age_group_years == "All Ages",
         race == "All races",
         injury_mechanism != "All Mechanisms") %>%
  ggplot() +
  geom_point(mapping = aes(x = year, y = age_adjusted_rate, color = injury_mechanism)) +
  ylim(c(0,8))

summary(nchs_injury_mortality$population)

summary(nchs_injury_mortality$age_specific_rate)

summary(nchs_injury_mortality$age_adjusted_rate)
