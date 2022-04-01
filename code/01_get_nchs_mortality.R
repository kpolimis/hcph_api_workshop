#' get national center for health statistics mortality data
#' from 1999 to 2022 with Rsocrata
# install.packages(c("here", "yaml", "RSocrata", "tidyverse"))
here::i_am("code/01_get_nchs_mortality.R")

library(yaml)
library(plyr)
library(dplyr)
library(readr)
library(RSocrata)
library(here)

socrata_app_credentials = yaml.load_file(here("credentials", 
                                              "socrata_app_credentials.yml"))

#' Yearly Counts of Deaths by State and Select Causes, 1999-2017
#' https://data.cdc.gov/NCHS/NCHS-Leading-Causes-of-Death-United-States/bi63-dtpu
yearly_deaths_by_state_1999_2017 <- read.socrata(
  "https://data.cdc.gov/resource/bi63-dtpu.json",
  app_token = socrata_app_credentials$app_token,
  email = socrata_app_credentials$email,
  password  = socrata_app_credentials$password
)

#' Weekly Counts of Deaths by State and Select Causes, 2014-2019
#' https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr
weekly_deaths_by_state_2014_2019 <- read.socrata(
  "https://data.cdc.gov/resource/3yf8-kanr.json",
  app_token = socrata_app_credentials$app_token,
  email = socrata_app_credentials$email,
  password  = socrata_app_credentials$password
)

#' Weekly Counts of Deaths by State and Select Causes, 2020-2022
#' https://data.cdc.gov/NCHS/Weekly-Provisional-Counts-of-Deaths-by-State-and-S/muzy-jte6
weekly_deaths_by_state_2020_2022 <- read.socrata(
  "https://data.cdc.gov/resource/muzy-jte6.json",
  app_token = socrata_app_credentials$app_token,
  email = socrata_app_credentials$email,
  password  = socrata_app_credentials$password
)

# glimpse(yearly_deaths_by_state_1999_2017)
# glimpse(weekly_deaths_by_state_2014_2019)
# glimpse(weekly_deaths_by_state_2020_2022)

state.name_dc_us = c(state.name, "District of Columbia", "United States")
state.abb_dc_us = c(state.abb, "DC", "US")

#' create yearly 1999 to 2022 mortality data by combining: 
#' (1) the yearly mortality data from 1999 to 2017 
#' (2) weekly mortality data from 2014 to 2019 data 
#' (3) weekly mortality data from 2020 to 2022 data. 

#' yearly mortality data from 1999 to 2017 
yearly_deaths_by_state_1999_2017_subset = yearly_deaths_by_state_1999_2017 %>%
  rename('state_name'='state', 'all_deaths'='deaths') %>% 
  filter(cause_name=="All causes" & state_name %in% state.name_dc_us) %>% 
  select(state_name, year, all_deaths)

#' weekly mortality data from 2014 to 2019 data
weekly_deaths_2014_2022 = weekly_deaths_by_state_2014_2019 %>% 
  select(jurisdiction_of_occurrence, mmwryear, allcause, weekendingdate) %>%
  rename('state_name'='jurisdiction_of_occurrence', 'all_cause_deaths'='allcause',
         'year'='mmwryear', 'week_ending_date' = 'weekendingdate') %>% 
  mutate(week_ending_date = as.character(week_ending_date)) %>%
  rbind(weekly_deaths_by_state_2020_2022 %>%
          rename('state_name'='jurisdiction_of_occurrence', 'year'='mmwryear', 'all_cause_deaths'='all_cause') %>% 
          select(state_name, year, all_cause_deaths, week_ending_date)) %>% 
  filter(state_name %in% state.name_dc_us) %>%
  arrange(state_name, year)

#' aggregate weekly mortality data by year
#' create yearly mortality data for states and U.S. from 2018 to 2022
yearly_deaths_by_state_2018_2022 = weekly_deaths_2014_2022 %>%
  filter(year>=2018) %>% 
  group_by(state_name, year) %>% 
  mutate(all_deaths = sum(as.numeric(all_cause_deaths), na.rm = TRUE)) %>% 
  select(state_name, year, all_deaths) %>% 
  distinct() %>% 
  ungroup()

#' produce a yearly mortality dataset for states and U.S. from 1999 to 2022 by combining 
#' (1) yearly mortality dataset from 1999 to 2017 
#' (2) yearly mortality dataset from 2018 to 2022
yearly_deaths_by_state_1999_2022 = rbind(yearly_deaths_by_state_1999_2017_subset,
                                         yearly_deaths_by_state_2018_2022) %>%
  arrange(year)

#' create data directory and save yearly mortality data
ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
write_csv(yearly_deaths_by_state_1999_2022, 
          here("data", "yearly_deaths_by_state_1999_2022.csv"))
