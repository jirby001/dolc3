## Project: Dolc3 data analysis - recording, tracking and analyzing social media and marketing data for Dolc3's success
## Date created: 261022
## Author: JCI
## Script readme: Importing and analyzing insta data
##    Inputs: Instagram csv output from Meta Business Suite
##    Outputs:

#### Load Libraries ####
library(here)
library(tidyverse)
library(googlesheets4)
library(lubridate)

##### Retrieve data ####
setwd(here("Data/Insta"))
file_list <- list.files(recursive = FALSE, pattern = "\\.csv$", full.names = TRUE)
insta_all <- bind_rows(lapply(file_list, read.csv))
setwd(here())

## metadata
calendar <- read_sheet("https://docs.google.com/spreadsheets/d/1Mcsu2AJTcEQh2r0rBPlj_ZL_VzrWlqGU8ZNSjrTkplI/edit?gid=1779911818#gid=1779911818",
                       sheet = "calendar", col_types = "ncccc")
## TODO: update calendar
## Cleaning
insta_all <- insta_all %>% filter(Account.name == "DOLC3") %>% select(-Date)

insta_all$Publish.time <- mdy_hm(insta_all$Publish.time)
insta_all$Publish.time <-force_tz(insta_all$Publish.time, "MST")




