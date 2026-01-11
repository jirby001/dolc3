## Project: Dolc3 data analysis - recording, tracking and analyzing social media and marketing data for Dolc3's success
## Date created: 260110
## Author: JCI
## Script readme: Importing and analyzing tiktok data
##    Inputs: Tiktok weekly reports
##    Outputs: plots of data

#### LOAD LIBRARIES ####
rm(list=ls())
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(here)
library(readxl)
library(lubridate)
library(ggplot2)

#### LOAD DATA FROM DRIVE ####

## metadata
calendar <- read_sheet("https://docs.google.com/spreadsheets/d/1Mcsu2AJTcEQh2r0rBPlj_ZL_VzrWlqGU8ZNSjrTkplI/edit?gid=1779911818#gid=1779911818",
                       sheet = "calendar", col_types = "ncccc")

calendar <- calendar %>%
  mutate(Date = mdy(paste(Date, Year)))

## weekly updates
week_overview <- read_sheet("https://docs.google.com/spreadsheets/d/1x4sEUtWDg8ERLhu43SujgmvJMzBbKm-fUkDUCnHE7EU/edit",
                   sheet = "overall", col_type = "nccnnnnnnnnnn")
week_content <- read_sheet("https://docs.google.com/spreadsheets/d/1x4sEUtWDg8ERLhu43SujgmvJMzBbKm-fUkDUCnHE7EU/edit",
                            sheet = "content", col_type = "ncccnnnnnc")
week_follower_activity <- read_sheet("https://docs.google.com/spreadsheets/d/1x4sEUtWDg8ERLhu43SujgmvJMzBbKm-fUkDUCnHE7EU/edit",
                           sheet = "follower_activity", col_type = "ncnn")

# #compiled data
tik_tok_new <- drive_get(as_id("https://drive.google.com/drive/u/0/folders/1ygFjhBgYD9TCMIEVERetMd2Ob8VSPAGo"))
tik_tok_glist <- drive_ls(tik_tok_new)
setwd(here("Data/Tiktok"))

walk(tik_tok_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
file_list <- list.files(recursive = FALSE, pattern = "all", full.names = TRUE)

tiktok_overview <- bind_rows(lapply(file_list, read_xlsx))

tiktok_content <- map_df(
  file_list,
  ~ read_excel(.x, sheet = "content")
)


tiktok_followers <- map_df(
  file_list,
  ~ read_excel(.x, sheet = "follower_activity")
) %>% filter(Year == 2026) #only data for 2026


setwd(here())
#### MUNGE ####

## week 
week_overview <- week_overview %>%
  mutate(Date = mdy(paste(Date, Year)))

week_follower_activity <- week_follower_activity %>%
  mutate(Date = mdy(paste(Date, Year)))

## overview 
overview_2025 <- tiktok_overview %>% filter(Year == 2025)
overview_2025 <- overview_2025 %>%
  mutate(Date = mdy(paste(Date, Year)))

overview_2026 <- tiktok_overview %>% filter(Year == 2026)
overview_2026 <- overview_2026 %>%
  mutate(Date = mdy(paste(Date, Year)))

#### PLOT ####

## weekly report
# profile views
(pv.week.ts.pl <- week_overview %>% ggplot(aes(x = Date, y = profile_views)) +
   geom_line() + theme_classic() +
   labs(title = "1/4 - 1/11 Tiktok profile views",
        y = "Profile Views") +
   geom_vline(
     data = calendar,
     aes(xintercept = Date, color = Where),
     alpha = 0.6
   ))
# video views 
(vv.week.ts.pl <- week_overview %>% ggplot(aes(x = Date, y = video_views)) +
    geom_line() + theme_classic() +
    labs(title = "1/4 - 1/11 Tiktok video views",
         y = "Daily video views") +
    geom_vline(
      data = calendar,
      aes(xintercept = Date, color = Where),
      alpha = 0.6
    ))

# video views to profile views
(vvpv.week.ts.pl <- week_overview %>% ggplot(aes(x = Date, y = profile_views/video_views)) +
    geom_line() + theme_classic() +
    labs(title = "1/4 - 1/11 Tiktok % of peopl checking out profile after seeing tiktok",
         y = "%") +
    geom_vline(
      data = calendar,
      aes(xintercept = Date, color = Where),
      alpha = 0.6
    ))

#followers
week_overview %>% ggplot(aes(x = Date, y = change_followers)) +
  geom_line() + theme_classic() +
  labs(title = "1/4 - 1/11 Tiktok change in followers",
       y = "change in followers") +
  geom_vline(
    data = calendar,
    aes(xintercept = Date, color = Where),
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0)

# success of content this week
(week.content.eng.bar.pl <- week_content %>% ggplot((aes(x = Date, y = `engagement index`))) +
  geom_col() + theme_classic() +
  labs(title = "1/4-1/11 Content engagement"))


# 2025 analysis
# profile views
(pv.2025.ts.pl <- overview_2025 %>% ggplot(aes(x = Date, y = profile_views)) +
  geom_line() + theme_classic() +
  labs(title = "2025 Tiktok profile views",
       y = "Profile Views") +
  geom_vline(
    data = calendar,
    aes(xintercept = Date, color = Media),
    alpha = 0.6
  ) +
  theme(plot.title = element_text(size = 40, face = "bold"),
        legend.title=element_text(size=30), 
        legend.text=element_text(size=25),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)))

ggsave(pv.2025.ts.pl, path = "Plots", file = "prof_views_ts.jpeg", width = 15, height = 10, units = "in")

# followers
overview_2025 %>% ggplot(aes(x = Date, y = change_followers)) +
  geom_line() + theme_classic() +
  labs(title = "2025 Tiktok change in followers",
       y = "change in followers") +
  geom_vline(
    data = calendar,
    aes(xintercept = Date, color = Media),
    alpha = 0.6
  ) +
  geom_hline(yintercept = 0)

# video views
(vv.ts.pl <- overview_2025 %>% ggplot(aes(x = Date, y = video_views)) +
    geom_line() + theme_classic() +
    labs(title = "2025 Tiktok video views",
         y = "Video Views") +
    geom_vline(
      data = calendar,
      aes(xintercept = Date, color = Media),
      alpha = 0.6
    ))
