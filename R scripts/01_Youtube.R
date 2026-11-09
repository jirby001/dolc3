## Project: Dolc3 data analysis - recording, tracking and analyzing social media and marketing data for Dolc3's success
## Date created: 260118
## Author: JCI
## Script readme: Importing and analyzing YouTube data
##    Inputs: YouTube weekly reports
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
library(patchwork)


#### LOAD DATA FROM DRIVE ####

## metadata
calendar <- read_sheet("https://docs.google.com/spreadsheets/d/1Mcsu2AJTcEQh2r0rBPlj_ZL_VzrWlqGU8ZNSjrTkplI/edit?gid=1779911818#gid=1779911818",
                       sheet = "calendar", col_types = "nDccc")

## weekly updates
week_overview<- read_sheet("https://docs.google.com/spreadsheets/d/1SG0d7Fq8SpjcCy3R1Bs1bD27lwbL-UREAUoByoq9HHc/edit?gid=990570439#gid=990570439",
                           sheet = "overview")
week_traffic <- read_sheet("https://docs.google.com/spreadsheets/d/1SG0d7Fq8SpjcCy3R1Bs1bD27lwbL-UREAUoByoq9HHc/edit?gid=990570439#gid=990570439",
                           sheet = "Traffic")
week_content <- read_sheet("https://docs.google.com/spreadsheets/d/1SG0d7Fq8SpjcCy3R1Bs1bD27lwbL-UREAUoByoq9HHc/edit?gid=990570439#gid=990570439",
                           sheet = "content")
week_traffic <- read_sheet("https://docs.google.com/spreadsheets/d/1SG0d7Fq8SpjcCy3R1Bs1bD27lwbL-UREAUoByoq9HHc/edit?gid=990570439#gid=990570439",
                                     sheet = "Traffic")
week_traffic_date <- read_sheet("https://docs.google.com/spreadsheets/d/1SG0d7Fq8SpjcCy3R1Bs1bD27lwbL-UREAUoByoq9HHc/edit?gid=990570439#gid=990570439",
                           sheet = "traffic_by_date")

# #compiled data
# youtube_new <- drive_get(as_id("https://drive.google.com/drive/u/0/folders/1ExfnFaESxWuEeHOjYOQOvVY7ybT4ZyBK"))
# youtube_glist <- drive_ls(youtube_new)
# setwd(here("Data/Youtube"))
# 
# walk(youtube_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# file_list <- list.files(recursive = FALSE, pattern = "all", full.names = TRUE)
# 
# youtube_overview <- bind_rows(lapply(file_list, read_xlsx))
# 
# youtube_content <- map_df(
#   file_list,
#   ~ read_excel(.x, sheet = "Content")
# )
# 
# 
# youtube_traffic <- map_df(
#   file_list,
#   ~ read_excel(.x, sheet = "Traffic")
# ) 
# 
# 
# setwd(here())

#### MUNGE ####

# youtube_content <- youtube_content[order(as.Date(youtube_content$Date, format="%m/%d/%Y")),]
# youtube_content <- youtube_content %>% mutate(Subscribers = cumsum(youtube_content$`Subscribers gained` - youtube_content$`Subscribers lost`))

#### PLOT ####

#weekly update
(subscribers.pl <- week_overview %>% ggplot(aes(x = Date, y = Subscribers)) +
   geom_col() + 
   theme_classic() +
   labs(title = "February Youtube New Subscribers",
        y = "Subscribers") )

ggsave(subscribers.pl, path = "Plots", file = "week_youtube_subscribers.jpeg", width = 20, height = 10, units = "in")


(impressions.pl <- week_overview %>% 
    ggplot(aes(x = Date, y = Impressions)) +
    geom_col() +
    labs(title = "February Youtube Impressions")+
    ylab("Impressions") +
    theme_classic()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 45),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))

#ggsave(impressions.pl, path = "Plots", file = "week_youtube_impressions.jpeg", width = 12, height = 20, units = "in")

(ctr.pl <- week_overview %>% ggplot(aes(x = Date, y = `Impressions click-through rate (%)`)) +
    geom_col() +
    labs(title = "February Youtube CTR ")+
    ylab("CTR % ") +
    theme_classic()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 45),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))

(imp_ctr <- impressions.pl +ctr.pl)
ggsave(imp_ctr, path = "Plots", file = "week_youtube_imp_ctr.jpeg", width = 25, height = 20, units = "in")


(viewers.pl <- week_overview %>% ggplot(aes(x = Date, y = `Unique viewers`/`Returning viewers`)) +
  geom_point() + 
    geom_line() +
    theme_classic()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 45),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))
ggsave(viewers.pl, path = "Plots", file = "week_youtube_viewer_type.jpeg", width = 12, height = 20, units = "in")


(traffic.pl <- week_traffic %>% ggplot(aes(x = `Traffic source`, y = Views))+
  geom_col() +theme_classic() +
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 75, hjust = 1),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))

ggsave(traffic.pl, path = "Plots", file = "week_youtube_traffic.jpeg", width = 16, height = 20, units = "in")

(traffic.imp.pl <- week_traffic %>% ggplot(aes(x = `Traffic source`, y = Impressions))+
    geom_col() +theme_classic() +
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 75, hjust = 1),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))

#### overview
(subscribers.pl <- youtube_content %>% ggplot(aes(x = Date, y = Subscribers)) +
   geom_line() + theme_classic() +
   labs(title = "Youtube Subscribers",
        y = "Subscribers") +
   geom_point(
     aes(xintercept = Date),
     alpha = 0.6
   ))

ggsave(subscribers.pl, path = "Plots", file = "youtube_subscribers.jpeg", width = 20, height = 10, units = "in")

(impressions.pl <- youtube_content %>% group_by(Type) %>% summarise(
   total_impressions = sum(Impressions, na.rm = TRUE),
   n_videos = n(),
   impressions_per_type = total_impressions / n_videos
 ) %>% 
   ggplot(aes(x = Type, y = impressions_per_type)) +
  geom_col() +
   labs(title = "Youtube Impressions (normalized) per video type")+
   ylab("Impressions (normalized)") +
   theme_classic()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 45),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))
 
ggsave(impressions.pl, path = "Plots", file = "youtube_impressions.jpeg", width = 12, height = 20, units = "in")

(ctr.pl <- youtube_content %>% group_by(Type) %>% summarise(
  total_crt = sum(`Impressions click-through rate (%)`, na.rm = TRUE),
  n_videos = n(),
  ctr_per_type = total_crt / n_videos
) %>% 
    ggplot(aes(x = Type, y = ctr_per_type)) +
    geom_col() +
    labs(title = "Youtube CTR % (normalized) per video type")+
    ylab("CTR % (normalized)") +
    theme_classic()+
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.y = element_text(size = 25,),
          axis.text.x = element_text(size = 25, angle = 45),
          axis.title.y = element_text(size = 25),
          axis.title.x = element_text(size = 25)))


ggsave(ctr.pl, path = "Plots", file = "youtube_ctr.jpeg", width = 12, height = 20, units = "in")


 
