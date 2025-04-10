#Install packages
#install.packages(readxl)
#install.packages(readcsv)

#Load libraries
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(readxl)
library(readcsv)

#Load data
pitching_data <- read_csv("Retrosheet_Data/pitching.csv")

#Summary of pitching data
summary(pitching_data)

#Collect data where the pitcher is a starter
sp <- pitching_data %>%
  filter(p_gs == 1)

View(sp)

#Only keep starting pitchers after 1960
sp <- sp %>%
  filter(date >19600000)

View(sp)

#Pull starting pitchers from 2020s
sp_2020 <- sp %>%
  filter(date >= 20200000)

#Pull starting pitchers from 2010s
sp_2010 <- sp %>%
  filter(date >= 20100000 & date < 20200000)

#Pull starting pitchers from 2000s
sp_2000 <- sp %>%
  filter(date >= 20000000 & date < 20100000)
