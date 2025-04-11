#Install packages
#install.packages(readxl)


#Load libraries
library(tidyverse)
library(ggrepel)
library(ggplot2)
library(readxl)
library(readr)


#Set working directory
setwd("~/Sports Analytics Playground/sports-analytics-playground")

#Load data
pitching_data <- read_csv("Retrosheet_Data/pitching.csv")
player_info <- read_csv("Retrosheet_Data/allplayers.csv")

#Summary of data
summary(pitching_data)
summary(player_info)

#Collect data where the pitcher is a starter
sp <- pitching_data %>%
  filter(p_gs == 1)

#Only keep starting pitchers after 1960
sp <- sp %>%
  filter(date >19600000)

#Change some column names so it can merge files
sp <- sp %>% 
  rename(p_team = team, p_opp = opp)

#Remove additional rows in player info data
player_info_clean <- player_info %>% 
  distinct(id, .keep_all = TRUE)


#Add throwing hand from player_info to sp
sp <- sp %>%
  left_join(player_info_clean, by = c("id" = "id"), relationship = "many-to-many") %>%
  select(gid, id, p_team, last, first, throw, p_ipouts, p_bfp, p_h, p_hr, p_r, p_er, p_w,
         p_k, win, loss, tie, p_gs, p_cg, p_opp, date)

#Replace NaN with 0
sp <- sp %>% 
  mutate(across(everything(), ~replace(., is.na(.), 0)))

#Convert outs to innings pitched
outs_to_ip <- function(p_ipouts) {
  innings <- sp$p_ipouts %/% 3
  remainder <- sp$p_ipouts %% 3
  innings + remainder /10
}

#Add created metrics to sp
sp <- sp %>% 
  mutate(p_ip = outs_to_ip(p_ipouts))

#Double check sp is formatted correctly
summary(sp)
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

#View various years
View(sp_2020)
View(sp_2010)
View(sp_2000)

#Find max value in each decade
max(sp_2020$p_ip, na.rm = FALSE)
max(sp_2010$p_ip, na.rm = FALSE)
max(sp_2010$p_ip, na.rm = FALSE)

#Plot histogram of outs pitched for each decade
ggplot(data = sp_2020, aes(p_ip)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10))+
  stat_bin(aes(y=..count.. + 300, label=..count..), geom="text", binwidth=1, color = "black")+
  labs( x = "Innings Pitched", y = "Frequency")

ggplot(data = sp_2010, aes(p_ip)) + 
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10))+
  stat_bin(aes(y=..count.. + 700, label=..count..), geom="text", binwidth=1, color = "black")+
  labs(x = "Innings Pitched", y = "Frequency")

ggplot(data = sp_2000, aes(p_ip)) + 
  geom_histogram(binwidth =1, fill = "purple", color = "black") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10))+
  stat_bin(aes(y=..count.. + 700, label=..count..), geom="text", binwidth=1, color = "black")+
  labs(x = "Innings Pitched", y = "Frequency")

#Find correlation between innings pitched and wins for each year
cor(sp_2020$win, sp_2020$p_ip)
cor(sp_2010$win, sp_2010$p_ip)
cor(sp_2000$win, sp_2000$p_ip)

#Histogram of wins based on number of innings pitched
sp_2020$p_ipf <- as.factor(sp_2020$p_ip)
ggplot(data = sp_2020, aes(x = p_ipf, y = sum(win)))+
  geom_bar(stat = "identity", color = "skyblue") +
  labs(title = "Wins by Innings Pitched", x = "Innings Pitched", y = "Number of Wins")

sp_2010$p_ipf <- as.factor(sp_2010$p_ip)
ggplot(data = sp_2010, aes(x = p_ipf, y = sum(win)))+
  geom_bar(stat = "identity", color = "orange") +
  labs(title = "Wins by Innings Pitched", x = "Innings Pitched", y = "Number of Wins") +
  theme_minimal()

sp_2000$p_ipf <- as.factor(sp_2000$p_ip)
ggplot(data = sp_2000, aes(x = p_ipf, y = sum(win)))+
  geom_bar(stat = "identity", color = "purple") +
  labs(title = "Wins by Innings Pitched", x = "Innings Pitched", y = "Number of Wins") +
  theme_minimal()
