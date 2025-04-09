#Install packages
#install.packages("Lahman")
#install.packages("ggrepel")

#Download libraries
library(tidyverse)
library(Lahman)
library(ggrepel)

#Look at team data since 2000
teams <- Teams

#Load batting data
batting <- Batting

#Create histogram of team's run scored
ggplot(data = teams, aes(R)) +
  geom_histogram()


