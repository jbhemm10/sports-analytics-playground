#Open libraries
library(tidyverse)
library(Lahman)
library(ggrepel)
library(ggplot2)

#Join starting pitcher data and people data
sp <- Pitching %>%
  inner_join(People, by = c("playerID" = "playerID")) %>%
  select(playerID, yearID, nameFirst, nameLast, teamID, throws, W, L, GS, CG, IPouts, SO, HBP, BB)

View(sp)
#Pull Starting pitcher data for pitchers from since 2020
sp_2020s <- sp %>%
  filter(yearID >= 2020) %>%
  filter(GS > 0)

#Pull starting pitcher data from 2010 to 2019
sp_2010s <- sp %>%
  filter(yearID >= 2010 & yearID < 2020) %>%
  filter(GS > 0)

#Pull starting pitcher data from 2000 to 2009
sp_2000s <- sp %>%
  filter(yearID >= 2000 & yearID < 2010) %>%
  filter(GS > 0)

#View starting pitcher data
View(sp_2020s)
View(sp_2010s)
View(sp_2000s)

