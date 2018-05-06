
# MA414 Final Project: Part Two Data Visualization, League 3 Point Analysis
# NBA 3-Pointer Analysis 
# Ziran Min
# U5927447
# minziran@bu.edu
# 05/07/2018

#install.packages("tidyverse")
library(tidyverse)
#install.packages("stringr")
library(stringr)

############################################################################################
# The use of all the code in this file is for the following section in my markdown pdf report
# Section: "Three Point Shooting in the League through 15 Seasons"
# Section: "Shooting Usage Percentage in Different Distances through Seasons"

############################################################################################
# Starting by reading the file I create in the end of my first part r file
# To analyze 3 point shot on league level through years, 
# I need to sum the shooting data of all rows (teams) in same year into one.

whole_table <- read.csv(file="/Users/MZR/Desktop/Final Project Data Set/ideal_whole_data.csv")
#whole_table <- read.csv(file="ideal_whole_data.csv")

for (year in 2003:2017) {
  whole_table_some_year <- filter(whole_table, Year == year)
  whole_table_some_year_select <- select(whole_table_some_year, X3P, X3PA, X2P, X2PA,
                                         X24..ft..Atp, Less.Than.8.ft..Atp, X16.24.ft..Atp,
                                         X8.16.ft..Atp)
  csum <- colSums(whole_table_some_year_select)
  combind <- rbind(whole_table_some_year_select, csum)
  if (year == 2003) {
    x <- 30
  }
  if (year != 2003) {
    x <- 31
  }
  league_some_year_total <- combind[x,]
  three_atp_per <- data_frame(round(as.numeric(league_some_year_total[2] / (league_some_year_total[2] + league_some_year_total[4])),3))
  league_some_year_total <- cbind(league_some_year_total, three_atp_per)
  colnames(league_some_year_total)[9] <- "3 ATP Per"
  distance_atp_total <- as.numeric(league_some_year_total[5] + league_some_year_total[6] + 
                                     league_some_year_total[7] + league_some_year_total[8])
  plus_24_per <- data_frame( round(as.numeric(league_some_year_total[5]/distance_atp_total),3 ))
  league_some_year_total <- cbind(league_some_year_total, plus_24_per)
  colnames(league_some_year_total)[10] <- "24+ ft ATP Per"
  
  less_8_per <- data_frame(round(as.numeric(league_some_year_total[6]/distance_atp_total), 3))
  league_some_year_total <- cbind(league_some_year_total, less_8_per)
  colnames(league_some_year_total)[11] <- "8 less ft ATP Per"
  
  sixteen_24_per <- data_frame(round(as.numeric(league_some_year_total[7]/distance_atp_total), 3))
  league_some_year_total <- cbind(league_some_year_total, sixteen_24_per)
  colnames(league_some_year_total)[12] <- "16-24 ft ATP Per"
  
  eight_16_per <- data_frame(round(as.numeric(league_some_year_total[8]/distance_atp_total),3))
  league_some_year_total <- cbind(league_some_year_total, eight_16_per)
  colnames(league_some_year_total)[13] <- "8-16 ft ATP Per"
  
  year_name <- data_frame(year)
  league_some_year_total <- cbind(league_some_year_total, year_name)
  colnames(league_some_year_total)[14] <- "Year"
  
  if (year == 2003) {
    league_year_total <- league_some_year_total
  }
  if (year != 2003) {
    league_year_total <- rbind(league_year_total, league_some_year_total)
  }
}

colnames(league_year_total)[1] <- "3 Point Made"
colnames(league_year_total)[2] <- "3 Point Attemps"
colnames(league_year_total)[3] <- "2 Point Made"
colnames(league_year_total)[4] <- "2 Point Attemps"
colnames(league_year_total)[5] <- "24+ ft. Attemps"
colnames(league_year_total)[6] <- "less 8 ft. Attemps"
colnames(league_year_total)[7] <- "16-24 ft. Attemps"
colnames(league_year_total)[8] <- "8-16 ft. Attemps"

# Create feature that is the percentage of 3 point made over all field goal made
league_year_total$Three_Made_Per <- round(league_year_total$`3 Point Made`/(league_year_total$`3 Point Made` + league_year_total$`2 Point Made`), 3)

# Now this "league_year_total" data frame only has 15 rows. Each row represents the league data of one season. 

#########################################################################################################################

# The following is the visualization part
# Some of them are in my markdown pdf report

ggplot(league_year_total, aes(Year, `3 Point Made`)) + geom_line() +
  labs(x = "Season", y = "3 Point Shot Made",
       title = "League 3 Point Made through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) 

ggplot(league_year_total, aes(Year, `3 Point Attemps`)) + geom_line() +
  labs(x = "Season", y = "3 Point Shot Attempt",
       title = "League 3 Point Attempt through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) 

ggplot(league_year_total, aes(Year)) +
  geom_line(aes(y = `3 Point Made`, col = "3 Point Made")) +
  geom_line(aes(y = `3 Point Attemps`, col = "3 Point Attempt")) +
  scale_colour_manual(values=c("skyblue", "blue"))+
  labs(x = "Season", y = "3 Point Shot",
       title = "League 3 Point Made and Attempt through Seasons") +
  scale_x_continuous(breaks = c(2003:2017))

ggplot(league_year_total, aes(Year)) +
  geom_line(aes(y = `2 Point Made`, col = "2 Point Made")) +
  geom_line(aes(y = `2 Point Attemps`, col = "2 Point Attemps")) +
  scale_colour_manual(values=c("orange", "red"))+
  labs(x = "Season", y = "2 Point Shot",
       title = "League 2 Point Made and Attempt through Seasons") +
  scale_x_continuous(breaks = c(2003:2017))

ggplot(league_year_total, aes(Year, `3 ATP Per`)) + geom_line() +
  labs(x = "Season", y = "3 Point Shot Usage %",
       title = "League 3 Point Shot Usage % through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year, `Three_Made_Per`)) + geom_line() +
  labs(x = "Season", y = "3 Point Field Goal over All Field Goal",
       title = "League 3 Point Field Goal over All Field Goal through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year)) +
  geom_line(aes(y = `3 ATP Per`, col = "3P Attempt Usage %")) +
  geom_line(aes(y = `Three_Made_Per`, col = "3P FG Made over All FG")) +
  scale_colour_manual(values=c("orange", "red"))+
  labs(x = "Season", y = "Percentage",
       title = "League 3 Point Made and Attempt Percentage through Seasons") +
  scale_x_continuous(breaks = c(2003:2017))

ggplot(league_year_total, aes(Year, `24+ ft ATP Per`)) + geom_line() +
  labs(x = "Season", y = "24+ ft. Shot Usage %",
       title = "League 24+ ft. Shot Usage % for Different Distances through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year, `8 less ft ATP Per`)) + geom_line() +
  labs(x = "Season", y = "8 less ft.Shot Usage %",
       title = "League 8 less ft. Shot Usage % for Different Distances through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year, `16-24 ft ATP Per`)) + geom_line() +
  labs(x = "Season", y = "16-24 ft. Shot Usage %",
       title = "League 16-24 ft. Shot Usage % for Different Distances through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year, `8-16 ft ATP Per`)) + geom_line() +
  labs(x = "Season", y = "8-16 ft. Shot Usage %",
       title = "League 8-16 ft. Shot Usage % for Different Distances through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(labels = scales::percent)

ggplot(league_year_total, aes(Year)) + 
  geom_line(aes(y = `24+ ft ATP Per`, col = "24+ ft. Shot Usage %")) + 
  geom_line(aes(y = `8 less ft ATP Per`, col = "8 less ft. Shot Usage %")) +
  geom_line(aes(y = `16-24 ft ATP Per`, col = "16-24 ft. Shot Usage %")) +
  geom_line(aes(y = `8-16 ft ATP Per`, col = "8-16 ft. Shot Usage %")) +
  scale_colour_manual(values=c("green", "red", "blue", "orange")) +
  labs(x = "Season", y = "Shot Usage %",
       title = "League Shot Usage % for Different Distances through Seasons") +
  scale_x_continuous(breaks = c(2003:2017)) + 
  scale_y_continuous(breaks = c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45), labels = scales::percent)
   

















