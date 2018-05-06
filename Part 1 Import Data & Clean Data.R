
# MA414 Final Project: Part One Importing Data, Cleaning Data, and Creating Ideal Data Sets
# NBA 3-Pointer Analysis 
# Ziran Min
# U5927447
# minziran@bu.edu
# 05/07/2018

#install.packages("httr")
library(httr)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("ballr")
library(ballr)
#install.packages("stringr")
library(stringr)
#install.packages("rjson")
library(rjson)
#################################################################################################
# Find the official NBA player ID number of every NBA athlete 
# Create table that stores player name and player ID

playerID_url = "http://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2017-18&IsOnlyCurrentSeason=0"
playerID <- fromJSON(file = playerID_url, method="C")
playerID_list <- unlist(playerID$resultSets[[1]][[3]])
# there is a null element in the JSON data which will be ignore during unlising, 
# so I need to add a empty element in its original place. 
playerID_list_modified <- append(playerID_list, '', 35548)
playerID_whole <- as_tibble(matrix(playerID_list_modified, ncol=13, byrow = TRUE))
colnames(playerID_whole) <- playerID$resultSets[[1]][[2]]
View(playerID_whole)
playerID_name <- select(playerID_whole, PERSON_ID, DISPLAY_FIRST_LAST,FROM_YEAR,TO_YEAR)
View(playerID_name)
playerID_name$FROM_YEAR <- as.numeric(playerID_name$FROM_YEAR)
playerID_name$TO_YEAR <- as.numeric(playerID_name$TO_YEAR)

write.csv(playerID_name, file = "/Users/MZR/Desktop/Final Project Data Set/playerID_name.csv", row.names = FALSE)
#write.csv(playerID_name, file = "playerID_name.csv", row.names = FALSE)

#################################################################################################
# Practice
# Single Player Curry example
# Shot data for Stephen Curry in 2017-2018 Season
# This Curry's player ID
playerID <- 201939
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2017-18&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&PlayerPosition=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
shotData <- fromJSON(file = shotURL, method="C")
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))

universe_shoot_colnames <- shotData$resultSets[[1]][[2]]

colnames(shotDataf) <- universe_shoot_colnames
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))

#View(shotDataf)

#################################################################################################
# Practice
# Shooting information table
# Getting the shooting info of every player in every game in 2017-2018 season
playerID_name_2017 <- filter(playerID_name, FROM_YEAR <= 2017, TO_YEAR >= 2017)
id_list <- list()
for (i in playerID_name_2017$PERSON_ID) {
  id_list <- append(id_list, i)
}

total <- list()
for (i in id_list) {
  URL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2017-18&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&PlayerPosition=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",i,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2017-18&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
  JsonData <- fromJSON(file = URL, method="C")
  shoot_info <- unlist(JsonData$resultSets[[1]][[3]])
  if (!is.null(shoot_info)) {
  current_player <- matrix(shoot_info, ncol=24, byrow = TRUE)
  total <- rbind(total, current_player)
  }
}

dataframe_2017 <- data.frame(total)
colnames(dataframe_2017) <- universe_shoot_colnames
dataframe_2017$LOC_X <- as.numeric(as.character(dataframe_2017$LOC_X))
dataframe_2017$LOC_Y <- as.numeric(as.character(dataframe_2017$LOC_Y))
dataframe_2017$SHOT_DISTANCE <- as.numeric(as.character(dataframe_2017$SHOT_DISTANCE))
whole_info_2017 <- as_tibble(dataframe_2017)

################################################################################################
# Actual Data Collecting 
# Generate every year's (2000-2018) shoot info of every player
# Create 15 csv files of 15 seasons and download for future locally use 
create_one_year_whole_nocolname <- function(year) {
  playerID_name_some_year <- filter(playerID_name, FROM_YEAR <= year, TO_YEAR >= year)
  id_list <- list()
  for (i in playerID_name_some_year$PERSON_ID) {
    id_list <- append(id_list, i)
  }
  total <- list()
  for (j in id_list) {
    
    first_year_str <- toString(year)
    second_year_num <- year + 1
    second_year_str <- substr(second_year_num, 3, 4)
    
    URL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=", first_year_str, "-", second_year_str, "&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&PlayerPosition=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",j,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=", first_year_str, "-", second_year_str, "&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
    JsonData <- fromJSON(file = URL, method="C")
    shoot_info <- unlist(JsonData$resultSets[[1]][[3]])
    if (!is.null(shoot_info)) {
      current_player <- matrix(shoot_info, ncol=24, byrow = TRUE)
      total <- rbind(total, current_player)
    }
  }
  return(total)
}

save_one_year_whole_nocolname <- function(year) {
  total_file <- create_one_year_whole_nocolname(year)
  file_year_str <- toString(year)
  file_name_str <- paste("whole_", file_year_str, ".csv", sep = "")
  file_locate_str <- paste("/Users/MZR/Desktop/Final Project Data Set/League Year Shoot Data Whole/", file_name_str, sep = "")
  write.csv(total_file, file = file_locate_str)
}


for (i in 2003:2016){
  save_one_year_whole_nocolname(i)
}


################################################################################################
# All the code left in this file is for the following use:
# After getting 15 whole data sets, in each file (each season), 
# I want to sum the rows (players) that are from same team.
# So in the end for every season, I only have 30 rows of data (there are 30 teams in the league)
# Note: in 2003-2004 season, there were 29 teams because Charlotte Bobcats (today's Charlotte Hornets)
# came to the league in 2004-2005 season. This also brings me some complex works for furture data cleaning 

a <- read.csv(file = "/Users/MZR/Desktop/Final Project Data Set/League Year Shoot Data Whole/whole_2017.csv")
b <- select(a,-X)

dataframe_2017_test <- data.frame(b)
colnames(dataframe_2017_test) <- universe_shoot_colnames

team_name_id <-unique(dataframe_2017_test[c("TEAM_NAME","TEAM_ID")])
team_name_id_list <- as_tibble(team_name_id)
team_name_id_list$TEAM_NAME <- as.character(team_name_id_list$TEAM_NAME)

# Create team name list
universe_team_list <- list()
for (i in team_name_id_list$TEAM_NAME) {
  universe_team_list <- append(universe_team_list,i)
}

#Create team id list
team_id_list <-unique(dataframe_2017_test[,"TEAM_ID" ])

universe_team_id <- list()
for (i in team_name_id_list$TEAM_ID) {
  universe_team_id <- append(universe_team_id,i)
}

# Team abbreviation is for me to get team season game record and win percent later
# Basketball Reference website has some different team abbreviations from NBA official team abbreviation
universe_team_officail_abbr <- c("OKC", "BKN", "MIA", "ORL", "MIN", "SAS", "BOS",
                        "NOP", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                        "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                        "WAS", "GSW", "PHX", "LAC", "SAC", "DET", "UTA", 
                        "MEM", "CLE")

# Some teams relocated or changes their name over past 15 years, so I need to change their abbreviation 
team_abbr_2014_2017 <-c("OKC", "BRK", "MIA", "ORL", "MIN", "SAS", "BOS",
                        "NOP", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                        "CHI", "DEN", "ATL", "CHO", "NYK", "LAL", "DAL", 
                        "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                        "MEM", "CLE")

team_abbr_2013 <- c("OKC", "BRK", "MIA", "ORL", "MIN", "SAS", "BOS",
                   "NOP", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                   "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                   "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                   "MEM", "CLE")


team_abbr_2012 <- c("OKC", "BRK", "MIA", "ORL", "MIN", "SAS", "BOS",
                   "NOH", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                   "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                   "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                   "MEM", "CLE")

team_abbr_2008_2011 <- c("OKC", "NJN", "MIA", "ORL", "MIN", "SAS", "BOS",
                    "NOH", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                    "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                    "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                    "MEM", "CLE")

team_abbr_2007 <- c("SEA", "NJN", "MIA", "ORL", "MIN", "SAS", "BOS",
                    "NOH", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                    "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                    "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                    "MEM", "CLE")

team_abbr_2005_2006 <- c("SEA", "NJN", "MIA", "ORL", "MIN", "SAS", "BOS",
                         "NOK", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                         "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                         "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                         "MEM", "CLE")

team_abbr_2004 <- c("SEA", "NJN", "MIA", "ORL", "MIN", "SAS", "BOS",
                   "NOH", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                   "CHI", "DEN", "ATL", "CHA", "NYK", "LAL", "DAL", 
                   "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                   "MEM", "CLE")
team_abbr_2003 <- c("SEA", "NJN", "MIA", "ORL", "MIN", "SAS", "BOS",
                   "NOH", "POR", "PHI", "HOU", "IND", "MIL", "TOR",
                   "CHI", "DEN", "ATL", "NYK", "LAL", "DAL", 
                   "WAS", "GSW", "PHO", "LAC", "SAC", "DET", "UTA", 
                   "MEM", "CLE")


##########################################################################################################

#Because in 2003-2004 season there were only 29 teams, I need to manipulate this years data first.

ideal_final_table <- list()

for (year in 2003) {
  file_name_string <- paste("/Users/MZR/Desktop/Final Project Data Set/League Year Shoot Data Whole/whole_", toString(year), ".csv", sep = "")
  read_original <- read.csv(file = file_name_string)
  read_modified <- select(read_original,-X)
  
  whole_data <- as_tibble(read_modified)
  colnames(whole_data) <- universe_shoot_colnames
  
  whole_data$TEAM_NAME <- as.character(whole_data$TEAM_NAME)
  whole_data$TEAM_ID <- as.numeric(whole_data$TEAM_ID)
  whole_data$SHOT_TYPE <- as.character(whole_data$SHOT_TYPE)
  whole_data$EVENT_TYPE <- as.character(whole_data$EVENT_TYPE)
  
  whole_data$SHOT_ZONE_RANGE <- as.character(whole_data$SHOT_ZONE_RANGE)
  
  team_3_made <- list()
  team_3_attemp <- list()
  team_3_per <- list()
  
  team_2_made <- list()
  team_2_attemp <- list()
  team_2_per <- list()
  
  team_3over2 <- list()
  
  team_24plus <- list()
  team_8less <- list()
  team_16_24 <- list()
  team_8_16 <- list()
  team_backcourt <- list()
  
  
  universe_team_id_2003 <- universe_team_id[-18]
  
  universe_team_list_2003 <- universe_team_list[-18]
  
  for (i in universe_team_id_2003) {
    some_team <- filter(whole_data, TEAM_ID == i)
    some_3 <- filter(some_team, SHOT_TYPE == "3PT Field Goal")
    some_3_count <- count(some_3$EVENT_TYPE == "Made Shot")
    some_3_made <- some_3_count[2][[1]][2]
    some_3_attemp <- some_3_made + some_3_count[2][[1]][1]
    some_3_per <- round(some_3_made/some_3_attemp, 3)
    
    team_3_made <- append(team_3_made, some_3_made)
    team_3_attemp <- append(team_3_attemp, some_3_attemp)
    team_3_per <- append(team_3_per, some_3_per)
    
    some_2 <- filter(some_team, SHOT_TYPE == "2PT Field Goal")
    some_2_count <- count(some_2$EVENT_TYPE == "Made Shot")
    some_2_made <- some_2_count[2][[1]][2]
    some_2_attemp <- some_2_made + some_2_count[2][[1]][1]
    some_2_per <- round(some_2_made/some_2_attemp, 3)
    
    team_2_made <- append(team_2_made, some_2_made)
    team_2_attemp <- append(team_2_attemp, some_2_attemp)
    team_2_per <- append(team_2_per, some_2_per)
    
    some_3over2 <- round(some_3_attemp/(some_3_attemp + some_2_attemp), 3)
    
    team_3over2 <- append(team_3over2, some_3over2)
    
    
    some_24plus <- filter(some_team, SHOT_ZONE_RANGE == "24+ ft.")
    some_24plus_count <- count(some_24plus$EVENT_TYPE == "Made Shot")
    some_24plus_attemp <- some_24plus_count[2][[1]][1] + some_24plus_count[2][[1]][2]
    team_24plus<- append(team_24plus, some_24plus_attemp)
    
    some_8less <- filter(some_team, SHOT_ZONE_RANGE == "Less Than 8 ft.")
    some_8less_count <- count(some_8less$EVENT_TYPE == "Made Shot")
    some_8less_attemp <- some_8less_count[2][[1]][1] + some_8less_count[2][[1]][2]
    team_8less<- append(team_8less, some_8less_attemp)
    
    some_16_24 <- filter(some_team, SHOT_ZONE_RANGE == "16-24 ft.")
    some_16_24_count <- count(some_16_24$EVENT_TYPE == "Made Shot")
    some_16_24_attemp <- some_16_24_count[2][[1]][1] + some_16_24_count[2][[1]][2]
    team_16_24<- append(team_16_24, some_16_24_attemp)
    
    some_8_16 <- filter(some_team, SHOT_ZONE_RANGE == "8-16 ft.")
    some_8_16_count <- count(some_8_16$EVENT_TYPE == "Made Shot")
    some_8_16_attemp <- some_8_16_count[2][[1]][1] + some_8_16_count[2][[1]][2]
    team_8_16<- append(team_8_16, some_8_16_attemp)
    
    some_backcourt <- filter(some_team, SHOT_ZONE_RANGE == "Back Court Shot")
    some_backcourt_count <- count(some_backcourt$EVENT_TYPE == "Made Shot")
    some_backcourt_attemp <- some_backcourt_count[2][[1]][1] + some_backcourt_count[2][[1]][2]
    team_backcourt<- append(team_backcourt, some_backcourt_attemp)
    
  }
  
  team_win_per <- list()
  
  for (i in team_abbr_2003) {
    some_games <- NBASeasonTeamByYear(i, year + 1)
    some_total_game_number <- dim(some_games)[1]
    some_win_info <- count(some_games$x_5 == "W")
    some_win_number <- some_win_info[2][[1]][2]
    some_win_percent <- round(some_win_number/(some_win_number+ some_win_info[2][[1]][1]), 3)
    
    team_win_per <- append(team_win_per, some_win_percent)
  }
  
  
  year_list <- rep(year, 29)
  
  
  universe_team_officail_abbr_2003 <- universe_team_officail_abbr[-18]
  
  ideal_team <- data_frame(
    "Team_ID" = universe_team_id_2003,
    "Team_Name" = universe_team_list_2003,
    "Team_Abbr" = universe_team_officail_abbr_2003,
    "3P" = team_3_made,
    "3PA" = team_3_attemp,
    "3Per" = team_3_per, 
    "2P" = team_2_made,
    "2PA"= team_2_attemp,
    "2Per" = team_2_per,
    "3PA_Per" = team_3over2,
    "Season_Win_Per" = team_win_per,
    "Year" = year_list,
    "24+ ft. Atp" = team_24plus,
    "Less Than 8 ft. Atp" = team_8less,
    "16-24 ft. Atp" = team_16_24,
    "8-16 ft. Atp" = team_8_16,
    "Back Court Atp" = team_backcourt
  )
  
  
  ideal_final_table <- rbind(ideal_final_table, ideal_team)
  
}



######################################################################################################################

# Now I can use loop to generate the data from 2004 to 2017


#ideal_final_table <- list()
  
for (year in 2004:2017) {
  file_name_string <- paste("/Users/MZR/Desktop/Final Project Data Set/League Year Shoot Data Whole/whole_", toString(year), ".csv", sep = "")
  read_original <- read.csv(file = file_name_string)
  read_modified <- select(read_original,-X)
  
  whole_data <- as_tibble(read_modified)
  colnames(whole_data) <- universe_shoot_colnames
  
  whole_data$TEAM_NAME <- as.character(whole_data$TEAM_NAME)
  whole_data$TEAM_ID <- as.numeric(whole_data$TEAM_ID)
  whole_data$SHOT_TYPE <- as.character(whole_data$SHOT_TYPE)
  whole_data$EVENT_TYPE <- as.character(whole_data$EVENT_TYPE)
  
  whole_data$SHOT_ZONE_RANGE <- as.character(whole_data$SHOT_ZONE_RANGE)
  
  team_3_made <- list()
  team_3_attemp <- list()
  team_3_per <- list()
  
  team_2_made <- list()
  team_2_attemp <- list()
  team_2_per <- list()
  
  team_3over2 <- list()
  
  
  team_24plus <- list()
  team_8less <- list()
  team_16_24 <- list()
  team_8_16 <- list()
  team_backcourt <- list()
  
  
  for (i in universe_team_id) {
    some_team <- filter(whole_data, TEAM_ID == i)
    some_3 <- filter(some_team, SHOT_TYPE == "3PT Field Goal")
    some_3_count <- count(some_3$EVENT_TYPE == "Made Shot")
    some_3_made <- some_3_count[2][[1]][2]
    some_3_attemp <- some_3_made + some_3_count[2][[1]][1]
    some_3_per <- round(some_3_made/some_3_attemp, 3)
    
    team_3_made <- append(team_3_made, some_3_made)
    team_3_attemp <- append(team_3_attemp, some_3_attemp)
    team_3_per <- append(team_3_per, some_3_per)
    
    some_2 <- filter(some_team, SHOT_TYPE == "2PT Field Goal")
    some_2_count <- count(some_2$EVENT_TYPE == "Made Shot")
    some_2_made <- some_2_count[2][[1]][2]
    some_2_attemp <- some_2_made + some_2_count[2][[1]][1]
    some_2_per <- round(some_2_made/some_2_attemp, 3)
    
    team_2_made <- append(team_2_made, some_2_made)
    team_2_attemp <- append(team_2_attemp, some_2_attemp)
    team_2_per <- append(team_2_per, some_2_per)
    
    some_3over2 <- round(some_3_attemp/(some_3_attemp + some_2_attemp), 3)
    
    team_3over2 <- append(team_3over2, some_3over2)
    
    
    some_24plus <- filter(some_team, SHOT_ZONE_RANGE == "24+ ft.")
    some_24plus_count <- count(some_24plus$EVENT_TYPE == "Made Shot")
    some_24plus_attemp <- some_24plus_count[2][[1]][1] + some_24plus_count[2][[1]][2]
    team_24plus<- append(team_24plus, some_24plus_attemp)
    
    some_8less <- filter(some_team, SHOT_ZONE_RANGE == "Less Than 8 ft.")
    some_8less_count <- count(some_8less$EVENT_TYPE == "Made Shot")
    some_8less_attemp <- some_8less_count[2][[1]][1] + some_8less_count[2][[1]][2]
    team_8less<- append(team_8less, some_8less_attemp)
    
    some_16_24 <- filter(some_team, SHOT_ZONE_RANGE == "16-24 ft.")
    some_16_24_count <- count(some_16_24$EVENT_TYPE == "Made Shot")
    some_16_24_attemp <- some_16_24_count[2][[1]][1] + some_16_24_count[2][[1]][2]
    team_16_24<- append(team_16_24, some_16_24_attemp)
    
    some_8_16 <- filter(some_team, SHOT_ZONE_RANGE == "8-16 ft.")
    some_8_16_count <- count(some_8_16$EVENT_TYPE == "Made Shot")
    some_8_16_attemp <- some_8_16_count[2][[1]][1] + some_8_16_count[2][[1]][2]
    team_8_16<- append(team_8_16, some_8_16_attemp)
    
    some_backcourt <- filter(some_team, SHOT_ZONE_RANGE == "Back Court Shot")
    some_backcourt_count <- count(some_backcourt$EVENT_TYPE == "Made Shot")
    some_backcourt_attemp <- some_backcourt_count[2][[1]][1] + some_backcourt_count[2][[1]][2]
    team_backcourt<- append(team_backcourt, some_backcourt_attemp)
    
  }
  
  if (year == 2004 ) {
    universe_team_abbr <- team_abbr_2004
  }
  
  if (year == 2005 |year == 2006) {
    universe_team_abbr <- team_abbr_2005_2006
  }
  
  if (year == 2007) {
    universe_team_abbr <- team_abbr_2007
  }
  
  if (year == 2008 | year == 2009 | year == 2010 | year == 2011) {
    universe_team_abbr <- team_abbr_2008_2011
  }
  
  if (year == 2012) {
    universe_team_abbr <- team_abbr_2012
  }
  
  if (year == 2013) {
    universe_team_abbr <- team_abbr_2013
  }
  
  if (year == 2014 | year == 2015 | year == 2016 | year == 2017) {
    universe_team_abbr <- team_abbr_2014_2017
  } 
    
  team_win_per <- list()
  
  for (i in universe_team_abbr) {
    some_games <- NBASeasonTeamByYear(i, year + 1)
    some_total_game_number <- dim(some_games)[1]
    some_win_info <- count(some_games$x_5 == "W")
    some_win_number <- some_win_info[2][[1]][2]
    some_win_percent <- round(some_win_number/(some_win_number+ some_win_info[2][[1]][1]), 3)
    
    team_win_per <- append(team_win_per, some_win_percent)
  }
  
  
  year_list <- rep(year, 30)
  
  ideal_team <- data_frame(
    "Team_ID" = universe_team_id,
    "Team_Name" = universe_team_list,
    "Team_Abbr" = universe_team_officail_abbr,
    "3P" = team_3_made,
    "3PA" = team_3_attemp,
    "3Per" = team_3_per, 
    "2P" = team_2_made,
    "2PA"= team_2_attemp,
    "2Per" = team_2_per,
    "3PA_Per" = team_3over2,
    "Season_Win_Per" = team_win_per,
    "Year" = year_list,
    "24+ ft. Atp" = team_24plus,
    "Less Than 8 ft. Atp" = team_8less,
    "16-24 ft. Atp" = team_16_24,
    "8-16 ft. Atp" = team_8_16,
    "Back Court Atp" = team_backcourt
  )
  
  
  ideal_final_table <- rbind(ideal_final_table, ideal_team)
  
}


save_ideal_final_table <- ideal_final_table

save_ideal_final_table$Team_ID <- unlist(save_ideal_final_table$Team_ID)
save_ideal_final_table$Team_Name <- unlist(save_ideal_final_table$Team_Name)
save_ideal_final_table$`3P` <- unlist(save_ideal_final_table$`3P`)
save_ideal_final_table$`3PA` <- unlist(save_ideal_final_table$`3PA`)
save_ideal_final_table$`3Per` <- unlist(save_ideal_final_table$`3Per`)
save_ideal_final_table$`2P` <- unlist(save_ideal_final_table$`2P`)
save_ideal_final_table$`2PA` <- unlist(save_ideal_final_table$`2PA`)
save_ideal_final_table$`2Per` <- unlist(save_ideal_final_table$`2Per`)
save_ideal_final_table$`3PA_Per` <- unlist(save_ideal_final_table$`3PA_Per`)
save_ideal_final_table$Season_Win_Per <- unlist(save_ideal_final_table$Season_Win_Per)
save_ideal_final_table$`24+ ft. Atp` <- unlist(save_ideal_final_table$`24+ ft. Atp`)
save_ideal_final_table$`Less Than 8 ft. Atp` <- unlist(save_ideal_final_table$`Less Than 8 ft. Atp`)
save_ideal_final_table$`16-24 ft. Atp` <- unlist(save_ideal_final_table$`16-24 ft. Atp`)
save_ideal_final_table$`8-16 ft. Atp` <- unlist(save_ideal_final_table$`8-16 ft. Atp`)
save_ideal_final_table$`Back Court Atp` <- unlist(save_ideal_final_table$`Back Court Atp`)



#############################################################################################################################

# save this manipulated dataset to my local computer for furture use
# different save location in different computer
write.csv(save_ideal_final_table, file = "/Users/MZR/Desktop/Final Project Data Set/ideal_whole_data.csv", row.names = FALSE)
#write.csv(save_ideal_final_table, file = "ideal_whole_data.csv", row.names = FALSE)




