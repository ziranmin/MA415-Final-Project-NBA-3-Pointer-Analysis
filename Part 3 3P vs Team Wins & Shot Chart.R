

# MA414 Final Project: Part Three, Data Visualization, League & Team 3 Point vs Team Win Analysis
# NBA 3-Pointer Analysis 
# Ziran Min
# U5927447
# minziran@bu.edu
# 05/07/2018

#install.packages("tidyverse")
library(tidyverse)
#install.packages("httr")
library(httr)
#install.packages("rjson")
library(rjson)
#install.packages("grid")
library(grid)
#install.packages("png")
library(png)
#install.packages("jpeg")
library(jpeg)
#install.packages("RCurl")
library(RCurl)


############################################################################################
# The use of all the code in this file is for the following section in my markdown pdf report
# Section: "Three Point Shooting vs Team Win Percent"
# Section: "Side Project & Future Works: Shooting Chart"
############################################################################################

# For Section: "Three Point Shooting vs Team Win Percent"

whole_table <- read.csv(file="/Users/MZR/Desktop/Final Project Data Set/ideal_whole_data.csv")
#whole_table$Three_Made_over_Total_Field_Goal <- round(whole_table$X3P / (whole_table$X3P + whole_table$X2P),3)



#I create a new feature called "Three Made over League" which is the percentage of onw team's total 
# number of 3 point made over the total number of 3 point made by all teams in one season. 

# Create two graphs show the changes of the Golden State Warriors' and Cleveland Cavaliers' 
# "Three Made over League" and win percent.

GSW_table <- filter(whole_table, Team_Abbr == "GSW")
GSW_table$three_made_over_league <- round(GSW_table$X3P/league_year_total$`3 Point Made`, 4)
ggplot(GSW_table, aes(Year)) + 
  geom_line(aes(y = `Season_Win_Per`, col = "Team Win Percent")) +  
  geom_line(aes(y = `three_made_over_league`*10, col = "Three Made over League")) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Three Made over League %",labels = scales::percent), labels = scales::percent)+
  scale_colour_manual(values=c("pink", "purple"))+
  labs(x = "Season", y = "Team Win Percentage %",
       title = "Team Win Percent and Three Made over League Percent through Seasons") +
  scale_x_continuous(breaks = c(2003:2017))+
  theme(axis.text.x=element_text(angle=270, hjust=1))
  

CLE_table <- filter(whole_table, Team_Abbr == "CLE")
CLE_table$three_made_over_league <- round(CLE_table$X3P/league_year_total$`3 Point Made`, 4)
ggplot(CLE_table, aes(Year)) + 
  geom_line(aes(y = `Season_Win_Per`, col = "Team Win Percent")) +  
  geom_line(aes(y = `three_made_over_league`*10, col = "Three Made over League")) +
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "Three Made over League %",labels = scales::percent), labels = scales::percent)+
  scale_colour_manual(values=c("pink", "purple"))+
  labs(x = "Season", y = "Team Win Percentage %",
       title = "Team Win Percent and Three Made over League Percent through Seasons") +
  scale_x_continuous(breaks = c(2003:2017))+
  theme(axis.text.x=element_text(angle=270, hjust=1)) 



# Build a scatter plot of every team's "3 Made over League" against "Team Win Percent" in recent 15 years.
# To create feature "3 Made over League" in every team's data in every season

team_abbr <- unique(whole_table[,"Team_Abbr"])
counter <- 1
for (i in 1:30) {
  team_str <- toString(team_abbr[[i]])
  if (team_str != "CHA") {
    team_table <- filter(whole_table, Team_Abbr == team_str)
    team_table$three_made_over_league <- round(team_table$X3P/league_year_total$`3 Point Made`, 4)
    if (counter == 1) {
     league_table_with_three_over_league <- team_table
      }
    if (counter != 1) {
     league_table_with_three_over_league <- rbind(league_table_with_three_over_league, team_table)
     }
     counter <- counter + 1
  }
  if (team_str == "CHA") {
    team_table <- filter(whole_table, Team_Abbr == team_str)
    league_year_total_no2003 <- league_year_total[-1,]
    team_table$three_made_over_league <- round(team_table$X3P/league_year_total_no2003$`3 Point Made`, 4)
    if (counter == 1) {
      league_table_with_three_over_league <- team_table
    }
    if (counter != 1) {
      league_table_with_three_over_league <- rbind(league_table_with_three_over_league, team_table)
    }
    counter <- counter + 1
  }
}

# Visaulization of scatter plot 
ggplot(league_table_with_three_over_league, aes(Season_Win_Per, three_made_over_league)) + 
  geom_point() +
  geom_smooth(method='lm',formula=y~x) + 
  labs(x = "Team Win Percent", y = "Team 3 Made over League",
     title = "Team Win Percent vs Team 3 Made over League") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent)

# Correlation test 
cor(league_table_with_three_over_league$Season_Win_Per, league_table_with_three_over_league$three_made_over_league)
cor.test(league_table_with_three_over_league$Season_Win_Per, league_table_with_three_over_league$three_made_over_league, method = "pearson")



############################################################################################################
# Section: "Side Project & Future Works: Shooting Chart"
# Reference: https://thedatagame.com.au/2015/09/27/how-to-create-nba-shot-charts-in-r/

# Get Curry's shoot data in 2015-2016 Season
# Build a rough shot chart
playerID <- 201939
shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=2015-16&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&PlayerPosition=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",playerID,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
shotData <- fromJSON(file = shotURL, method="C")
shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
colnames(shotDataf) <- shotData$resultSets[[1]][[2]]
shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) +
  geom_point(aes(colour = EVENT_TYPE))


# To make beautify the chart, I add court background, player profile photo, and make points more easy to understand
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))
playerImg.URL <- paste("http://stats.nba.com/media/players/132x132/",playerID,".png", sep="")
playerImg <- rasterGrob(readPNG(getURLContent(playerImg.URL)), 
                        width=unit(0.15, "npc"), height=unit(0.23, "npc"))
ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
  annotation_custom(court, -250, 250, -52, 418) +
  geom_point(aes(colour = EVENT_TYPE, alpha = 0.8, shape = EVENT_TYPE), size = 3) +
  scale_color_manual(values = c("red", "blue")) +
  scale_shape_manual(values=c(19,4)) +
  guides(alpha = FALSE, size = FALSE) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), "\n","2015-2016 Season", sep = "")) +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))
pushViewport(viewport(x = unit(0.8, "npc"), y = unit(0.8, "npc")))
print(grid.draw(playerImg), newpage=FALSE)
grid.text(label = "thedatagame.com.au", just = "centre", vjust = 50)

























