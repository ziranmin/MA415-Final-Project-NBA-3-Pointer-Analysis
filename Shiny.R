#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#install.packages(shiny)
library(shiny)
#install.packages(shinydashboard)
library(shinydashboard)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("stringr")
library(stringr)
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
options(warn = -1)

######################################################################################################################
#Code From Part 2
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

league_year_total$Three_Made_Per <- round(league_year_total$`3 Point Made`/(league_year_total$`3 Point Made` + league_year_total$`2 Point Made`), 3)


######################################################################################################################
# New code just for shiny
some_year <- filter(whole_table, Year == 2004)
team_name_abbr_list <- select(some_year, Team_Name, Team_Abbr)
team_name_list <- list()
for (i in team_name_abbr_list$Team_Name){
  team_name_list <- append(team_name_list, toString(i))
}
team_name_list <- sort(unlist(team_name_list))

######################################################################################################################
# Code from part 3
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

######################################################################################################################

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

playerID_name_since_2003 <- filter(playerID_name, TO_YEAR >= 2003)

name_list <- list()
for (i in playerID_name_since_2003$DISPLAY_FIRST_LAST){
  name_list <- append(name_list, toString(i))
}
name_list <- sort(unlist(name_list))

year_list <- list()
for (i in 2003:2017) {
  year_list <- append(year_list, toString(i))
}
year_list <- unlist(year_list)
######################################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "NBA Shot Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Player Shot Chart", tabName = "shot_chart"),
      menuItem("League Level Shot Analysis", tabName = "Shot_League"),
      menuItem("3 Point vs Team Win Percent", tabName = "3_Win"),
      menuItem("Correlation of 3P & Win Percent", tabName = "3_Win_Cor")
    )
  ),
  
  dashboardBody(
    tabItems(
        tabItem(tabName = "shot_chart",
                fluidRow(
                  box(selectInput("player_name_mode",
                                  "Player Name (If some player didn't play in some season, it shows error):",
                                  choices = name_list, 
                                  selected = "Stephen Curry"), 
                      selectInput("year_mode",
                                  "Season (If some player didn't play in some season, it shows error):",
                                  choices = year_list,
                                  selected = "2015"),
                      plotOutput("plot1"), width = 12)
                )
        ),
        
        tabItem(tabName = "Shot_League",
                fluidRow(
                 box(selectInput("Shot_League_mode",
                                "Shot Variable:",
                                choices = list("3 Point Made & 3 Point Attempt",
                                               "3 Point Made over All Field Goal Made & 3 Point Attempt over All Field Goal Attempt",
                                               "24+ ft. Shot Usage %",
                                               "16-24 ft. Shot Usage %",
                                               "8-16 ft. Shot Usage %",
                                               "8 less ft. Shot Usage %",
                                               "(24+ ft. Shot Usage %) & (16-24 ft. Shot Usage %) & (8-16 ft. Shot Usage %) & (8 less ft. Shot Usage %)"
                                               )), 
                    plotOutput("plot2"), width = 12)
              )
      ),
      
      tabItem(tabName = "3_Win",
              fluidRow(
                box(selectInput("three_win_mode",
                                "Team Name:",
                                choices = team_name_list), 
                    plotOutput("plot3"), width = 12)
              )
      ),
      
      tabItem(tabName = "3_Win_Cor",
              fluidRow(
                box(selectInput("3_Win_Cor_mode",
                                "One Choice:",
                                choices = list("Team Win Percent vs Team 3 Made over League")), 
                    plotOutput("plot4"), width = 12)
              )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot({
    
      name <- input$player_name_mode
      year <- input$year_mode
      player_name_id <- filter(playerID_name_since_2003, DISPLAY_FIRST_LAST == name)
      player_id <- player_name_id$PERSON_ID
    
    
      first_year_str <- year
      second_year_num <- as.numeric(year) + 1
      second_year_str <- substr(second_year_num, 3, 4)
    
    
      shotURL <- paste("http://stats.nba.com/stats/shotchartdetail?CFID=33&CFPARAMS=", first_year_str, "-", second_year_str,"&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&PlayerPosition=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerID=",player_id,"&PlusMinus=N&Position=&Rank=N&RookieYear=&Season=", first_year_str,"-", second_year_str, "&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=&mode=Advanced&showDetails=0&showShots=1&showZones=0", sep = "")
     
      shotData <- fromJSON(file = shotURL, method="C")
      shotDataf <- data.frame(matrix(unlist(shotData$resultSets[[1]][[3]]), ncol=24, byrow = TRUE))
      colnames(shotDataf) <- shotData$resultSets[[1]][[2]]
      shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
      shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
      shotDataf$SHOT_DISTANCE <- as.numeric(as.character(shotDataf$SHOT_DISTANCE))
    
    
      courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
      court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                        width=unit(1,"npc"), height=unit(1,"npc"))
    
      graph <- ggplot(shotDataf, aes(x=LOC_X, y=LOC_Y)) + 
      annotation_custom(court, -250, 250, -52, 418) +
      geom_point(aes(colour = EVENT_TYPE, alpha = 0.8, shape = EVENT_TYPE), size = 3) +
      scale_color_manual(values = c("red", "blue")) +
      scale_shape_manual(values=c(19,4)) +
      guides(alpha = FALSE, size = FALSE) +
      xlim(250, -250) +
      ylim(-52, 418) +
      coord_fixed() +
      ggtitle(paste("Shot Chart\n", unique(shotDataf$PLAYER_NAME), "\n",first_year_str, "-", toString(second_year_num), " Season", sep = "")) +
      theme(line = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(size = 17, lineheight = 1.2, face = "bold"))
    
      print(graph)
  
  })
    
    
  output$plot2 <- renderPlot({
    
    if (input$Shot_League_mode == "3 Point Made & 3 Point Attempt") {
      graph <- ggplot(league_year_total, aes(Year)) +
        geom_line(aes(y = `3 Point Made`, col = "3 Point Made")) +
        geom_line(aes(y = `3 Point Attemps`, col = "3 Point Attempt")) +
        scale_colour_manual(values=c("skyblue", "blue"))+
        labs(x = "Season", y = "3 Point Shot",
             title = "League 3 Point Made and 3 Point Attempts through Seasons") +
        scale_x_continuous(breaks = c(2003:2017)) 
      print(graph)
    }
    
    if (input$Shot_League_mode == "3 Point Made over All Field Goal Made & 3 Point Attempt over All Field Goal Attempt") {
      graph <- ggplot(league_year_total, aes(Year)) +
        geom_line(aes(y = `3 ATP Per`, col = "3P Attempt Usage %")) +
        geom_line(aes(y = `Three_Made_Per`, col = "3P FG Made over All FG")) +
        scale_colour_manual(values=c("orange", "red"))+
        labs(x = "Season", y = "Percentage",
             title = "League % of 3P Made over All FG Made and % of 3P Attempt over All FG Attempt through Seasons") +
        scale_x_continuous(breaks = c(2003:2017)) 
      print(graph)
    }
    
    if (input$Shot_League_mode == "24+ ft. Shot Usage %") {
      graph <- ggplot(league_year_total, aes(Year, `24+ ft ATP Per`)) + geom_line(color = "red") +
        labs(x = "Season", y = "24+ ft. Shot Usage %",
             title = "League 24+ ft. Shot Usage %  through Seasons") +
        scale_x_continuous(breaks = c(2003:2017))
      print(graph)
    }
    
    if (input$Shot_League_mode == "16-24 ft. Shot Usage %") {
      graph <- ggplot(league_year_total, aes(Year, `16-24 ft ATP Per`)) + geom_line(color = "green") +
        labs(x = "Season", y = "16-24 ft. Shot Usage %",
             title = "League 16-24 ft. Shot Usage % through Seasons") +
        scale_x_continuous(breaks = c(2003:2017))
      print(graph)
    }
    
    if (input$Shot_League_mode == "8-16 ft. Shot Usage %") {
      graph <- ggplot(league_year_total, aes(Year, `8-16 ft ATP Per`)) + geom_line(color = "orange") +
        labs(x = "Season", y = "8-16 ft. Shot Usage %",
             title = "League 8-16 ft. Shot Usage % through Seasons") +
        scale_x_continuous(breaks = c(2003:2017))
      print(graph)
    }
    
    if (input$Shot_League_mode == "8 less ft. Shot Usage %") {
      graph <- ggplot(league_year_total, aes(Year, `8 less ft ATP Per`)) + geom_line(color = "blue") +
        labs(x = "Season", y = "8 less ft.Shot Usage %",
             title = "League 8 less ft. Shot Usage % through Seasons") +
        scale_x_continuous(breaks = c(2003:2017))
      print(graph)
    }
    

    if (input$Shot_League_mode == "(24+ ft. Shot Usage %) & (16-24 ft. Shot Usage %) & (8-16 ft. Shot Usage %) & (8 less ft. Shot Usage %)") {
      graph <- ggplot(league_year_total, aes(Year)) + 
        geom_line(aes(y = `24+ ft ATP Per`, col = "24+ ft. Shot Usage %")) + 
        geom_line(aes(y = `8 less ft ATP Per`, col = "8 less ft. Shot Usage %")) +
        geom_line(aes(y = `16-24 ft ATP Per`, col = "16-24 ft. Shot Usage %")) +
        geom_line(aes(y = `8-16 ft ATP Per`, col = "8-16 ft. Shot Usage %")) +
        scale_colour_manual(values=c("green", "red", "blue", "orange")) +
        labs(x = "Season", y = "Shot Usage %",
             title = "League Shot Usage % for Different Distances through Seasons") +
        scale_x_continuous(breaks = c(2003:2017)) + 
        scale_y_continuous(breaks = c(0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45), labels = scales::percent) 
      print(graph)
    }
    
    
  })


  
  output$plot3 <- renderPlot({
    
    some_team <- filter(team_name_abbr_list, Team_Name == input$three_win_mode)
    some_team_abbr <- toString(some_team$Team_Abbr)
    
    if (some_team_abbr != "CHA") {
    
    some_team_table <- filter(whole_table, Team_Abbr == some_team_abbr)
    some_team_table$three_made_over_league <- round(some_team_table$X3P/league_year_total$`3 Point Made`, 4)
    graph <- ggplot(some_team_table, aes(Year)) + 
      geom_line(aes(y = `Season_Win_Per`, col = "Team Win Percent")) +  
      geom_line(aes(y = `three_made_over_league`*10, col = "Three Made over League")) +
      scale_y_continuous(sec.axis = sec_axis(~./10, name = "Three Made over League %",labels = scales::percent), labels = scales::percent)+
      scale_colour_manual(values=c("purple", "pink"))+
      labs(x = "Season", y = "Team Win Percentage %",
           title = paste(input$three_win_mode, " Win Percent and Three Made over League Percent through Seasons", sep = "")) +
      scale_x_continuous(breaks = c(2003:2017))
    print(graph)
    }
    
    if (some_team_abbr == "CHA"){
      some_team_table <- filter(whole_table, Team_Abbr == some_team_abbr)
      no2003 <- league_year_total[-1,]
      some_team_table$three_made_over_league <- round(some_team_table$X3P/no2003$`3 Point Made`, 4)
      graph <- ggplot(some_team_table, aes(Year)) + 
        geom_line(aes(y = `Season_Win_Per`, col = "Team Win Percent")) +  
        geom_line(aes(y = `three_made_over_league`*10, col = "Three Made over League")) +
        scale_y_continuous(sec.axis = sec_axis(~./10, name = "Three Made over League %",labels = scales::percent), labels = scales::percent)+
        scale_colour_manual(values=c("purple", "pink"))+
        labs(x = "Season", y = "Team Win Percentage %",
             title = paste(input$three_win_mode, " Win Percent and Three Made over League Percent through Seasons", sep = "")) +
        scale_x_continuous(breaks = c(2003:2017))
      print(graph)
    }
    
  })
  
  
  output$plot4 <- renderPlot({
        graph <- ggplot(league_table_with_three_over_league, aes(Season_Win_Per, three_made_over_league)) + 
          geom_point() +
          geom_smooth(method='lm',formula=y~x) + 
          labs(x = "Team Win Percent", y = "Team 3 Made over League",
               title = "Team Win Percent vs Team 3 Made over League") +
          scale_x_continuous(labels = scales::percent) + 
          scale_y_continuous(labels = scales::percent)

        print(graph)
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)


