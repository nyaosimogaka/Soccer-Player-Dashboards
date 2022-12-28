#Load libraries
library(shiny)
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggsoccer)

#Read data
df <- read.csv("full.csv")

#Define Minutes Played Function
calculate.minsplayed <- function(x) {
  if(all(x$Event == 'FT')) {
    return(data.frame(x$Mins))
  } else if (all(x$Event %in% c('FT', 'Sub In'))) {
    subin.value <- x$Mins[x$Event == 'Sub In']
    ft.value <- x$Mins[x$Event == 'FT']
    return(data.frame(ft.value - subin.value))
  } else if (all(x$Event %in% c('FT', 'Sub Out'))) {
    return(data.frame(x$Mins[x$Event == 'Sub Out']))
  } else if (all(x$Event %in% c('Sub In', 'Sub Out', 'FT'))) {
    subin.value <- x$Mins[x$Event == 'Sub In']
    subout.value <- x$Mins[x$Event == 'Sub Out']
    return(data.frame(subout.value - subin.value))
  }
}

#Define Fouls Function
calculate.fouls <- function(x) {
  fouls <- x %>%
    filter(Event == 'Foul') %>%
    select(Event) %>%
    count(Event)
}

#Cards Function
calculate.cards <- function(x) {
  cards <- x %>%
    filter(Result == 'yellow card' | Result == 'red card') %>%
    select(Result) %>%
    count(Result)
}

#Define Shots Function
calculate.shots <- function(x) {
  shots <- x %>%
    filter(Type == 'Shot') %>%
    select(Type, Event) %>%
    group_by(Type, Event) %>%
    count(Event) %>%
    pivot_wider(names_from = Event, values_from = n)
}

#Goals Function
calculate.goals <- function(x) {
  goals <- x %>%
    filter(Result == 'goal') %>%
    select(Result) %>%
    count(Result)
}

#Player Dashboard 1 Merge mins_played, fouls, cards, shots, goals function



#Player Dashboard 2 Function
calculate.playersummary <- function(x) {
  player.summary <- x %>%
    select(Type, Event, Result) %>%
    group_by(Type, Event) %>%
    mutate(Event = fct_recode(Event, `INT/Tkl` = 'Tackle', `INT/Tkl` = 'INT')) %>%
    mutate(Result = fct_recode(Result, good = 'RefAdv', good = 'won', good = 'sucsessful', good = 'complete', good = 'possession gain', `not good` = 'out of bounds', `not good` = 'incomplete', `not good` = 'possession loss', `not good` = 'save', `not good` = 'RefStop', `not good` = 'block', `not good` = 'unsucsessful', `not good` = 'lost', `good` = 'goal', `not good` = 'yellow card', `not good` = 'red card')) %>%
    mutate(Result = fct_explicit_na(Result, "not good")) %>%
    mutate(Result = replace(Result, Type=='Shot', 'good')) %>%
    mutate(Result = replace(Result, Event=='INT/Tkl', 'good')) %>%
    count(Result) %>%
    pivot_wider(names_from = Result, values_from = n) %>%
    drop_na(Type) %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>%
    mutate(total = good + `not good`) %>%
    mutate(`%` = good/total * 100) %>%
    subset(select = -c(`not good`)) %>%
    mutate_if(is.numeric, ~round(., 0)) %>%
    filter(!((Event == 'Fouled') | (Type == 'restart') | (Type == 'Shot') | (Event == 'Foul')))
}

#Player Details Table Function
display.details <- function(x) {
  details.table <- x %>%
    select(Period, Type, Event, Result, Mins, Secs)
}

#Player Viz Function
display.plot <- function(x) {
  viz1 <- x %>%
    select(Type, Event, Result, Mins, Secs, X, Y) %>%
    mutate(secs = if_else(Secs <=9, paste0('0',Secs), as.character(Secs))) %>%
    mutate(Time = paste0(Mins, ":", secs)) %>%
    select(Type, Event, Result, Time, X, Y) %>%
    #Filter out events that do not include possession gain/loss
    filter(!(Event == 'Foul' | Event == 'Fouled' | Event == 'Sub Out' | Event == 'Sub In' | Event == 'Fairplay Start' 
             | Event == 'start' | Event == 'kick off' | Type == 'shot' | Event == 'loose ball duel' | Event == 'aerial duel'
             | Event == 'RefStop' | Event == 'fair play' | Event == 'offside' | Event == 'HT' | Event == 'FT' | Type == 'set piece'
             | Event == 'Out Of Scope' | Type == 'Shot')) %>%
    filter(!(Type == 'pass' & Result == 'complete') & !(Event == 'launch' & Result == 'out of bounds') 
           & !(Event == 'throw' & Result == 'complete') & !(Event == 'touch' & Result == 'out of bounds')
           & !(Event == 'touch' & Result == 'out of bounds') & !(Type == 'set piece' & Event == 'corner kick')
           & !(Event == 'launch' & Event == 'possession loss') & !(Event == 'block' & is.na(Event))
           & !(Event == 'touch' & is.na(Result)) & !(Event == 'launch' & is.na(Result))
           & !(Event == 'touch' & Result == 'possession loss') & !(Event == 'pass' & is.na(Result))
           & !(Event == 'block' & Result == 'out of bounds') & !(Event == 'dribble' & is.na(Result))
           & !(Event == 'dribble' & Result == 'successful') & !(Event == 'dribble' & Result == 'sucsessful')
           & !(Event == 'pass' & Result == 'save') & !(Event == 'launch' & Result == 'block') 
           & !(Event == 'block' & Result == 'possession loss'))
}


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Soccer Dashboards"),
  
  #Tab-set Panel for Team Dashboards, Player Dashboards, Team Comparisons and Player Comparisons 
  mainPanel(
    tabsetPanel(
      tabPanel("Player", br(),
               sidebarLayout(
                 sidebarPanel(
                   selectInput("tournament", "Select Tournament", choices = unique(df$Tournament)),
                   selectInput("match", "Select Match", choices = unique(df$MatchID)),
                   selectInput("team", "Select Team", choices = unique(df$Team)),
                   selectInput("player", "Select Player", choices = unique(df$Player))
                 ),
                 mainPanel(
                   plotOutput("playerviz"),
                   tableOutput("playersummary1"),
                   tableOutput("playersummary2"),
                   dataTableOutput("details")
                 )
               ))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Observer to update tournament selection
  
  observeEvent(input$tournament,{
    tournament <- input$tournament
    match_options <- unique(df[which(df$Tournament == tournament),]$MatchID)
    
    updateSelectInput(session, inputId = "match",
                      choices = match_options)
  })
  
  #Observer to update match selection
  
  observeEvent(input$match,{
    match <- input$match
    team_options <- unique(df[which(df$MatchID == match),]$Team)
    
    updateSelectInput(session, inputId = "team",
                      choices = team_options)
  })
  
  #Observer to update team selection
  
  observeEvent(input$team,{
    team <- input$team
    player_options <- unique(df[which(df$Team == team),]$Player)
    
    updateSelectInput(session, inputId = "player",
                      choices = player_options)
  })
  
  #Observer to update player selection
  
  observeEvent(input$player, {
    player <- input$player
  })
  
  #Reactive data frame for match selection
  tournaments <- reactiveValues()
  observe({
    tournaments$df <- df[which(df$Tournament == input$tournament),]
  })
  
  #Reactive data frame for match selection
  matchs <- reactiveValues()
  observe({
    matchs$df <- df[which(df$Tournament == input$tournament & df$MatchID == input$match),]
  })
  
  #Reactive data frame for team selection
  teams <- reactiveValues()
  observe({
    teams$df <- df[which(df$Tournament == input$tournament & df$MatchID == input$match & df$Team == input$team),]
  })
  
  #Reactive data frame for player selection
  values <- reactiveValues()
  observe({
    values$df <- df[which(df$Tournament == input$tournament & df$MatchID == input$match & df$Team == input$team & df$Player == input$player),]
  })
  
  #DASHBOARD 1
  
  output$playersummary1 <- renderTable({
    events.data <- values$df
    marker.data <- matchs$df %>%
      filter(Event == 'start' | Event == 'HT' | Event == 'FT')
    full.data <- rbind(events.data, marker.data)
    full.data %>%
      calculate.minsplayed()
  })
  
  #DASHBOARD 2
  output$playersummary2 <- renderTable({
    events.data <- values$df
    
    events.data %>%
      calculate.playersummary()
  },
  digits = 0)
  
  #VIZ 1 Output - Touch Map
  output$playerviz <- renderPlot({
    events.data <- values$df
    
    events.data <- events.data %>%
      display.plot()
    
    ggplot(data = events.data, aes(x=X, y=Y, color = Result, shape = Event)) +
      annotate_pitch(fill = 'springgreen4', colour = 'white') +
      geom_point()+
      theme_pitch()
  })
  
  #DETAILS TABLE - Display specific events
  
  output$details <- renderDataTable({
    events.data <- values$df
    marker.data <- matchs$df %>%
      filter(Event == 'start' | Event == 'HT' | Event == 'FT')
    full.data <- rbind(events.data, marker.data)
    full.data %>%
      display.details()
  },
  options = list(pageLength = 5))
  
}

# Run the application 
shinyApp(ui = ui, server = server)
