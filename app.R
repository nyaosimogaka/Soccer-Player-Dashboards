#Load libraries
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyverse)
library(ggsoccer)
library(ggplot2)
library(gdata)
library(huxtable)
library(data.table)
library(janitor)

#Read data
df <- read_excel("Dashboard.xlsx", sheet=2)

#drop Player
df <- subset(df, select = -c(Player))
#Rename PlayerID to player
df$Player <- df$PlayerID
#drop PlayerID
df <- subset(df, select = -c(PlayerID))
#correct data types and re scale y and y2 coordinates
df$X <- as.integer(df$X)
df$Y <- 100-as.integer(df$Y)
df$X2 <- as.integer(df$X2)
df$Y2 <- 100-as.integer(df$Y2)
df$Y <- as.integer(df$Y)
df$Y2 <- as.integer(df$Y2)
df %<>% mutate_if(is.character,as.factor)

#Clean String Data
df$Event <- str_replace(df$Event,"Save","save")
df$Event <- str_replace(df$Event,"Int","INT")
df$Event <- str_replace(df$Event,"Offside","offside")
df$Event <- str_replace(df$Event,"Block","block")
df$Event <- str_replace(df$Event,"Fairplay start","Fairplay Start")
#change character columns to factor columns
df %<>% mutate_if(is.character,as.factor)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Match Data Analysis"),
  fluidRow(
    column(2, 
           selectInput("match", "Select Match", choices = unique(df$MatchID)),
           selectInput("team", "Select Team", choices = unique(df$Team)),
           selectInput("player", "Select Player", choices = unique(df$Player))
    ),
    column(2, 
           tableOutput("mins_played"),
           tableOutput("fouls"),
           tableOutput("cards"),
           tableOutput("shots"),
           tableOutput("goals")
    ),
    column(3,
           tableOutput("summary")
    ),
    column(5,
           dataTableOutput("details")
    )
    
  ),
  fluidRow(
    column(6,
           plotOutput("viz")
    ),
    column(6,
           plotOutput("viz2")
           )
  )
  
  # sidebarPanel(
  #   selectInput("team", "Select Team", choices = unique(df$Team)),
  #   selectInput("player", "Select Player", choices = unique(df$Player)),
  #   selectInput("event", "Select Event", choices = unique(df$Event), multiple = TRUE),
  #   checkboxGroupInput("type", "Select Event Type", choices = unique(df$Type))),
  # mainPanel(
  #   tableOutput("summary"),
  #   plotOutput("viz")
  # )
  # 
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
  matchs <- reactiveValues()
  observe({
    matchs$df <- df[which(df$MatchID == input$match),]
  })
  
  #Reactive data frame for team selection
  teams <- reactiveValues()
  observe({
    teams$df <- df[which(df$MatchID == input$match & df$Team == input$team),]
  })
  
  #Reactive data frame for player selection
  values <- reactiveValues()
  observe({
    values$df <- df[which(df$MatchID == input$match & df$Team == input$team & df$Player == input$player),]
  })
  
  ##SUMMARY TABLE 1
  #Minutes played - store to a DF to merge later
  
  output$mins_played <- renderTable({
    events.data <- values$df
    marker.data <- matchs$df %>%
      filter(Event == 'start' | Event == 'FT' | Event == 'HT')
    #code to calculate minutes played from events.data (include start/FT in observe events)
    full.data <- rbind(events.data, marker.data)
    full.data %>%
      mutate(mins_played = if_else(Event == 'Sub Out', Mins, 
                                   if_else(Event == 'Sub In',max(Mins) - Mins, 
                                           if_else(Event != 'Sub Out' | Event != 'Sub In', max(Mins), max(Mins))))) %>%
      filter(case_when(
        Event %in% 'Sub Out' ~ Event != 'FT',
        Event %in% 'Sub In' ~ Event != 'FT', 
        Event != 'Sub Out' ~ Event == 'FT',
        Event != 'Sub In' ~ Event == 'FT'
      )) %>%
      select(mins_played)
  })
  
  #Fouls - store to a DF to merge later
  
  output$fouls <- renderTable({
    events.data <- values$df
    #code to calculate fouls committed from events.data, include yellow/red cards
    events.data %>%
      filter(Event == 'Foul') %>%
      select(Event) %>%
      count(Event)
  })
  
  #Cards - store to a DF to merge later
  output$cards <- renderTable({
    events.data <- values$df
    #code to calculate fouls committed from events.data, include yellow/red cards
    events.data %>%
      filter(Result == 'yellow card' | Result == 'red card') %>%
      select(Result) %>%
      count(Result)
  })
  
  #Shots - store to a DF to merge later
  output$shots <- renderTable({
    events.data <- values$df
    #code to calculate shots made from events.data, include ont/oft summary and goals scored
    events.data %>%
      filter(Type == 'Shot') %>%
      select(Type, Event) %>%
      group_by(Type, Event) %>%
      count(Event) %>%
      pivot_wider(names_from = Event, values_from = n)
  })
  
  #Goals - store to a DF to merge later
  output$goals <- renderTable({
    events.data <- values$df
    #code to calculate goals from events.data
    events.data %>%
      filter(Result == 'goal') %>%
      select(Result) %>%
      count(Result)
  })
  
  #Merge_DFs(Mins Played, Fouls, Cards, Shots, goals)
  
  
  #DETAILS TABLE - Display specific events
  
  output$details <- renderDataTable({
    events.data <- values$df
    marker.data <- matchs$df %>%
      filter(Event == 'start' | Event == 'HT' | Event == 'FT')
    full.data <- rbind(events.data, marker.data)
    full.data %>%
      select(Period, Type, Event, Result, Mins, Secs)
  },
  options = list(pageLength = 5))
  
  #DASHBOARD
  output$summary <- renderTable({
    events.data <- values$df
    
    events.data %>%
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
  },
  digits = 0)
  
  #VIZ 1 Output - Touch Map
  output$viz2 <- renderPlot({
    events.data <- values$df %>%
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
    
    
    ggplot(data = events.data, aes(x=X, y=Y, color = Result, shape = Event)) +
      annotate_pitch(fill = 'springgreen4', colour = 'white') +
      geom_point()+
      theme_pitch()
  })
  
  #VIZ 2 Output - Line Graph (Time vs Event Count[player/team])
  output$viz <- renderPlot({
    player.events <- values$df %>%
      select(Player, Mins) %>%
      group_by(Player, Mins) %>%
      count(Mins) %>%
      mutate(Object = Player) %>%
      subset(select = -c(Player)) %>%
      select(Object, Mins, n)
    team.events <- teams$df %>%
      select(Team, Mins) %>%
      group_by(Team, Mins) %>%
      count(Mins) %>%
      mutate(Object = Team) %>%
      subset(select = -c(Team)) %>%
      select(Object, Mins, n)
    
    bind_rows(team.events, player.events, .id = NULL) %>%
      ggplot(aes(x = Mins, y = n, group = Object)) +
      geom_line(
                size = 1.2) +
      geom_point(
                 aes(fill = Object),
                 size = 3.5, shape = 21, stroke = 2.5) +
      scale_x_continuous(breaks = seq(0, 95, by = 5),
                         labels = c(seq(0, 40, by = 5), "HT", 
                                    seq(50, 90, by = 5), "FT"),
                         limits = c(-3, 95),
                         expand = c(0.01, 0)) +
      scale_y_continuous(breaks = seq(0, 30, by = 5),
                         labels = seq(0, 30, by = 5)) +
      theme_minimal() +
      theme(text = element_text(family = "robotoc"),
            plot.subtitle = element_text(size = 18, family = "robotoc",
                                         color = "grey20"),
            axis.title = element_text(size = 18, color = "grey20"),
            axis.text = element_text(size = 16, face = "bold"),
            panel.grid.minor = element_blank(),
            legend.position = c(0.25, 0.95),
            legend.direction = "horizontal",
            legend.title = element_blank())
    
    #ggplot(data = events.data, aes(x=X, y=Y)) +
      #annotate_pitch(fill = 'springgreen4', colour = 'white') +
      #geom_point()+
      #theme_pitch()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
