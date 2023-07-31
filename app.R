#Load Libraries
library(shiny)
library(dplyr)
library(tidyr)
library(knitr)
library(ggplot2)
library(ggsoccer)
## DATA PROCESSING ----

#Read data ----
df <- read.csv("EventData.csv")

#Convert and reformat data types ----
df$Mins <- as.numeric(df$Mins)
df$Y <- 100-as.numeric(df$Y)
df$Y2 <- 100-as.numeric(df$Y2)
df$X <- as.numeric(df$X)
df$X2 <- as.numeric(df$X2)

#Player Details Table Function
display.details <- function(x) {
  details.table <- x %>%
    select(Opponent, Category, Subcategory, Result, period, Timestamp) %>%
    mutate(
      Event = case_when(
        Category == "Pass" ~ paste(Result, Category, "(", Subcategory, ")", sep = " "),
        Category == "Shot" ~ paste(Category, Subcategory, "(", Result, ")", sep = " "),
        Category == "Block" ~ paste(Category),
        Category == "FK" ~ paste("Free Kick"),
        Subcategory == "Offside" ~ paste(Subcategory),
        Subcategory == "Kick Off" ~ paste(Subcategory),
        Subcategory == "Half Time" ~ paste(Subcategory),
        Subcategory == "Full Time" ~ paste(Subcategory),
        Subcategory == "Fouled" ~ paste(Subcategory),
        Subcategory == "Foul" ~ paste(Subcategory),
        Subcategory == "Clearance" ~ paste(Subcategory),
        Subcategory == "Carry" ~ paste(Subcategory),
        Subcategory == "Touch" ~ paste(Subcategory, "(", Result, ")"),
        Subcategory == "Sub In" ~ paste("Substitute On"),
        Subcategory == "Sub Out" ~ paste("Substitute Off"),
        Subcategory == "Interception" | Subcategory == "Tackle" ~ paste(Subcategory),
        Subcategory == "Dribble" ~ paste(Result, Subcategory),
        Category == "Duel" ~ paste(Result, " ", Subcategory, "-", Category, sep=""),
        TRUE ~ "Other Scenarios" # Default narrative for all other cases
        )
      ) %>%
    select(Opponent, Event, period, Timestamp)
}

#Goals Function
calculate.goals <- function(x) {
  goals.table <- x %>%
    filter(Result == 'Goal') %>%
    select(Result) %>%
    count(Result)
}

#Shots Function
calculate.shots <- function(x) {
  shots.table <- x %>%
    filter(Category == 'Shot') %>%
    select(Category, Subcategory) %>%
    group_by(Category, Subcategory) %>%
    count(Subcategory) %>%
    pivot_wider(names_from = Subcategory, values_from = n)
}

#Fouls Function
calculate.fouls <- function(x) {
  fouls.table <- x %>%
    filter(Subcategory == 'Foul') %>%
    select(Subcategory) %>%
    count(Subcategory)
}

#Cards Function (Update to handle, Disc-Foul-yc, Disc-Foul-2yc, Disc-Foul-rc)
calculate.cards <- function(x) {
  cards.tables <- x %>%
    filter(Result == 'Yellow Card' | Result == 'Red Card') %>%
    select(Result) %>%
    count(Result)
}

#Challenges Function
calculate.duels <- function(x) {
  duels.table <- x %>% 
    filter(Category == 'Duel') %>% 
    mutate(Narrative = case_when( 
      Category == "Duel" ~ paste(Subcategory, Category, sep = " "),
      TRUE ~ "Other Scenarios" # Default narrative for all other cases
    )
    ) %>%
    select(Narrative, Result) %>%
    group_by(Narrative, Result) %>%
    count(Result)
}

#Ball Recoveries Function
calculate.recoveries <- function(x) {
  recoveries.table <- x %>% 
    filter(Subcategory == 'Interception' | Subcategory == 'Tackle') %>% 
    select(Subcategory) %>% 
    mutate(
      Narrative = case_when(
        Subcategory == "Interception" | Subcategory == "Tackle" ~ paste("Int/Tkl"), 
        TRUE ~ "Other Scenarios"
      )
    ) %>% 
    select(Narrative) %>% 
    count(Narrative)
}

#Dribbles Function
calculate.dribbles <- function(x) {
  dribbles.table <- x %>% 
    filter(Subcategory == 'Dribble') %>%
    select(Subcategory, Result) %>%
    group_by(Subcategory, Result) %>%
    count(Result)
}

#Carries Function
calculate.carries <- function(x) {
  carries.table <- x %>% 
    filter(Subcategory == 'Carry') %>%
    select(Subcategory) %>%
    group_by(Subcategory) %>%
    count(Subcategory)
}

graphs <- c("shot_map", "pass_map", "touch_map")

# Define UI for random distribution app ----
ui <- fluidPage(
  #App Theme ----
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  # App title ----
  titlePanel("Player Evaluation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2, # Adjust the width of the sidebar
      
      # Input: Select the tournament, date, team and player to filter ----
      selectInput("tournament", "Select Tournament", choices = unique(df$Tournament)),
      selectInput("tarehe", "Select Date", choices = unique(df$Date)),
      selectInput("team", "Select Team", choices = unique(df$Team)),
      selectInput("player", "Select Player", choices = unique(df$Player)),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Checkbox for the user defined plots ----
      checkboxGroupInput("plot_options", "Select Plots to Display", choices = graphs),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      width = 10, # Adjust the width of the main panel
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           tableOutput("minutes_"),
                           tableOutput("goals"),
                           tableOutput("shots"),
                           tableOutput("fouls"),
                           tableOutput("cards"),
                           tableOutput("duels"),
                           tableOutput("recoveries"),
                           tableOutput("dribbles"),
                           tableOutput("carries")),
                  tabPanel("Plot", 
                           plotOutput("viz"),
                           plotOutput("viz2"),
                           plotOutput("viz3")
                           ),
                  tabPanel("Match Events", dataTableOutput("details")),
                  )
                  
      )
    )
  )

# Define server logic 
server <- function(input, output, session) {
  #Observer to update tournament selection
  
  observeEvent(input$tournament,{
    tournament <- input$tournament
    match_options <- unique(df[which(df$Tournament == tournament),]$Date)
    
    updateSelectInput(session, inputId = "tarehe",
                      choices = match_options)
  })
  
  #Observer to update date selection
  
  observeEvent(input$tarehe,{
    tournament <- input$tournament
    tarehe <- input$tarehe
    team_options <- unique(df[which(df$Tournament == tournament & df$Date == tarehe),]$Team)
    
    updateSelectInput(session, inputId = "team",
                      choices = team_options)
  })
  
  #Observer to update team selection
  
  observeEvent(input$team,{
    tournament <- input$tournament
    tarehe <- input$tarehe
    team <- input$team
    player_options <- unique(df[which(df$Tournament == tournament & df$Date == tarehe & df$Team == team),]$Player)
    
    updateSelectInput(session, inputId = "player",
                      choices = player_options)
  })
  
  #Observer to update player selection
  
  observeEvent(input$player, {
    player <- input$player
  })
  
  #Reactive data frame for Tournament selection
  tournaments <- reactiveValues()
  observe({
    tournaments$df <- df[which(df$Tournament == input$tournament),]
  })
  
  #Reactive data frame for match selection
  tarehes <- reactiveValues()
  observe({
    tarehes$df <- df[which(df$Tournament == input$tournament & df$Date == input$tarehe),]
  })
  
  #Reactive data frame for team selection
  teams <- reactiveValues()
  observe({
    teams$df <- df[which(df$Tournament == input$tournament & df$Date == input$tarehe & df$Team == input$team),]
  })
  
  #Reactive data frame for player selection
  values <- reactiveValues()
  observe({
    values$df <- df[which(df$Tournament == input$tournament & df$Date == input$tarehe & df$Team == input$team & df$Player == input$player),]
  })
 
  #Minutes Played
  output$minutes_ <- renderTable({
    marker.data <- tarehes$df %>%
      filter(Subcategory == 'Full Time')
    events.data <- values$df %>%
      filter(Subcategory == 'Sub Out' | Subcategory == 'Sub In')
    #bind marker.data and events.data to a new data frame(full.data)
    full.data <- rbind(events.data, marker.data)
    #calculate minutes played on full.data
    full.data %>%
      mutate(
        minutes = case_when(
          all(Subcategory == 'Full Time') ~ Mins,
          all(Subcategory %in% c('Full Time', 'Sub In')) ~ last(Mins[Subcategory == 'Full Time']) - last(Mins[Subcategory == 'Sub In']),
          all(Subcategory %in% c('Full Time', 'Sub Out')) ~ last(Mins[Subcategory == 'Sub Out']),
          all(Subcategory %in% c('Sub In', 'Sub Out', 'Full Time')) ~ last(Mins[Subcategory == 'Sub Out']) - last(Mins[Subcategory == 'Sub In']),
          TRUE ~ NA_real_  # Default action for unmatched cases
        )
      ) %>%
      select(minutes) %>%
      distinct(minutes, .keep_all = TRUE)
  })

  
  #SHOTS INFO
  output$goals <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.goals()
  })
  
  #SHOTS INFO
  output$shots <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.shots()
  })
  
  #FOULS INFO
  output$fouls <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.fouls()
  })
  
  #CARDS INFO
  output$cards <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.cards()
  })
  
  #PASS INFO
  output$duels <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.duels()
  })
  
  #RECOVERIES INFO
  output$recoveries <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.recoveries()
  })
  
  #DRIBBLES INFO
  output$dribbles <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.dribbles()
  })
  
  #CARRIES INFO
  output$carries <- renderTable({
    events.data <- values$df
    events.data %>%
      calculate.carries()
  })
  
  #DETAILS TABLE - Display specific events
  
  output$details <- renderDataTable({
    events.data <- values$df
    events.data %>%
      display.details()
  },
  options = list(
    pageLength = 5))
  
  #PLOT OPTIONS ---
  
  # Plotting Shot Maps, Pass Maps, Touch Maps ----
  
  observe({
    selected_plots <- input$plot_options
    
    # Render Shots Plot if selected
    if ("shot_map" %in% selected_plots) {
      output$viz <- renderPlot({
        events.data <- values$df %>%
          filter(Category == 'Shot') %>%
          select(Date, Opponent, Player, Category, Subcategory, Result, period, Timestamp, X, Y)
        
        ggplot(events.data, aes(x=X, y=Y, color = Subcategory)) +
          annotate_pitch(fill = 'darkgrey', colour = 'white') +
          geom_point() +
          theme_pitch()
      })
    } else {
      output$viz <- renderPlot({ NULL }) # Render a blank plot if not selected
    }
    
    # Render Pass Map if selected
    if ("pass_map" %in% selected_plots) {
      output$viz2 <- renderPlot({
        events.data <- values$df %>%
          filter(Category == 'Pass') %>%
          select(Date, Opponent, Player, Category, Subcategory, Result, period, Timestamp, X, Y, X2, Y2)
        
        ggplot(events.data, aes(x=X, y=Y, xend = X2, yend = Y2, color = Result)) +
          annotate_pitch(fill = 'darkgrey', colour = 'white') +
          geom_segment() +
          theme_pitch()
      })
    } else {
      output$viz2 <- renderPlot({ NULL }) # Render a blank plot if not selected
    }
    
    # Render Touch Map if selected
    if ("touch_map" %in% selected_plots) {
      output$viz3 <- renderPlot({
        events.data <- values$df %>%
          filter(Category == 'Shot' | Category == 'Pass'| Category == 'Duel' | Category == 'Offense' | Category == 'Defense' | Subcategory == 'Offside') %>%
          mutate(Narrative = case_when(
            Category == "Pass" ~ paste(Category),
            Category == "Shot" ~ paste(Category),
            Category == "Duel" ~ paste(Category),
            Category == "Offense" ~ paste(Subcategory),
            Category == "Defense" ~ paste(Subcategory),
            Subcategory == "Offside" ~ paste(Subcategory),
            TRUE ~ "Other Scenarios" # Default narrative for all other cases
          )) %>%
          select(Narrative, Timestamp, X, Y)
        
        ggplot(events.data, aes(x=X, y=Y, color = Narrative)) +
          annotate_pitch(fill = 'darkgrey', colour = 'white') +
          geom_point() +
          theme_pitch()
      })
    } else {
      output$viz3 <- renderPlot({ NULL }) # Render a blank plot if not selected
    }
  })
  
  # observeEvent(input$plot_options, {
  #   # Check which plots were selected
  #   selected_plots <- input$plot_options
  #   
  #   # Use updateTabsetPanel to dynamically show/hide the plots
  #   if ("shot map" %in% selected_plots) {
  #     updateTabsetPanel(session, "plot_tab", selected = "viz")
  #   }
  #   if ("pass map" %in% selected_plots) {
  #     updateTabsetPanel(session, "plot_tab", selected = "viz2")
  #   }
  #   if ("touch map" %in% selected_plots) {
  #     updateTabsetPanel(session, "plot_tab", selected = "viz3")
  #   }
  # })
  # 
  # 
  # #SHOTS PLOT ----
  # output$viz <- renderPlot({
  #   events.data <- values$df %>%
  #     filter(Category == 'Shot') %>%
  #     select(Date, Opponent, Player, Category, Subcategory, Result, period, Timestamp, X, Y)
  # 
  #   ggplot(events.data, aes(x=X, y=Y, color = Subcategory)) +
  #     annotate_pitch(fill = 'darkgrey', colour = 'white') +
  #     geom_point() +
  #     theme_pitch()
  # })
  # 
  # #PASS MAP
  # output$viz2 <- renderPlot({
  #   events.data <- values$df %>%
  #     filter(Category == 'Pass') %>%
  #     select(Date, Opponent, Player, Category, Subcategory, Result, period, Timestamp, X, Y, X2, Y2)
  # 
  #   ggplot(events.data, aes(x=X, y=Y, xend = X2, yend = Y2, color = Result)) +
  #     annotate_pitch(fill = 'darkgrey', colour = 'white') +
  #     geom_segment() +
  #     theme_pitch()
  # })
  # 
  # #TOUCH MAP
  # output$viz3 <- renderPlot({
  #   events.data <- values$df %>%
  #     filter(Category == 'Shot' | Category == 'Pass'| Category == 'Duel' | Category == 'Offense' | Category == 'Defense' | Subcategory == 'Offside') %>%
  #     mutate(Narrative = case_when(
  #       Category == "Pass" ~ paste(Category),
  #       Category == "Shot" ~ paste(Category),
  #       Category == "Duel" ~ paste(Category),
  #       Category == "Offense" ~ paste(Subcategory),
  #       Category == "Defense" ~ paste(Subcategory),
  #       Subcategory == "Offside" ~ paste(Subcategory),
  #       TRUE ~ "Other Scenarios" # Default narrative for all other cases
  #     )) %>%
  #     select(Narrative, Timestamp, X, Y)
  # 
  #   ggplot(events.data, aes(x=X, y=Y, color = Narrative)) +
  #     annotate_pitch(fill = 'darkgrey', colour = 'white') +
  #     geom_point() +
  #     theme_pitch()
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
