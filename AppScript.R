library(tidyverse)
library(shiny)
library(plotly)
library(shinythemes)
fifa_full <- read_csv("data/fifa_15_23.csv")
variable_choices <- names(fifa_full)[19:24]

ui <- fluidPage(
  titlePanel("FIFA Career Mode Comparison App"),
  theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel("Line Graph",
             sidebarPanel(
               selectInput("yvar", label = "Choose a Variable", choices = variable_choices, selected = "pace"),
               selectInput("league_choice", label = "League Filter",
                           choices = c("All", unique(fifa_full$league_name)), selected = "All"),
               selectInput("team_choice", label = "Team Filter",
                           choices = c("All", unique(fifa_full$club_name)), selected = "All"),
               selectInput("nationality_choice", label = "Nationality Filter",
                           choices = sort(c("All", unique(fifa_full$nationality_name))), selected = "All"),
               selectInput(inputId = "player_choice", label = "Choose player(s)",
                           choices = NULL, selected = NULL, multiple = TRUE)
             ),
             mainPanel(plotlyOutput(outputId = "line_graph"),
                       tableOutput(outputId = "summary_table1")
             )),
    tabPanel("Bar Graph",
             mainPanel(plotlyOutput(outputId = "bar_graph"),
                       selectInput("fifa_year", label = "Choose the Year", choices = 15:23, selected = 23),
                       tableOutput(outputId = "summary_table2")
             )),
    tabPanel("Player Guess",
             sidebarPanel(actionButton("generate_player", "Generate New Random Player"),
                          actionButton("show_hint", "Hint"),
                          textOutput("hint_text1"),
                          textOutput("hint_text2"),
                          textOutput("hint_text3"),
                          textOutput("hint_text4"),
                          textOutput("hint_text5"),
                          textOutput("hint_text6")
             ),
             mainPanel(tableOutput(outputId = "player_summary"),
                       textOutput("hint_text7")
             )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(c(input$league_choice, input$nationality_choice, input$team_choice), {
    player_choices <- fifa_full|> distinct(short_name) |> pull(short_name)
    
    if (input$league_choice != "All" & input$nationality_choice == "All" & input$team_choice == "All") {
      player_choices <- fifa_full |> filter(league_name == input$league_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if (input$nationality_choice != "All" & input$league_choice == "All" & input$team_choice == "All") {
      player_choices <- fifa_full |> filter(nationality_name == input$nationality_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if (input$team_choice != "All" & input$nationality_choice == "All" & input$league_choice == "All") {
      player_choices <- fifa_full |> filter(club_name == input$team_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if(input$league_choice != "All" & input$nationality_choice != "All" & input$team_choice == "All") {
      player_choices <- fifa_full|> filter(league_name == input$league_choice, nationality_name == input$nationality_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if(input$nationality_choice != "All" & input$team_choice != "All" & input$league_choice == "All"){
      player_choices <- fifa_full |> filter(club_name == input$team_choice, nationality_name == input$nationality_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if( input$league_choice != "All" & input$team_choice != "All" & input$nationality_choice == "All"){
      player_choices <- fifa_full |> filter(club_name == input$team_choice, league_name == input$league_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    if(input$league_choice != "All" & input$nationality_choice != "All" & input$team_choice != "All"){
      player_choices <- fifa_full |> filter(club_name == input$team_choice, league_name == input$league_choice, nationality_name == input$nationality_choice) |> distinct(short_name) |> pull(short_name)
    }
    
    updateSelectInput(inputId = "player_choice",
                      choices = sort(player_choices))
  })
  
  observeEvent(input$league_choice, {
    player_choices <- fifa_full |> distinct(short_name) |> pull(short_name)
    
    if (input$league_choice != "All") {
      team_choices <- fifa_full |> 
        filter(league_name == input$league_choice) |> 
        distinct(club_name) |> 
        pull(club_name)
    } else {
      team_choices <- unique(fifa_full$club_name)
    }
    
    updateSelectInput(inputId = "team_choice", choices = sort(c("All", team_choices)), selected = "All")
  })
  
  fifa_line_react <- reactive({
    fifa_full |> filter(short_name %in% input$player_choice)|>
      group_by(short_name, fifa_version)|>
      summarise(overall = median(overall, na.rm = TRUE),
                pace = median(pace, na.rm = TRUE),
                shooting = median(shooting, na.rm = TRUE),
                passing = median(passing, na.rm = TRUE),
                dribbling = median(dribbling, na.rm = TRUE),
                defending = median(defending, na.rm = TRUE),
                physic = median(physic, na.rm = TRUE))
  })
  
  fifa_bar_react <- reactive({
    fifa_full|>
      filter(short_name %in% input$player_choice, fifa_version == input$fifa_year)|>
      pivot_longer(cols = 19:24, names_to = "stat", values_to = "value")|>
      group_by(short_name, stat)|>
      summarise(value = round(median(value)), 
                overall = round(median(overall)))
  })
  
  i <- reactiveVal(0)
  
  random_react <- eventReactive(input$generate_player, {
    output$hint_text1 <- renderText(NULL)
    output$hint_text2 <- renderText(NULL)
    output$hint_text3 <- renderText(NULL)
    output$hint_text4 <- renderText(NULL)
    output$hint_text5 <- renderText(NULL)
    output$hint_text6 <- renderText(NULL)
    output$hint_text7 <- renderText(NULL)
    
    i(0)
    
    fifa_full |>
      sample_n(1)
    
  })
  
  output$player_summary <- renderTable({
    if (!is.null(random_react())) {
      random_react() |>
        select(6, 11, 12, 16:24)
    }
  })
  
  
  observeEvent(input$show_hint, {
    if (!is.null(random_react()) & i() == 0) {
      output$hint_text1 <- renderText({
        paste0("FIFA Version: ", random_react()$fifa_version)
      })
      i(1)
    }else if(i() == 1){
      output$hint_text2 <- renderText({
        paste0("Player Positions: ", random_react()$player_positions)
      })
      i(2)
    }else if(i() == 2){
      output$hint_text3 <- renderText({
        paste0("Nationality: ", random_react()$nationality_name)
      })
      i(3)
    }else if(i() == 3){
      output$hint_text4 <- renderText({
        paste0("League Name: ", random_react()$league_name)
      })
      i(4)
    }else if(i() == 4){
      output$hint_text5 <- renderText({("*FINAL HINT*")})
      output$hint_text6 <- renderText({
        paste("Club Name: ", random_react()$club_name)
      })
      i(5)
    }else if(i() == 5){
      output$hint_text7 <- renderText({
        paste0("Player: ", random_react()$long_name)
      })
      i(6)
    }
    
  })
  
  output$line_graph <- renderPlotly({
    req(input$player_choice)
    plot1 <- ggplot(data = fifa_line_react(),
                    aes(x = fifa_version, y = .data[[input$yvar]], color = short_name)) +
      geom_point(aes(text = paste(input$yvar,":", .data[[input$yvar]], "<br>overall:", overall)))+
      geom_line() +
      labs(title = "Player Comparison Over Time", x = "Year")+
      scale_x_continuous(breaks = c(15, 16, 17, 18, 19, 20, 21, 22, 23))+
      scale_color_brewer(palette = "Set1")
    
    ggplotly(plot1, tooltip = "text")
  })
  
  output$bar_graph <- renderPlotly({
    req(input$player_choice, input$fifa_year)
    plot2 <- ggplot(fifa_bar_react(), 
                    aes(x = stat, y = value, group = short_name, fill = short_name, label = value))+
      geom_col(position = "dodge", color = "black")+
      labs(x = "Statistics", y = input$fifa_year, title = glue::glue("FIFA ", input$fifa_year, " Comparison"))+
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(plot2, tooltip = "label")
    
    
  })
  
  output$summary_table1 <- renderTable({
    fifa_full |>
      filter(short_name %in% input$player_choice) |>
      group_by(short_name)|>
      summarise(`avg overall` = mean(overall, na.rm = TRUE),
                `avg pace` = mean(pace, na.rm = TRUE),
                `avg shooting` = mean(shooting, na.rm = TRUE),
                `avg passing` = mean(passing, na.rm = TRUE),
                `avg dribbling` = mean(dribbling, na.rm = TRUE),
                `avg defending` = mean(defending, na.rm = TRUE),
                `avg physical` = mean(physic, na.rm = TRUE))
    
  })
  output$summary_table2 <- renderTable({
    fifa_full |>
      filter(short_name %in% input$player_choice, fifa_version == input$fifa_year) |>
      group_by(short_name)|>
      summarise(overall = round(median(overall)),
                pace = round(median(pace)),
                shooting = round(median(shooting)),
                passing = round(median(passing)),
                dribbling = round(median(dribbling)),
                defending = round(median(defending)),
                physical = round(median(physic)))
  })
  
  
}

shinyApp(ui, server)

library(rsconnect)
deployApp()
