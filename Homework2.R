library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Function to load dataset from public link
load_dataset <- function() {
  dataset_url <- 'https://raw.githubusercontent.com/Zachary1441/Homework2Stat436/main/ncaab.csv'
  dataset <- read_csv(dataset_url)
  dataset <- dataset %>%
    filter(YEAR != 2020)%>% # No Tournament this year
    rename(FieldGoal_2P_O = `2P_O`)%>% # I was having issues with these names and Shiny not finding them in the dataset
    rename(FieldGoal_2P_D = `2P_D`)%>%
    rename(FieldGoal_3P_O = `3P_O`)%>%
    rename(FieldGoal_3P_D = `3P_D`)
  return(dataset)
}
# Function to generate scatter plot
generate_scatterplot <- function(data, x_var, y_var, highlight_var, season) {
  data_season <- data %>% filter(YEAR == season)
  p <- ggplot(data_season, aes_string(x = x_var, y = y_var, color = highlight_var)) +
    geom_point() +
    labs(title = paste("Scatter Plot of", x_var, "vs", y_var, "for Season", season),
         x = x_var, y = y_var) +
    theme_minimal()+
    theme(plot.background = element_rect(fill = "black"),
          text = element_text(color = "white", size = 14),
          axis.text = element_text(color = "white"),  # Set axis text color to white
          axis.ticks = element_line(color = "white"))
  
  return(p)
}

# Function to generate bar plot
generate_barplot <- function(data, season) {
  # Filter data by season and calculate wins per seed
  wins_per_seed <- data %>%
    filter(YEAR == season) %>%
    group_by(SEED) %>%
    summarise(total_wins = sum(
      case_when(
        is.na(POSTSEASON) | POSTSEASON == "R64" ~ 0,
        POSTSEASON == "R32" ~ 1,
        POSTSEASON == "S16" ~ 2,
        POSTSEASON == "E8" ~ 3,
        POSTSEASON %in% c("F4", "2nd") ~ 4,
        POSTSEASON == "Champion" ~ 5,
        TRUE ~ 0  # Handle any other cases as 0
      )
    ))
  
  # Create bar plot
  p <- ggplot(wins_per_seed, aes(x = factor(SEED), y = total_wins)) +
    geom_bar(stat = "identity", fill = "gold") +
    labs(title = paste("Total Wins per Seed in Season", season),
         x = "Seed", y = "Total Wins") +
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, max(wins_per_seed$total_wins) + 1, by = 1)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),  # Set x-axis text color to white
          plot.background = element_rect(fill = "black"),
          text = element_text(color = "white", size = 14),
          axis.text = element_text(color = "white"),
          axis.ticks = element_line(color = "white"))
  
  return(p)
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;  /* Set text color to white for better visibility */
      }
      .plot-container {
        background-color: black;  /* Adjust plot container background */
      }
      .control-label {
      color: black !important; /* Change to the desired text color */
    }
    "))
  ),
  titlePanel("College Basketball Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season:", choices = unique(dataset$YEAR)),
      selectInput("x_var", "X Variable:", choices = c("ADJDE", "FieldGoal_2P_D",
                                                      "FieldGoal_3P_D", "DRB", 
                                                      "TORD", "W", "EFG_D" )),
      selectInput("y_var", "Y Variable:", choices = c("ADJOE", "FieldGoal_2P_O",
                                                      "FieldGoal_3P_O", "ORB", 
                                                      "TOR", "ADJ_T", "EFG_O")),
      selectInput("highlight_var", "Highlight Variable:", choices = c("POSTSEASON", "CONF")),
      actionButton("update_plot", "Update Plot")
    ),
    mainPanel(
      plotOutput("scatterplot"),
      uiOutput("scatter_description"),
      plotOutput("barplot"),
      uiOutput("barplot_description")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load dataset
  dataset <- reactive({
    load_dataset()
  })
  output$scatterplot <- renderPlot({
    generate_scatterplot(dataset(), input$x_var, input$y_var, input$highlight_var, input$season)
  })
  # Update scatter plot
  observeEvent(input$update_plot, {
    output$scatterplot <- renderPlot({
      generate_scatterplot(dataset(), input$x_var, input$y_var, input$highlight_var, input$season)
    })
  })
  output$scatter_description <- renderUI({
    HTML("<div style='color: white; font-size: 14px;'>
          <p>This scatter plot shows the relationship between the selected variables.</p>
          <p>Each point represents a team, and the color represents the selected highlighting variable.</p>
          <p>It can allow the user to see any relationships with selected variables between conferences or tournament wins for teams.<p>
         </div>")
  })
  
  # Update bar plot
  output$barplot <- renderPlot({
    generate_barplot(dataset(), input$season)
  })
  output$barplot_description <- renderUI({
    HTML("<div style='color: white; font-size: 14px;'>
          <p>This bar plot displays the total wins per seed for the selected season.</p>
          <p>Each bar represents a seed, and the height of the bar represents the total number of wins for teams with that seed.</p>
          <p>It allows the user to see the change in the spread of wins throughout the seasons.<p>
         </div>")
  })
}

# Run the application
shinyApp(ui = ui, server = server)