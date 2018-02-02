library(shiny)
library(tidyverse)
library(DT)
library(stringr)
library(tools)
library(reprex) # needed for deploying app, not working
library(callr)  # needed for deploying app, not working

load("movies.Rdata")


# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Application title -----------------------------------------------
  titlePanel("Movie browser"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis ----------------------------------
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB num of votes" ="imdb_num_votes", 
                              "Critic's score" = "critics_score", 
                              "Audience score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "audience_score"),
      
      # Select variable for x-axis ----------------------------------
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("IMDB rating" = "imdb_rating", 
                              "IMDB num of votes" ="imdb_num_votes", 
                              "Critic's score" = "critics_score", 
                              "Audience score" = "audience_score", 
                              "Runtime" = "runtime"), 
                  selected = "critics_score"),
     
      # Select variable for z-axis ----------------------------------
      selectInput(inputId = "z", 
                  label = "Color by",
                  choices = c("title_type", "genre", "mpaa_rating", "critics_rating", "audience_rating"), 
                  selected = "mpaa_rating"),
      
      # creating a slider input for point coloring
      sliderInput(inputId = "alpha", 
                  label = "Transparency",
                  min = 0, max = 1, 
                  value = 0.7, step = 0.1)
      
      ),
  #   
  # # Add checkbox
  #   checkboxInput(inputId = "showdata",
  #                 label = "Show data table",
  #                 value = TRUE)
  # ),
    
  # Output: 
    #Show scatterplot --------------------------------------
      mainPanel(
        plotOutput(outputId = "scatterplot"),
   
    # Show data table ---------------------------------------------  
        DT::dataTableOutput(outputId = "moviestable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
# Create scatterplot object the plotOutput function in the UI is expecting --
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha) + theme_bw() +
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),   # awesome options from Stringr package
           y = toTitleCase(str_replace_all(input$y, "_", " ")),
           color = toTitleCase(str_replace_all(input$z, "_", " "))) 
  })

# data table output the datatableOutput function in the UI is expecting -------------------------------------
  output$moviestable <- DT::renderDataTable({
      DT::datatable(data = movies[, 1:7], 
                    options = list(pageLength = 10), 
                    rownames = FALSE) 
    
    # # Print data table if checkbox is desired -------------------------------------
    # output$moviestable <- DT::renderDataTable({
    #   print(input$showdata)
    #   if(input$showdata == TRUE){
    #     DT::datatable(data = movies[, 1:7], 
    #                   options = list(pageLength = 10), 
    #                   rownames = FALSE) 
    #   }
    # })
    
    }
  )
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)