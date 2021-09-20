

# Packages
library(shiny)
library(tidyverse)
library(ggimage)
library(extrafont)

# Function scripts
source("Rscripts/draw.R")
source("Rscripts/grid.R")
source("Rscripts/lookup.R")

# UI function
ui <- fluidPage(

    # Application title
    h1(id = "titre-app", "Minesweeper: clear the field from all bombs!"),
    
    # Select the wanted grid
    # For now I didn't put that many rows and columns because I'm new and don't know how to scale properly the app on a window
    fluidRow(
        id = "select-inputs",
        h2(id = "titre-inputs", "Settings of the game"),
        column(3, 
               sliderInput(inputId = "lignes", label = "Number of desired rows:", 
                            value = 10, min = 1, max = 50)),
        column(3, 
               sliderInput(inputId = "colonnes", label = "Number of desired columns:", 
                            value = 10, min = 1, max = 80)),
        column(3, 
               sliderInput(inputId = "bombes", label = "Number of desired bombs:", 
                            value = 10, min = 1, max = 100)),
        column(3,
               actionButton(inputId = "start", label = "Let's GO!"))
    ),
    
    # The grid to play and information of the ongoing game, and the grid for the game
    fluidRow(
        column(3,
               id = "infos-game",
               htmlOutput(outputId = "info_game"),
               br(),
               htmlOutput(outputId = "field_to_clear"),
               actionButton(inputId = "clear", label = "Clear this field!")),
        
        column(8,
               plotOutput(outputId = "demineur", click = "plot_click"))
    ),
    
    # Basic CSS styling (I might have to learn a lot for further results)
    tags$style(
        HTML("h1 {
             color: #870637; 
             font-size: 4em; 
             font-family: Onyx; 
             text-align: center;
             font-weight: bold;
        }"), # H1 headers
        HTML("h2 {
             color: #870637; 
             font-size: 2.5em; 
             font-family: Onyx;
             text-indent: 50px;
             text-decoration: underline;
        }"), # H2 headers
        HTML(".control-label {
             color: #870637;
             font-family: Onyx;
             font-size: 2em;
             font-weight: normal;
        }"), # Labels of the input
        HTML("#select-inputs {
             border: 2px dashed #0A2985;
             background-color: #85B9F1;
             margin: 0px 5px;
        }"), # Background of the 1st part of the app
        HTML("#start:hover {opacity: 1}"), # Hover on the button that fade
        HTML("#start {
             background-color: #0E6F02;
             border: 1px solid #14850A;
             color: white;
             padding: 4px;
             text-align: center;
             font-size: 4em;
             font-family: Onyx;
             margin: 10px;
             opacity: 0.4;
             transition: 0.3s;
             cursor: pointer;
        }"), # Custom the button
        HTML("#clear:hover {opacity: 1}"), # Hover on the button that fade
        HTML("#clear {
             background-color: #1B35CC;
             border: 1px solid #180658;
             color: white;
             padding: 1px;
             text-align: center;
             font-size: 1.5em;
             font-family: Onyx;
             margin: 2px;
             opacity: 0.4;
             transition: 0.3s;
             cursor: pointer;
        }"), # Custom the button
        HTML("#infos-game {
             background-color: #B0CC99;
             margin-top: 20px;
             border: 1px solid #002F2F;
             padding: 8px;
             text-align: center;
             color: #870637;
             font-size: 1.5em;
             margin: 10px;
             font-family: Onyx;
        }") # Put some space between the 2 parts of the app and custom the background
    )        
)

# Server function
server <- function(input, output) {
    
    selected_field <- reactiveVal("No field selected!")
    selected_row <- reactiveVal(NULL)
    selected_col <- reactiveVal(NULL)
    
    # The maximum number of bombs is 1 bomb in each field of the board
    observeEvent(
        eventExpr = {input$lignes;input$colonnes},
        handlerExpr = {updateNumericInput(inputId = "bombes", max = input$lignes * input$colonnes)}
    )
    
    # Fix the number of rows and columns even if the inputs are changed but the button isn't pressed
    Nrows <- eventReactive(
        eventExpr = input$start,
        valueExpr = input$lignes
    )
    Ncols <- eventReactive(
        eventExpr = input$start,
        valueExpr = input$colonnes
    )
    Nbooms <- eventReactive(
        eventExpr = input$start,
        valueExpr = input$bombes
    ) 
    
    # Reset the selected field at the start of the game
    observeEvent(
        eventExpr = input$start,
        handlerExpr = {
            selected_field("No field selected!")
            selected_row(NULL)
            selected_col(NULL)
        }
    )
    
    # Generate the data
    donnees <- eventReactive(
        eventExpr = input$start,
        valueExpr = create_grid(Nrows(), Ncols(), Nbooms())
    )
    tab_donnees <- reactive(compte_voisins(as.character(donnees()), Nrows(), Ncols()))
    df_donnees <- reactive(tableau_compte_init(donnees(), tab_donnees()))
    
    # When you click on the screen, locate the targetted field
    observeEvent(
        eventExpr = input$plot_click,
        handlerExpr = {
            selected_field(paste0("Field (Line ", round(Nrows() + 1 - input$plot_click$y, 0), ";Column ", round(input$plot_click$x, 0), ")"))
            selected_row(round(input$plot_click$y, 0))
            selected_col(round(input$plot_click$x, 0))
        }
    )
    
    # observeEvent(
    #     eventExpr = input$clear,
    #     handlerExpr = {
    #         rep(input$)
    #     }
    # )
    
    output$field_to_clear <- renderText(selected_field())
    
    output$table_data <- renderTable(donnees())
    
    output$demineur <- renderPlot({
        draw_board(df_donnees(), selected_row(), selected_col())
    },
    height = function() 25 * Nrows())
    
}

# Run the application 
shinyApp(ui = ui, server = server)
