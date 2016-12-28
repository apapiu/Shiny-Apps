library(shiny)
library(shinythemes)
library(ggplot2)
library(data.table)
library(dplyr)
library(highcharter)

descp = "Welcome to Airbnb NYC! This is a shiny app where you can explore prices!"


train = fread("listings_2.csv")

nbds = unique(train$neighbourhood_cleansed)
types = unique(train$room_type)

ui <- fluidPage(theme = shinytheme("flatly"),
                
                fluidRow(
                    column(
                        verticalLayout(
                            
                            wellPanel(descp),
                            
                            wellPanel(selectInput(inputId = "nbd", " ", choices = nbds),
                                      selectInput(inputId = "type", " ", choices = types))
                            ) , width = 3),
                    
                    
                    column(
                        highchartOutput("plot", width = "800px", height = "800px"), width = 8)
                )
)


server <- function(input, output) {
    
    output$plot <- renderHighchart({
        
        train[neighbourhood_cleansed == input$nbd][
            room_type == input$type]$price %>% 
            hchart(xlim = c(0, 1000), color = "#003200") %>% 
            hc_add_theme(hc_theme_smpl())
        
    })
}

shinyApp(ui = ui, server = server)