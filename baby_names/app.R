library(shiny)
library(babynames)
library(data.table)
library(streamgraph)
library(dygraphs)
library(tidyr)

ui <- fluidPage(
            
    titlePanel("Baby Name Explorer"),
                 
    sidebarLayout(
        sidebarPanel( helpText("Pick up to five names below and explore their popularity as baby names over time:"),
                      textInput(inputId = "name1", " ", value = "Judy"),
                      textInput(inputId = "name2", " ", value = "Emma"),
                      textInput(inputId = "name3", " ", value = "Samantha"),
                      textInput(inputId = "name4", " "),
                      textInput(inputId = "name5", " "),
                      radioButtons(inputId = "sex",label = "", 
                                         choices = c( "Female" = "F", "Male" = "M")),

                      width = 2),
        
        mainPanel(dygraphOutput("plot2", width = "1100px", height = "600px"),
                  streamgraphOutput("plot1")

        )
    )
)

server <- function(input, output) {
    
    #data <- reactive({
    #
    #   data.table(babynames)[name %in% c(input$name1, input$name2, input$name3,input$name4, 
    #                                        input$name5)][sex %in% input$sex][year >=1930]
    #   })

    #observeEvent(input$type, 
    #output$plot1 <- renderStreamgraph({
    #    data.table(babynames)[name %in% c(input$name1, input$name2, input$name3,input$name4, 
    #                                      input$name5)][sex %in% input$sex][,prop := NULL][,sex := NULL][year > 1910] %>% 
    #    streamgraph(name, n, year) %>% 
    #    sg_fill_brewer("Set1")
        
    #})
    #)
    
    
        
    output$plot2 <- renderDygraph({
        
        data.table(babynames)[name %in% c(input$name1, input$name2, input$name3,input$name4, 
                                          input$name5)][sex %in% input$sex][,prop := NULL][,sex := NULL][year > 1910] %>% 
            spread(key = name, value = n) %>% 
            dygraph() %>%  
            dyAxis("y",label = "Baby Count", labelWidth = 14 ) %>% 
            dyAxis("x", drawGrid = FALSE , label = "Year of Birth", labelHeight = 14) %>% 
            dyOptions(axisLineWidth = .5, strokeWidth = 1.5, axisTickSize = 10, 
                      axisLabelFontSize = 12, gridLineColor = "lightgrey",
                      axisLabelColor = "grey", axisLineColor = "grey", 
                      colors = RColorBrewer::brewer.pal(5, "Dark2"))
        })
}

shinyApp(ui = ui, server = server)
