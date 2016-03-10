library(shiny)
library(shinythemes)
library(babynames)
library(data.table)

ui <- fluidPage( theme = shinytheme("flatly"),
    sidebarLayout(
        sidebarPanel( textInput(inputId = "name1", "Pick up to 5 Names Below:"),
                      textInput(inputId = "name2", " "),
                      textInput(inputId = "name3", " "),
                      textInput(inputId = "name4", " "),
                      textInput(inputId = "name5", " "),
                      actionButton("click", "Fight!"), width = 2),
        
        mainPanel(plotOutput("plot2", width = "600px", height = "400px")
                  #streamgraphOutput("plot1")

        )
    )
)


server <- function(input, output) {
    
    observeEvent(input$click, {
        
    output$plot1 <- renderStreamgraph({
        babynames[name %in% c(input$name1, input$name2, input$name3,input$name4, input$name5
                              )][sex == "M"][year >=1930] %>% 
            streamgraph(name, n, year) %>% 
            sg_fill_brewer("Set1")
    })
    output$plot2 <- renderPlot({
        
        babynames[name %in% c(input$name1, input$name2, input$name3, input$name4, input$name5
        )][sex == "M"][year >=1880] %>% 
            ggplot(aes(x = year, y = prop*100, colour = name)) +
            geom_line(size = .7) + theme_minimal(base_size = 10) + scale_colour_brewer(palette = "Set1") +
            ylab("Percentage") + xlab("Year")
    })
    
    })
}

shinyApp(ui = ui, server = server)
