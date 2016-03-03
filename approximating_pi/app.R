library(shiny)
library(shinythemes)
library(ggplot2)
library(rsconnect)
library(plotly)


ui <- fluidPage( theme = shinytheme("flatly"),
    
    titlePanel("Approximating Pi"),
                
    fluidRow( column(wellPanel("We will aproximate Pi by generating random points
                               in a square and then counting what fraction of those
                               points are inside the circle contained in the square.
                               You can think of this as randomly throwing darts at a square board.
                               Since we know what the area of a square is we've 
                               replaced a calculus problem with a counting problem! Play around with the slider on the
                               right - you should get a beter approximation of Pi the more points you use."), width = 5),
              
               column(wellPanel(sliderInput(inputId = "num",
                            label = "Number of Points",
                            value = 5, min = 10, max = 20000)), width = 6)),
              
    fluidRow(column( "Your approximation of pi is:",
                         verbatimTextOutput("pi"),width = 5)),
                
               splitLayout(
                    plotlyOutput("plot", width = "600px", height = "600px"),
                    plotlyOutput("plotly", width = "600px", height = "600px"))
            )


server <- function(input, output) {
    output$pi <- renderPrint({
        x <- replicate(2, runif(n = input$num ,min = -1, max = 1))
        
        inside <- (apply(x, 1, function(x) {x[1]^2 +x[2]^2}))<=1
        
        y <- data.frame(x, inside)
        
        4 *sum(inside)/input$num
    })
    
    output$plot <- renderPlotly({
        x <- replicate(2, runif(n = input$num ,min = -1, max = 1))
        
        inside <- (apply(x, 1, function(x) {x[1]^2 +x[2]^2}))<=1
        
        y <- data.frame(x, inside)
        
        
        ggplotly(qplot(y[,1], y[,2], color = inside, xlab = "x", ylab = "y") +
            scale_colour_manual(values=c("#9999CC", "#66CC99")) + 
                                    theme(legend.position="none"))
    })
    
    output$plotly <- renderPlotly({
        
        x <- replicate(3, runif(n = input$num ,min = -1, max = 1))
        
        inside <- (apply(x, 1, function(x) {x[1]^2 +x[2]^2} +
                             x[3]^2))<=1
        
        y <- data.frame(x, inside)
        
        plot_ly(y, x = X1, y = X2, z = X3, type = "scatter3d", mode = "markers",
                color = inside) %>% layout(showlegend = FALSE)
        
    })
}

shinyApp(ui = ui, server = server)

