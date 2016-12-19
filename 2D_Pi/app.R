library(shiny)
library(shinythemes)
library(ggplot2)

generate_points <- function(num){
    #generate points in a square:
    x <- replicate(2, runif(num ,min = -1, max = 1))
    inside <- (apply(x, 1, function(x) {x[1]^2 +x[2]^2}))<=1
    return(data.frame(x, inside))
}

descp = readChar("description.txt", nchars = 1e6)

ui <- fluidPage(theme = shinytheme("flatly"),
                
                fluidRow(
                    column(
                        verticalLayout(
                            
                            wellPanel(descp),
                            
                            wellPanel(sliderInput(inputId = "num",
                                                  label = "Number of Points",
                                                  value = 1000, min = 10, max = 30000)),
                            
                            wellPanel("Your approximation of pi is:",
                                      verbatimTextOutput("pi"))) , width = 3),
                    
                    
                    column(
                        plotOutput("plot", width = "800px", height = "800px"), width = 8)
                )
)


server <- function(input, output) {
    
    rv <- reactive({generate_points(input$num)})
    
    output$pi <- renderPrint({
        4 *sum(rv()$inside)/input$num
    })
    
    output$plot <- renderPlot({
        plot(x = rv()$X1, y = rv()$X2, xlab = "x", ylab = "y",
             col = as.factor(rv()$inside), pch=20)
    })
}

shinyApp(ui = ui, server = server)