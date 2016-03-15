library(shiny)
library(shinythemes)
library(ggplot2)
library(babynames)
library(data.table)
library(dygraphs)
library(tidyr)
library(dplyr)
library(DT)

names <- data.table(babynames)



ui <- fluidPage( theme = shinytheme("flatly"),
    tabsetPanel(
        tabPanel("Boy Names",
                 
                 sidebarLayout(
                     sidebarPanel( helpText("Pick up to five names below and explore their popularity as baby names over time:"),
                                   textInput(inputId = "name1", " ", value = "Alexander"),
                                   textInput(inputId = "name2", " ", value = "Jonathan"),
                                   textInput(inputId = "name3", " ", value = "Cody"),
                                   textInput(inputId = "name4", " "),
                                   textInput(inputId = "name5", " "),
                                   
                                   numericInput(inputId = "yearm",label = "Pick a year below to see 
                                        the most popular  names for that year:", value = 2014 ),
                                   
                                   dataTableOutput(outputId = "list1"),
                                   
                                   width = 3),
                     
                     mainPanel(wellPanel(dygraphOutput("plot1"),
                                         width = 9)
                    
                     )
                 )
                 ),
        
        
        tabPanel("Girl Names",
                 
                 sidebarLayout(
                     sidebarPanel( helpText("Pick up to five names below and explore their popularity as baby names over time:"),
                                   textInput(inputId = "name6", " ", value = "Judy"),
                                   textInput(inputId = "name7", " ", value = "Emma"),
                                   textInput(inputId = "name8", " ", value = "Samantha"),
                                   textInput(inputId = "name9", " "),
                                   textInput(inputId = "name10", " "),
                                   
                                   numericInput(inputId = "yearf",label = "Pick a year below to see 
                                                the most popular  names for that year:", value = 2014 ),
                                   
                                   dataTableOutput(outputId = "list2"),
                                   
                                   width = 3),
                     
                     mainPanel(wellPanel(dygraphOutput("plot2"),
                                         width = 9)
                               
                     )
                     )
        ),
        
        
        tabPanel("Gender Neutral Names", 
                 
                 sidebarLayout(
                     
                     sidebarPanel(
                     
                     textInput(inputId = "neutral_name", "Pick a name to see its popularity by sex:",
                               value = ""),
                  
                     tags$br(),
                     
                     
                     numericInput(inputId = "year2",label = "Don't know where to start? 
                                  Pick a year below to see the most popular gender-neutral names
                                  for that year:", value = 2014 ),
                     
                     dataTableOutput(outputId = "list3"),
                     
                     width = 3
                     
                     ),
                     
                     mainPanel( 
                         wellPanel(plotOutput("plot3")) 
                         #wellPanel(plotOutput("plot2"))
                         )
                     
                 )
                )
    )
)




server <- function(input, output) {

    output$plot1 <- renderDygraph({
        
        names[name %in% c(input$name1, input$name2, input$name3,input$name4, 
                                          input$name5)][sex == "M"][,prop := NULL][,sex := NULL][year > 1910] %>% 
            spread(key = name, value = n) %>% 
            dygraph() %>%  
            dyAxis("y",label = "Baby Count", labelWidth = 14 ) %>% 
            dyAxis("x", drawGrid = FALSE , label = "Year of Birth", labelHeight = 14) %>% 
            dyOptions(axisLineWidth = .5, strokeWidth = 1.5, axisTickSize = 10, 
                      axisLabelFontSize = 12, gridLineColor = "lightgrey",
                      axisLabelColor = "grey", axisLineColor = "grey", 
                      colors = RColorBrewer::brewer.pal(5, "Dark2"))
        
    })
    
    
    output$plot2 <- renderDygraph({
        
        names[name %in% c(input$name6, input$name7, input$name8,input$name9, 
                          input$name10)][sex == "F"][,prop := NULL][,sex := NULL][year > 1910] %>% 
            spread(key = name, value = n) %>% 
            dygraph() %>%  
            dyAxis("y",label = "Baby Count", labelWidth = 14 ) %>% 
            dyAxis("x", drawGrid = FALSE , label = "Year of Birth", labelHeight = 14) %>% 
            dyOptions(axisLineWidth = .5, strokeWidth = 1.5, axisTickSize = 10, 
                      axisLabelFontSize = 12, gridLineColor = "lightgrey",
                      axisLabelColor = "grey", axisLineColor = "grey", 
                      colors = RColorBrewer::brewer.pal(5, "Dark2"))
        
    })
    
    
    
    #output$plot2 <- renderPlot({
    #    
    #    names[name == input$neutral_name][year > 1960] %>%
    #        ggplot(aes(x = year, y = n, fill = sex)) +
    #        #geom_area(position = "fill") +
    #        geom_bar(stat = "identity", position = "fill", width = 1) +
    #        ylab("Fraction Female/Male") +
    #        geom_hline(yintercept = .5) + theme_minimal() +
    #        scale_fill_brewer(palette = "Set1") +
    #        ggtitle("Proportion of Babies with the given name by Sex")
    #
    #})
    
    
    output$plot3 <- renderPlot({
        names[name == input$neutral_name][year > 1960] %>%
            ggplot(aes(x = year, y = n, color = sex)) +
            #geom_area(position = "fill") +
            #geom_bar(stat = "identity", position = "dodge", width = 1) +
            geom_line() +
            ylab("Baby Count") +
            geom_hline(yintercept = .5) + theme_minimal() +
            scale_color_brewer(palette = "Set1")
    })
    
    output$list1 <- renderDataTable({
        
        names[year == input$yearm][sex == "M"][,.(name = name, count = n)] %>% 
            head(100) %>% 
            datatable(options = list(dom = "tp"))
            
    })
    
    output$list2 <- renderDataTable({
        
        names[year == input$yearf][sex == "F"][,.(name = name, count = n)] %>% 
            head(100) %>% 
            datatable(options = list(dom = "tp"))
        
    })
    
    output$list3 <- renderDataTable({
        
        names[year == input$year2] %>% 
            .[,prop := NULL] %>%
            spread(sex, n, fill = 0) %>% 
            mutate(count = F + M, frac = F/(M + F)) %>% 
            .[frac >0.4] %>% 
            .[frac <0.6] %>% 
            arrange(desc(count)) %>% 
            transmute(name, count) %>% datatable(options = list(dom = "tp"))
        
    })
    
    
}

shinyApp(ui = ui, server = server)
