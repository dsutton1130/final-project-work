library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)



switchers <- load("switchers.rds")

ui <- fluidPage(navbarPage("Switchers",
                           theme = shinytheme("journal"),
                           tabPanel("About",
                                    imageOutput("switchers1", width = "100%", height = "100%"),
                                    br(),
                                    br(),
                                    imageOutput("switchers2", width = "100%", height = "100%"),
                                    br(),
                                    h3("Analyzing the Relationships Between Vote-Switching & Demographics, Policy, and Public Opinion", align = "center")),
                           tabPanel("Graphics",
                                    tabsetPanel(
                                        tabPanel("Who Switches?"),
                                        tabPanel("Demographics of the Switchers",
                                                 h3("President 2012 Switchers"),
                                                 sidebarPanel(
                                                     helpText("Select a demographic group to learn about switchers"),
                                                     span(),
                                                     helpText(em("In this case switchers are Obama-to-Romney or McCain-to-Obama voters in 2008 and 2012, respectively")),
                                                     selectInput("plot1", "Demographic Groups:",
                                                                 choices = list("Race" = "race_12",
                                                                                "Gender" = "gender_12",
                                                                                "Age" = "pres.agegroup12",
                                                                                "Education" = "educ_12",
                                                                                "Party" = "partyreg12",
                                                                                "Income" = "income"),
                                                                 selected = "Race")),
                                                 mainPanel(plotOutput("switchers")))))))

server <- function(input, output, session){
    
    output$switchers1 <- renderImage({
        
        list(src = './switchers_image1.webp',
             height = 300,
             width = 600,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    output$switchers2 <- renderImage({
        
        list(src = './switchers_gradient.png',
             height = 60,
             width = 350,
             alt = "Analyzing the Relationships Between Vote-Switching & Demographics, Policy, and Public Opinion",
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    output$switchers <- renderPlot({
        if(input$plot1 == "race_12"){
            s1
            
        } else if(input$plot1 == "gender_12"){
            s2
            
        } else if(input$plot1 == "pres.agegroup12"){
            s3
            
        } else if(input$plot1 == "educ_12"){
            s4
            
        } else if(input$plot1 == "partyreg12"){
            s5
            
        } else if(input$plot1 == "income"){
            s6
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)