library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)



pres12_vars <- read_rds("pres12_vars.rds")

ui <- fluidPage(navbarPage("Switchers",
                           theme = shinytheme("journal"),
                           tabPanel("About",
                                    imageOutput("switchers1"),
                           br(),
                           h2(div("SWITCHERS:", style = "color: blue", align = "center")),
                           h4("Analyzing the Relationships Between Vote-Switching & Demographics, Policy, and Public Opinion", align = "center")),
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
                                                                                "Age" = "agegroup",
                                                                                "Education" = "educ_12",
                                                                                "Party" = "partyreg12",
                                                                                "Income" = "income"),
                                                                 selected = "Race")),
                                                 mainPanel(plotOutput("pres12_vars")))))))

server <- function(input, output, session){
    
    output$switchers1 <- renderImage({
        
        list(src = './switchers_image1.webp',
             height = 300,
             width = 700,
             style = "display: block; margin-left: auto; margin-right: auto;")},
        deleteFile = FALSE
    )
    
    output$pres12_vars <- renderPlot({
        if(input$plot1 == "race_12"){  
            ggplot(pres12_vars, aes(race_12, race_12/sum(race_12), fill =
                                        reorder(presvote12, race_12/sum(race_12)))) +
                geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Race of Switchers") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Race") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                                   label = c("White", "Black", "Hispanic", "Asian",
                                             "Native American", "Mixed", "Other")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom =
                          element_line("black"), axis.line.y.left = element_line("black")) +
                scale_fill_manual("Voted for", labels = c("Obama-Romney", "McCain-Obama"), values=c("red", "blue"))
        } else if(input$plot1 == "gender_12"){
            ggplot(pres12_vars, aes(gender_12, gender_12/sum(gender_12), fill =
                                        reorder(presvote12, gender_12/sum(gender_12)))) +
                geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Gender of Switchers") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Gender") +
                scale_x_continuous(breaks = c(1, 2), label = c("Male", "Female")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue"))
        } else if(input$plot1 == "agegroup"){
            ggplot(pres12_vars, aes(agegroup, agegroup/sum(agegroup), fill =
                                        reorder(presvote12, agegroup/sum(agegroup)))) + geom_col() +
                scale_y_continuous(labels = scales::percent) + xlab("Age of Switchers") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Age") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), label =
                                       c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85-94")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for", labels =
                                                                                        c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue"))
        } else if(input$plot1 == "educ_12"){
            pres12_vars %>%
                filter(educ_12 < 7) %>%
                ggplot(aes(educ_12, educ_12/sum(educ_12), fill = reorder(presvote12, educ_12/sum(educ_12)))) +
                geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Education Level") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Education") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), label = c("No High School",
                                                                           "High school graduate", "Some college",
                                                                           "2-year", "4-year", "Post-grad")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue")) +
                coord_flip()
        } else if(input$plot1 == "partyreg12"){
            pres12_vars %>%
                filter(partyreg12 < 5) %>%
                ggplot(aes(partyreg12, partyreg12/sum(partyreg12), fill = reorder(presvote12,
                                                                                  partyreg12/sum(partyreg12)))) +
                geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Party of Switchers") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Party") +
                scale_x_continuous(breaks = c(1, 2, 3, 4), label = c("None/Ind/Didn't Say", "Democratic",
                                                                     "Republican", "Other")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue"))
        } else if(input$plot1 == "income"){
            ggplot(pres12_vars, aes(income, income/sum(income), fill = reorder(presvote12, income/sum(income)))) +
                geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Income of Switchers in US Dollars") +
                ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Income") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), label = c("0-29,999", "30,000-59,999",
                                                                           "60,000-99,999", "100,000-149,999",
                                                                           "150,000+",
                                                                           "Didn't say/Skipped/Not asked")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue")) +
                coord_flip()
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)