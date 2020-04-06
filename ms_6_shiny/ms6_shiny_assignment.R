library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)


pres12_vars <- read_rds("pres12_vars.rds")

ui <- fluidPage(navbarPage("Switchers",
                           theme = shinytheme("journal"),
                           tabPanel("About"),
                           tabPanel("Graphics",
                                    tabsetPanel(
                                      tabPanel("Who Switches?"),
                                      tabPanel("Demographics of the Switchers",
                                               h3("President 2012 Switchers"),
                                               sidebarPanel(
                                                 helpText("Select a demographic group to learn about switchers"),
                                                 selectInput("pres12_vars", "Demographic Groups:",
                                                             choices = list("Race",
                                                                            "Gender",
                                                                            "Age",
                                                                            "Education",
                                                                            "Party",
                                                                            "Income"),
                                                             selected = "Race")),
                                               mainPanel(plotOutput("pres12_vars")))))))

server <- function(input, output, session){
  output$pres12_vars <- renderPlot({
    ggplot(pres12_vars, aes(race_12, race_12/sum(race_12), fill =
                              reorder(presvote12, race_12/sum(race_12)))) +
      geom_col() + scale_y_continuous(labels = scales::percent) + xlab("Race of Switchers") +
      ylab("Percentage of Switchers") + ggtitle("President 2012 Switchers by Race") +
      scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                         label = c("White", "Black", "Hispanic", "Asian",
                                   "Native American", "Mixed", "Other")) +
      theme(panel.background = element_rect("white"), axis.line.x.bottom =
              element_line("black"), axis.line.y.left = element_line("black")) +
      scale_fill_manual("Voter for", labels = c("Romney", "Obama"), values=c("red", "blue"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)