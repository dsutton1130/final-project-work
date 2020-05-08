library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(gt)
library(broom)

CCES_Panel <- read_rds("CCES.rds")
switchers <- read_rds("switchers.rds")
diff.lifeevents <- read_rds("diff.lifeevents.rds")
mod.bestvars <- read_rds("mod.bestvars.rds")
mod.bestvars.check <- read_rds("mod.bestvars.check.rds")

ui <- fluidPage(navbarPage("Switchers",
                           theme = shinytheme("simplex"),
                           tabPanel("About",
                                    imageOutput("switchers1", width = "100%", height = "100%"),
                                    br(),
                                    br(),
                                    imageOutput("switchers2", width = "100%", height = "100%"),
                                    br(),
                                    h3("Analyzing the Relationships Between Vote-Switching & Demographics, Policy Positions, and Public Opinion", align = "center"),
                                    br(),
                                    div(),
                                    
                                    
                                    
                                    br(),
                                    fluidRow(column(2),
                                             
                                             h4(strong("About this Project"), align = "center"),          
                                             
                                             
                                             p("This project investigates switchers - people who vote for one major
                                                               party's candidate in an election and then vote for the other
                                                               major party's candidate in the following election. I use
                                                               survey responses on a wide variety of subjects, such as demographics, policy
                                                               positions and life events to compare switchers to the general electorate and
                                                               present a profile of the switchers themselves. The data presented here comes from
                                                               the 2010-2014 Cooperative Congressional Election Study Panel Survey."),
                                             
                                             br(),
                                             
                                             p("The particular group of switchers examined here are the 2012 presidential election switchers in the
                                               United States. These switchers can be divided neatly into two groups: Those who voted for the Republican
                                               candidate (John McCain) in 2008 and for the Democratic candidate (Barack Obama) in 2012 and those who voted for
                                               the Democratic candidate (Obama) in 2008 and for the Republican candidate (Mitt Romney) in 2012."),
                                             
                                             h4(strong("About the Author"), align = "center")),
                                    p("David J. Sutton is a first-year Ph.D. student at Harvard University studying American Politics. His research interests
                             include constitutional history and interpretation, early American political thought, elections and voting behavior. You
                             can contact him at dsutton@g.harvard.edu."),
                                    p("The code for this project can be found ", tags$a("here.", href="https://github.com/dsutton1130/final-project-work"))),
                           
                           tabPanel("Graphics",
                                    tabsetPanel(
                                        tabPanel("Who Switches?",
                                                 h3("How Switchers Compare to the Electorate"),
                                                 sidebarPanel(
                                                     helpText("Select a demographic group to compare switchers to the electorate"),
                                                     span(),
                                                     helpText(em("In this case switchers are Obama-Romney or McCain-Obama voters in 2008 and 2012, respectively")),
                                                     selectInput("plot1", "Demographic Groups:",
                                                                 choices = list("Race" = "race_12",
                                                                                "Gender" = "gender_12",
                                                                                "Age" = "agegroup12",
                                                                                "Education" = "educ_12",
                                                                                "Party" = "partyreg12",
                                                                                "Income" = "income12"),
                                                                 selected = "Race")),
                                                 mainPanel(plotOutput("CCES_Panel"))),
                                        tabPanel("Demographics",
                                                 h3("President 2012 Switchers"),
                                                 sidebarPanel(
                                                     helpText("Select a demographic group to learn about switchers"),
                                                     span(),
                                                     helpText(em("In this case switchers are Obama-Romney or McCain-Obama voters in 2008 and 2012, respectively")),
                                                     selectInput("plot2", "Demographic Groups:",
                                                                 choices = list("Race" = "race_12",
                                                                                "Gender" = "gender_12",
                                                                                "Age" = "pres.agegroup12",
                                                                                "Education" = "educ_12",
                                                                                "Party" = "partyreg12",
                                                                                "Income" = "income.12"),
                                                                 selected = "Race")),
                                                 mainPanel(plotOutput("switchers"))),
                                        tabPanel("Life Events",
                                                 h3("Life Events in the Previous Two Years"),
                                                 mainPanel(plotOutput("diff.lifeevents")),
                                                 p("Voters were asked if they had experienced any of the life events
                                               (listed on the graph's y-axis) over the past two years. This graph indicates the
                                               difference in percentage points between those who answered 'yes' in the general
                                               electorate and those who answered 'yes' in the group of 2012 presidential election
                                               switchers."),
                                                 br(),
                                                 p("There does not seem to be much difference between the electorate and switchers
                                             regarding the percentage of each group which experienced these life events within approximately
                                             two years before the 2012 presidential election.")),
                                        tabPanel("Model",
                                                 h3("Logistic Regression"),
                                                 mainPanel(tableOutput("mod.bestvars")), br(),
                                                 p("After running a logistic regression on over fifty variables, I identified
                                                   the eight variables with the strongest relationship to switching in the 2012 presidential
                                                   election. The table to the left displays the results of regressing switching
                                                   on these eight variables."), 
                                                 p("pvote08 asked in 2010 which candidate the respondent voted for in 2008, with a response of 1 being Barack Obama and 2
                                                   being John McCain."),
                                                 p("The variable ideo.diff.repub is the absolute value of the difference between a
                                                   respondent's ideological self-placement and their placement of the Republican party on a 7-point scale,
                                                   with 1 as 'Very Liberal' and 7 as 'Very Conservative.'"),
                                                 p("CC12_321 asked respondents about their views on climate change, from least skeptical (1) to most skeptical (5)."),
                                                 p("CC12_302 asked respondents: 'Would you say that OVER THE PAST YEAR the nation’s economy has...?' and the respondents choices were 1) Gotten much better,
                                                   2) Gotten better, 3) Stayed about the same, 4) Gotten worse, 5) Gotten much worse."),
                                                 p("CC12_322_3 asked respondents if the United States government should allow police to question anyone they think may be in the country illegally."),
                                                 p("The variable religpew_12_3 indicates if the respondent is Mormon, with 1 as yes and 0 as no."),
                                                 p("The variable partyreg12_1 indicates if the respondent identified as an Independent, not affiliated with any party, or would not say with which political party they
                                                   affiliate, with 1 indicating they belong to this group and 0 indicating they do not.", align = "right"),
                                                   p("Religpew_12_2 indicates if the respondent is Roman Catholic, with a 1 as yes and a 0 as no.", align = "right"), br(),
                                                 p("The codebook for this iteration of the CCES can be found ", tags$a("here.", href="https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/TOE8I1")), br(),
                                                 p("And code for this project can be found ", tags$a("here.", href="https://github.com/dsutton1130/final-project-work")),
                                                 span(),
                                                 column(12, align = "left",
                                                        h3("Modeling"), br(),
                                                        mainPanel(plotOutput("mod.bestvars.check")), br(),
                                                        p("This plot reveals the difficulty in attempting to model for switching in the 2012
                                                   presidential election. An evaluation of the logistic regression model used above reveals
                                                   that although the model correctly predicted the lack of switching in over 8000 respondents (identified
                                                   in the plot as 'True Negatives'), the model failed to identify 299 switchers (identified in the plot as
                                                   'False Negatives'). The conclusion at which we arrive is that, at least as indicated by the variables
                                                   measured in the 2010-2014 CCES Panel Survey, 2012 presidential election switchers are not significantly 
                                                          different than voters in the general electorate of the same election.")
                                                 ))))))

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
    
    output$CCES_Panel <- renderPlot({
        if(input$plot1 == "race_12"){
            CCES_Panel %>%
                group_by(p12switch) %>%
                count(race_12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(race_12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Race", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), label = c("White", "Black", "Hispanic",
                                                                                 "Asian", "Native American",
                                                                                 "Mixed", "Other", "Middle Eastern")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Electorate", "Switchers"), values=c("dark grey", "purple"))
            
        } else if(input$plot1 == "gender_12"){
            CCES_Panel %>%
                group_by(p12switch) %>%
                count(gender_12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(gender_12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Gender", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2), label = c("Male", "Female")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Electorate", "Switchers"), values=c("dark grey", "purple"))
            
        } else if(input$plot1 == "agegroup12"){
            CCES_Panel %>%
                group_by(p12switch) %>%
                count(agegroup12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(agegroup12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Age", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), label = c("18-24", "25-34", "35-44",
                                                                               "45-54", "55-64", "65-74", "75-84",
                                                                               "85-94")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Switchers", "Electorate"), values=c("purple", "dark grey"))
            
        } else if(input$plot1 == "educ_12"){
            CCES_Panel %>%
                group_by(p12switch) %>%
                count(educ_12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(educ_12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Education", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), label = c("No High School",
                                                                           "High school graduate", "Some college",
                                                                           "2-year", "4-year", "Post-grad")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Electorate", "Switchers"), values=c("dark grey", "purple")) +
                coord_flip()
            
        } else if(input$plot1 == "partyreg12"){
            CCES_Panel %>%
                filter(partyreg12 < 5) %>%
                group_by(p12switch) %>%
                count(partyreg12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(partyreg12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Party", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4), label = c("None/Independent/Didn't Say",
                                                                     "Democrat", "Republican",
                                                                     "Other")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Electorate", "Switchers"), values=c("dark grey", "purple"))
            
        } else if(input$plot1 == "income12"){
            CCES_Panel %>%
                group_by(p12switch) %>%
                count(income12) %>%
                mutate(pct = n/sum(n)) %>%
                ggplot(aes(income12, pct, fill = factor(p12switch))) +
                geom_col(position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Income", y = "Percentage of Group", title = "How Switchers Compare to Electorate",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_discrete(breaks = c(1, 2, 3, 4, 5, 6), label = c("0-29,999", "30,000-59,999",
                                                                         "60,000-99,999", "100,000-149,999",
                                                                         "150,000+", "Didn't say/Skipped/Not asked")) +
                theme_classic() +
                scale_fill_manual(na.translate = F, "Group", labels = c("Electorate", "Switchers"), values=c("dark grey", "purple")) +
                coord_flip()
        }
        
        
    })
    
    output$switchers <- renderPlot({
        if(input$plot2 == "race_12"){
            switchers %>%
                filter(p12switch == 1) %>%
                ggplot(aes(race_12, ..prop.., fill = reorder(presvote12, race_12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = "Race of Switchers",
                     y = "Percentage of Switchers",
                     title = "President 2012 Switchers by Race",
                     caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7),
                                   label = c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other")) +
                theme_classic() + scale_fill_manual("Voted for", labels = c("Obama-Romney", "McCain-Obama"),
                                                    values=c("red", "blue"))
            
        } else if(input$plot2 == "gender_12"){
            switchers %>%
                filter(p12switch == 1) %>%
                ggplot(aes(gender_12, ..prop.., fill = reorder(presvote12, gender_12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) + scale_y_continuous(labels = scales::percent) +
                xlab("Gender of Switchers") + ylab("Percentage of Switchers") +
                labs(title = "President 2012 Switchers by Gender", caption =
                         "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2), label = c("Male", "Female")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue"))
            
        } else if(input$plot2 == "pres.agegroup12"){
            switchers %>%
                filter(p12switch == 1) %>%
                ggplot(aes(pres.agegroup12, ..prop.., fill = reorder(presvote12, pres.agegroup12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) + xlab("Age of Switchers") +
                ylab("Percentage of Switchers") +
                labs(title = "President 2012 Switchers by Age", caption =
                         "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8), label = c("18-24", "25-34", "35-44",
                                                                                 "45-54", "55-64", "65-74", "75-84",
                                                                                 "85-94")) +
                scale_y_continuous(labels = scales::percent) + theme(panel.background = element_rect("white"),
                                                                     axis.line.x.bottom = element_line("black"),
                                                                     axis.line.y.left = element_line("black")) +
                scale_fill_manual("Voted for", labels = c("Obama-Romney", "McCain-Obama"), values=c("red", "blue"))
            
        } else if(input$plot2 == "educ_12"){
            switchers %>%
                filter(p12switch == 1) %>%
                ggplot(aes(educ_12, ..prop.., fill = reorder(presvote12, educ_12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) + scale_y_continuous(labels = scales::percent) +
                xlab("Education Level") + ylab("Percentage of Switchers") +
                labs(title = "President 2012 Switchers by Education", caption =
                         "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), label = c("No High School",
                                                                           "High school graduate",
                                                                           "Some college", "2-year",
                                                                           "4-year", "Post-grad")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue")) +
                coord_flip()
            
        } else if(input$plot2 == "partyreg12"){
            switchers %>%
                filter(p12switch == 1, partyreg12 < 5) %>%
                ggplot(aes(partyreg12, ..prop.., fill = reorder(presvote12, partyreg12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) + scale_y_continuous(labels = scales::percent) +
                xlab("Party of Switchers") + ylab("Percentage of Switchers") +
                labs(title = "President 2012 Switchers by Party", caption =
                         "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4), label = c("None/Ind/Didn't Say", "Democratic",
                                                                     "Republican","Other")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("Obama-Romney", "McCain-Obama"),
                                                                                    values=c("red", "blue"))
            
        } else if(input$plot2 == "income.12"){
            switchers %>%
                filter(p12switch == 1) %>%
                ggplot(aes(income.12, ..prop.., fill = reorder(presvote12, income.12)), stat = "count") +
                geom_bar(aes(y = ..count../sum(..count..)), position = position_dodge()) + scale_y_continuous(labels = scales::percent) +
                xlab("Income of Switchers") + ylab("Percentage of Switchers") +
                labs(title = "President 2012 Switchers by Income", caption =
                         "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
                scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6), label = c("0-29,999", "30,000-59,999",
                                                                           "60,000-99,999","100,000-149,999",
                                                                           "150,000+", "Didn't say/Skipped/Not asked")) +
                theme(panel.background = element_rect("white"), axis.line.x.bottom = element_line("black"),
                      axis.line.y.left = element_line("black")) + scale_fill_manual("Voted for",
                                                                                    labels = c("McCain-Obama", "Obama-Romney"),
                                                                                    values=c("blue", "red")) +
                coord_flip()
        }
        
        
    })
    
    output$diff.lifeevents <- renderPlot({
        diff.lifeevents %>%
            ggplot(aes(diff, name)) +
            geom_point() +
            labs(title = "How Switchers and the Electorate Differ",
                 subtitle = "Experiences within the last two years",
                 x = "Percentage Point Difference between Electorate and Switchers", y = "Life Event", caption =
                     "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
            theme_minimal() +
            scale_x_continuous(limits = c(-10, 10)) +
            scale_y_discrete(label = c("Better job", "Been victim of a crime", "Employee benefits cut",
                                       "Visited doctor's office", "Visited emergency room", "Loss of job",
                                       "Child moved out", "New child in family", "Divorce", "Marriage",
                                       "Promotion at work", "Raise at work", "Traffic ticket"))
    })
    
    output$mod.bestvars <- render_gt({
        mod.bestvars %>%
            tidy(conf.int = TRUE) %>%
            select(term, estimate, conf.low, conf.high, p.value) %>%
            gt() %>%
            fmt_number(columns = 2:4, decimals = 2) %>%
            fmt_number(columns = 5, decimals = 8) %>%
            cols_label(term = "Variable", estimate = "Estimate", conf.low = "Lower Bound",
                       conf.high = "Upper Bound", p.value = "P Value") %>%
            tab_header("Effect of 'Best' Variables on Switching",
                       subtitle = "Data from 2010-2014 CCES Panel Survey")
    })
    
    output$mod.bestvars.check <- renderPlot({
        mod.bestvars.check %>%
            ggplot(aes(status, .fitted), group = status) +
            labs(title = "Evaluating the Model's Predictions", x = "Prediction Status",
                 y = "Probability of Switching",
                 caption = "Source: 2010-2014 Cooperative Congressional Election Study Panel Survey") +
            scale_x_discrete(label = c("False Negative", "True Negative", "True Positive")) +
            geom_point() +
            geom_jitter() +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)