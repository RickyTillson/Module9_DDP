library(shiny)
library(markdown)

# page setup

shinyUI(navbarPage( "Z-Tests for Proportions",
    tabPanel("Readme",
        fluidRow(includeMarkdown("about.md")
        )
    ),
                    
    tabPanel("2-Sample",
        sidebarLayout(
            sidebarPanel(
                
                tags$html(
                    tags$style(style="color:red", "red"
                    )
                ),
                
                # input - proportion
                sliderInput("proportion1",
                            tags$div(HTML(paste(tags$span(style = 'color:blue', "score or proportion for the first sample")), sep="")),
                            min = 1, max = 99, value = 40, step = 1
                ),
                
                # input - base
                numericInput("base1",
                             tags$div(HTML(paste(tags$span(style = 'color:blue', "base size for the first sample")), sep="")),
                             value = 200
                ),
                
                # input - proportion
                sliderInput("proportion2",
                            tags$div(HTML(paste(tags$span(style = 'color:red', "score or proportion for the second sample")), sep="")),
                    min = 1, max = 99, value = 60, step = 1
                ),
                
                # input - base
                numericInput("base2",
                             tags$div(HTML(paste(tags$span(style = 'color:red', "base size for the second sample")), sep="")),
                    value = 300
                ),
                
                # input - number of tails
                radioButtons("tails2",
                             "1 or 2 tailed test",
                             choices = list("1 tailed" = 1, "2 tailed" = 2),
                             selected = 2
                ),
                
                # input - direction of test if 1 tailed
                conditionalPanel(condition = "input.tails2 == 1",
                                 radioButtons("direction2",
                                              "are you testing for significance for sample 2 above or below sample 1?",
                                              choices = list("above" = 1, "below" = 2),
                                              selected = 1
                                 )
                ),
                
                # input - CI
                sliderInput("confidence2",
                            "what significance level is required?",
                            min = 50, max = 99.5, value = 95, step = 0.5
                )
            ),
            
            # main panel being built with input from server.R
            mainPanel(
                htmlOutput("TestResult2"),
                plotOutput("sigPlot2")
            )
        )
    ),
    
    tabPanel("1-Sample",
        sidebarLayout(
            sidebarPanel(
                             
                # input - proportion
                sliderInput("proportion",
                            tags$div(HTML(paste(tags$span(style = 'color:darkgreen', "score or proportion for the sample")), sep="")),
                    min = 1, max = 99, value = 40, step = 1
                ),
                                 
                # input - base
                numericInput("base",
                             tags$div(HTML(paste(tags$span(style = 'color:darkgreen', "base size for the sample")), sep="")),
                    value = 200
                ),
                                 
                # input - target
                    sliderInput("target",
                    "score or proportion for target",
                    min = 1, max = 99, value = 60, step = 1
                ),
                                 
                # input - number of tails
                radioButtons("tails1",
                    "1 or 2 tailed test",
                    choices = list("1 tailed" = 1, "2 tailed" = 2),
                    selected = 2
                ),
                                 
                # input - direction of test if 1 tailed
                conditionalPanel(condition = "input.tails1 == 1",
                    radioButtons("direction1",
                    "are you testing for significance above or below the target?",
                    choices = list("above" = 1, "below" = 2),
                    selected = 1
                    )
                ),
                                 
                # input - CI
                sliderInput("confidence1",
                    "what significance level is required?",
                    min = 50, max = 99.5, value = 95, step = 0.5
                )
            ),
                             
                # main panel being built with input from server.R
            mainPanel(
                htmlOutput("TestResult1"),
                plotOutput("sigPlot1")
            )
        )
    )
))