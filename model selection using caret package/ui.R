library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 4"),
  
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             plotOutput("BoxPlots"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max=1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("GLMnet Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("GlmModelSummary1"),
             hr(),
             plotOutput("GlmModelPlots"),
             verbatimTextOutput("GlmModelSummary2")
    ),
    tabPanel("PLS Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("ANN Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("AnnModelSummary1"),
             hr(),
             plotOutput("AnnModelPlots"),
             verbatimTextOutput("AnnModelSummary2")
    ),   
    tabPanel("Random Forest Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("rfSummary1"),
             hr(),
             plotOutput("rfPlots"),
             verbatimTextOutput("rfSummary2")
    ),
    tabPanel("Support Vector Machine",
             tags$h3("Best tuning parameters:"),
             tableOutput("svmSummary1"),
             hr(),
             plotOutput("svmPlots"),
             verbatimTextOutput("svmSummary2")
    ),
    tabPanel("Boosting Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("boostSummary1"),
             hr(),
             plotOutput("boostPlots"),
             verbatimTextOutput("boostSummary2")
    ),
    tabPanel("Cubist Model",
             tags$h3("Best tuning parameters:"),
             tableOutput("cubistSummary1"),
             hr(),
             plotOutput("cubistPlots"),
             verbatimTextOutput("cubistSummary2")
    ),
    tabPanel("Model Selection",
             tags$h3("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c("GLMnet", "PLS", "ANN", "RF","SVM","Boosting","Cubist"), selected = "Cubist")
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot"),
             verbatimTextOutput("predictions")
    )
  )
))
