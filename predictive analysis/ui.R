library(shiny)
library(caret)
library(SDMTools)
library(shinyjs)
library(visdat)

Data <- read.csv("Ass3Data.csv")
mychoices = c("Y","Layer1", "Layer2","Layer3","Layer4","Layer5","Layer6","Layer7","Layer8","Layer9","Layer10","Layer11","Layer12","Layer13","Layer14","Layer15","Layer16","Layer17","Layer18","Layer19","Layer20","Layer21","Layer22","Layer23","Layer24","Layer25","Layer26","Layer27","Layer28","Layer29","Layer30")
num_choices = c("Y","Layer1", "Layer2","Layer3","Layer4","Layer5","Layer6","Layer7","Layer8","Layer9","Layer10","Layer11","Layer12","Layer13","Layer14","Layer15","Layer16","Layer17","Layer18","Layer19","Layer20","Layer21","Layer22","Layer23","Layer24","Layer25","Layer26","Layer27","Layer28","Layer29","Layer30")
categorical_variables <- c("Author","Priority", "Price","Speed", "Duration","Scarcity","Location","Agreed",
                           "State","Class","Surface")
mychoices_num = c("Layer1", "Layer2","Layer3","Layer4","Layer5","Layer6","Layer7","Layer8","Layer9","Layer10","Layer11","Layer12","Layer13","Layer14","Layer15","Layer16","Layer17","Layer18","Layer19","Layer20","Layer21","Layer22","Layer23","Layer24","Layer25","Layer26","Layer27","Layer28","Layer29","Layer30")


#creating the UI for the app
shinyUI(fluidPage(
  useShinyjs(),
  
  titlePanel(
    h3("Assignment 3 -- Ravi Kumar Singh",align="middle")
  ),
  mainPanel(
    tabsetPanel(
      navbarMenu(title = "Data",
                 
                 tabPanel("Table",#column(width = 10),selectInput(inputId = "var", label = "Variables to show", choices=mychoices, selected = mychoices, multiple = TRUE, width = "800px")),
                          DT::dataTableOutput("table")
                 ),
                 
                 tabPanel("Summary",div(style="width:150%;",fluidRow(verbatimTextOutput("glimpse"))),
                          div(style="width:150%;",fluidRow(verbatimTextOutput("summary")))),
                 tabPanel("matplot",
                          sidebarLayout(sidebarPanel(checkboxInput("remove", "Removing structural break", value = FALSE)),
                                        mainPanel(plotOutput("matplot"),verbatimTextOutput("mat_explain"))
                          ))
      ),
      
      navbarMenu(title='Missing Values',
                 #sliderInput("obs", "Number of observations:",min = 0, max = 100, value = 75),
                 
                 tabPanel("missplot",
                          sidebarLayout(sidebarPanel(
                            checkboxInput("omit", "Removes missing values", value = FALSE)),
                            mainPanel(plotOutput("missPlot",height = "400",width = "100%" ),verbatimTextOutput("mis_explain"))
                          )),
                 tabPanel("corrplot", 
                          sidebarLayout(sidebarPanel(
                            checkboxInput("omit", "Removes missing values", value = FALSE)),
                            mainPanel(plotOutput("corrPlot",height="500", width = "150%"))
                          )),
                 tabPanel("interaction term",
                          sidebarLayout(sidebarPanel(
                            selectInput(inputId = "categorical",label= "Select Variables for X", choices = categorical_variables, selected= "Price", multiple = FALSE),
                            selectInput(inputId = "categorical1",label= "Select Variables for Y", choices = categorical_variables, selected= "Duration", multiple = FALSE)),
                            mainPanel(plotOutput("interaction",height="500", width = "150%"),verbatimTextOutput("interaction_explain"))
                          )
                 )),
      navbarMenu("Outliers",
                 
                 tabPanel("Boxplot",
                          sidebarLayout(sidebarPanel(
                            selectInput(inputId = "vc",label= "Select Variables", choices = mychoices_num, selected= "Layer1", multiple = FALSE),
                            sliderInput("uMultiplier", label = "IQR multiplier", min = 0, max = 4, value = 1.5, step=0.1)),
                            mainPanel(plotOutput("uBox",height="500", width = "150%")))),
                 tabPanel("Cook",
                          plotOutput("cookplot",height="500", width = "150%"))
      ),
      tabPanel("Data Split",sliderInput("Training observations",label = "Percentage of Training Observations",value = 80, min = 0, max = 100,step =5),
               verbatimTextOutput("ds")),
      
      navbarMenu("Dimensional Reduction",
                 tabPanel("Scree Plot", plotOutput("screeplot"),verbatimTextOutput("scree_explain")),
                 tabPanel("PCA Summary",div(style="width:120%;",fluidRow(verbatimTextOutput("pca_summary")))),
                 tabPanel("PLS plot",plotOutput("plsPlot"),verbatimTextOutput("pls_explain"))
                 
                 
      ),
      
      navbarMenu("Train",
                 tabPanel("PCA Summary",
                          sidebarLayout(sidebarPanel(
                            #sliderInput("comp","Number of principal components", min= 1, max= 30, value = 6  ),
                            textAreaInput("Formula1", label = "Formula1", rows = 8, value = "Y ~Priority+ Price+Speed+Duration+Location+State+
                                          Class+Surface+PC2+PC3+PC4+PC5+PC6")),
                            mainPanel(div(style="width:120%;",fluidRow(verbatimTextOutput("ModelSummary1"))),div(style="width:120%;",fluidRow(verbatimTextOutput("pca_explain")))))),
                 tabPanel("PLS Summary",sliderInput("comp", label = "component", min = 1, max = 52, value = 21, step=1),div(style="width:120%;",fluidRow(verbatimTextOutput("pls_summary"))))
      ),
      
      # navbarMenu(" Linear Model Test",
      #            tabPanel("Model output", plotOutput("LMModelPlot")),
      #            tabPanel("Residual plot", plotOutput("hist_residuals"))
      # ),
      
      navbarMenu("Test Phase",
                 tabPanel("Model output", plotOutput("PCAModelPlot"),verbatimTextOutput("rmse"),verbatimTextOutput("results")),
                 tabPanel("Residual plot",plotOutput("pca_hist_residuals"),div(style="width:150%;",fluidRow(verbatimTextOutput("resid")))),
                 tabPanel("PLS model output", plotOutput("predicted"),div(style="width:150%;",fluidRow(verbatimTextOutput("pls_rmse"))),div(style="width:150%;",fluidRow(verbatimTextOutput("rmse_pls"))),div(style="width:150%;",fluidRow(verbatimTextOutput("pls_rsquare"))),div(style="width:150%;",fluidRow(verbatimTextOutput("rSquare"))))
                 
                 
      )
    ))
)
)




