library(shiny)
library(DT)
library(cluster)

variable_choice = c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium","Phenols","Flavanoids","Nonflavanoids","Proanthocyanins","Color","Hue","Dilution","Proline")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("K- Means Modelling"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Number of observations:",
                  min = 7,
                  max = 178,
                  value = 178),
      sliderInput("xNoise",
                  "Scale of X noise:",
                  min = 0,
                  max = 5,
                  value = 0,
                  step = 0.1),
      checkboxInput("scale", "Scaled or Non-Scaled", TRUE),
      selectInput("algorithm","Algorithm to Choose", choices = c("Hartigan-Wong","Lloyd","MacQueen")),
      sliderInput("cluster_size",
                  "Cluster-Size",
                  min=1,
                  max=5,
                  value=3,
                  step=1)),
      
      selectInput("variable", "Variable to choose", choices = variable_choice, selected = variable_choice,multiple = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Cluster Plot",
                 plotOutput("clusterPlot")
        ),
        tabPanel("Dendogram",
                 plotOutput("dendogram")
        ),
        tabPanel("Elbow plot",
                 plotOutput("elbow_plot")
      ),
      tabPanel("Confusion Matrix",
               DT::dataTableOutput("stats")
      )
     )
    )
  )
)
