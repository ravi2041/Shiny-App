library(shiny)
library(DT)
library(ggplot2)
library(MASS)
library(cluster)
#install.packages("rattle")
data(wine, package='rattle')
wine <- data.frame(matrix(unlist(wine), nrow=178, byrow=T))

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

shinyServer(function(input, output) {
  
  selectedData <- reactive({
    ifelse(input$scale ,scale(wine), wine)
  })
  model <- reactive({
    kmeans(selectedData(),centers = input$cluster_size, algorithm = input$algorithm)
  })

  output$clusterPlot <- renderPlot({
  m <- model()
  clusplot(selectedData(), m$cluster, main='2D representation of the Cluster solution',color=TRUE,
           shade=TRUE,labels=2, lines=0)

  })
  
  output$elbow_plot <- renderPlot({
    m <- model()
    wssplot(selectedData(), nc=6)
  })
  
  output$stats <-DT::renderDataTable({
    #out.data <- formulaScale()
    m <-model()
    DT::datatable(data = table(selectedData(),m$cluster))
  })
})
