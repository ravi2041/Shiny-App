setwd("C:/Users/Ravi Singh/Desktop/data science industry/assignment 3")
library(shiny)
library(ggplot2)
library(caret)
library(ROCR)
library(corrgram)
library(caret)
library(mice)
library(pls)


Data <- read.csv("Ass3Data.csv")
rownames(Data) <- Data[,2]
myData <- Data[,-c(2)]
myData$Date <- as.Date(myData$Date, format = "%Y-%m-%d")
categorical_variables <- c("Author","Priority", "Price","Speed", "Duration","Scarcity","Location","Agreed",
                           "State","Class","Surface")
mychoices = c("Layer1", "Layer2","Layer3","Layer4","Layer5","Layer6","Layer7","Layer8","Layer9","Layer10",
              "Layer11","Layer12","Layer13","Layer14","Layer15","Layer16","Layer17","Layer18","Layer19",
              "Layer20","Layer21","Layer22","Layer23","Layer24","Layer25","Layer26","Layer27","Layer28","Layer29","Layer30")
summary(myData)
myData$Author=factor(myData$Author,labels=c('KL','NW','PR','XX'))
myData$Priority=factor(myData$Priority,labels=c('High','Low','Medium'))
myData$Price=factor(myData$Price,labels=c('Cheap','Costly','Extravagant'))
myData$Speed=factor(myData$Speed,labels=c('Fast','Medium','Slow'))
myData$Duration=factor(myData$Duration,labels=c('Long','Perpetual','Short'))
myData$Scarcity=factor(myData$Scarcity,labels=c('Common','Rare','Typical'))
myData$Location=factor(myData$Location,labels=c('Boiler','Plant_A','Press_room'))
myData$Agreed=factor(myData$Agreed,labels=c('No','Yes'))
myData$State=factor(myData$State,labels=c('Checked','Uncertain','Under_review'))
myData$Class=factor(myData$Class,labels=c('Class_G','Class_K','Class_Y'))
myData$Surface=factor(myData$Surface,labels=c('Rough','Smooth','Textured'))
# Define server logic 
shinyServer(
  function(input, output) {
    
    getData <- reactive({
      if(input$remove){
        mydata<- myData[-c(1:12), ]
        return(mydata)
        }else {
      return(myData)
    }
    })
    
    
    getSplitInd <- reactive({
      data <- getData()
      total_obs <- nrow(data)
      smp_size <- total_obs *((input$`Training observations`)/100)
      set.seed(101)
      index <- sample(seq_len(nrow(data)), size = smp_size)
      return(round(index))
    })
    
    output$ds <- renderPrint({
      data <-getData()
      total_obs <- nrow(data)
      Total_Training_Obs <- nrow(getTrainData())
      Total_Testing_Obs <- nrow(getTestData())
      cat(" Total Observations :",total_obs ,"\n",
          "Total Training Observations :",Total_Training_Obs,"\n",
          "Total Testing Observations :",Total_Testing_Obs)
    })
    
    getTrainData <- reactive({
      mydata <- myData[getSplitInd(), ]
      if(input$omit){
        mydata <- na.omit(mydata)
        return(mydata)
      }else {
        return(mydata)
      }
    })
    
    getTestData <- reactive({
      mydata <- myData[-getSplitInd(), ]
      if(input$omit){
        mydata <- na.omit(mydata)
        return(mydata)
      }else{
        return(mydata)
      }
    })

    pcaCompTrain <- reactive({
      mydata <- na.omit(getTrainData())
      #fact_creator <-model.matrix(~.,mydata[,categorical_variables])#, na.action=na.omit)
      prin_comp <- prcomp(mydata[,mychoices])
      #train_data <-data.frame(fact_creator[,-1],prin_comp$x)
      train_data <-data.frame(mydata[,categorical_variables],prin_comp$x)
      train_data <-data.frame(mydata$Y,train_data)
      colnames(train_data)[1] <- "Y"
      return(train_data)

    })
    
    pcaCompTest <- reactive({
        mydata <- na.omit(getTestData())
        #fact_creator <-model.matrix(~.,mydata[,categorical_variables])#, na.action=na.omit)
        prin_comp <- prcomp(mydata[,mychoices])
        #test_data <-data.frame(fact_creator[,-1],prin_comp$x)
        test_data <-data.frame(mydata[,categorical_variables],prin_comp$x)
        test_data <-data.frame(mydata$Y,test_data)
        colnames(test_data)[1] <- "Y"
        return(test_data)
    }) 
    
    output$plsPlot <-renderPlot({
      mydata <- getTrainData()
      model <- plsr(Y ~ ., ncomp = 53, data = mydata, validation = "CV")
      plot(RMSEP(model), legendpos = "topright")
      # plot(model, "loadings", comps = 1:10, legendpos = "topleft")
      # abline(h = 0)
      
    })
    
    output$pls_summary <-renderPrint({
      mydata <- getTrainData()
      n<- input$comp
      model <- plsr(Y ~ ., ncomp = n, data = mydata, validation = "CV")
      summary(model)
    })
    
    output$predicted <- renderPlot({
      mydata <- getTrainData()
      n <- input$comp
      model <- plsr(Y ~ ., ncomp = n, data =mydata, validation = "CV")
      plot(model, ncomp = n, asp = 1, line = TRUE)
    })
      
    output$rmse_pls <- renderPrint({
      mydata <- getTrainData()
      test <- getTestData()
      n <- input$comp
      model <- plsr(Y ~ ., ncomp = n, data = mydata, validation = "CV")
      RMSEP(model, newdata= test)
    }) 
    
    
    output$rSquare <- renderPrint({
      mydata <- getTrainData()
      test <- getTestData()
      n <- input$comp
      model <- plsr(Y ~ ., ncomp = n, data = mydata, validation = "CV")
      R2(model)
    })
      
    output$table <- DT::renderDataTable({
      myData <- getData()
      DT::datatable(myData)
    })
    
    output$glimpse <- renderPrint({
      myData <- getData()
      dplyr::glimpse(myData)
    })
    
    cooksd <- reactive({
      train_data <- getData()
      mod<- lm(Y~., data= train_data)
      cooksd <- cooks.distance((mod))
      return(cooksd)
    }) 
    
    output$cookplot <- renderPlot({
      d <- cooksd()
      plot(d, pch ="*", cex= 2,col= 'blue', main = "Influential obs by cooks distance")
      
      abline(h=4*mean(d, na.rm = T), col='red')
      text(x=1:length(d)+1, y= d, labels= ifelse(d>4*mean(d, na.rm= T),
                                                           names(d),""), col="red")
    })
    
    output$interaction <- renderPlot({
      mydata <- getData()
      p <-ggplot(mydata,aes(x=mydata[,input$categorical],y=Y,color=mydata[,input$categorical1]))
      p<- p+geom_boxplot()+theme_bw()
      p <- p+labs(x = input$categorical,color =input$categorical1 )
      p
    })
    
    output$summary <- renderPrint({
      trainData <- getTrainData()
      testData <- getTestData()
      mydata <- rbind(trainData,testData)
      summary(mydata)
    })
    
    output$corrPlot <- renderPlot({
      trainData <- getTrainData()
      testData <- getTestData()
      mydata <- rbind(trainData,testData)
      corrgram(mydata, order=TRUE, main="Correlogram",
               upper.panel=panel.pie)
    })
    
    output$missPlot <- renderPlot({
      trainData <- getTrainData()
      testData <- getTestData()
      mydata <- rbind(trainData,testData)
      vis_miss(mydata)
    })
    
    output$matplot <- renderPlot({
      mydata <-getData()
      matplot(y= mydata[,mychoices], type='l', xlab= "Obsevations", ylab = ' Values')
      legend(legend= colnames(mydata[,mychoices]), x= "topright", y="topright", ncol=3)
    })
    
    output$screeplot <- renderPlot({
      y <- getTrainData()
      y <- na.omit(y)
      prin_comp <- prcomp(y[,mychoices] )
      screeplot(prin_comp,type="lines", main="Principal Component")
    })
    
    output$pca_summary <- renderPrint({
      mydata <- na.omit(myData)
      prin_comp <- prcomp(mydata[,mychoices])
      summary(prin_comp)

    })

    
    getModelPca <- reactive({
      train_data <- pcaCompTrain()
      train_control <- trainControl(method="cv", number=10)
      model <-  train(as.formula(input$Formula1), data=train_data, trControl=train_control, method="lm")
      #model <- lm(formula=as.formula(input$Formula1), data = train_data)
      return(model)
    })
    
    output$ModelSummary1 <-renderPrint({
      summary(getModelPca())
    })
    

    
    output$PCAModelPlot <- renderPlot({
      #ModelPlotData <- getData()
      model <- getModelPca()
      pred <-  predict(model, pcaCompTest())
      actual <- pcaCompTest()$Y[ rownames(pcaCompTest()) %in% names(pred) ]
      plot(actual, pred, xlab = "Actual", ylab="Predicted",pch=21, cex=1.5, col='red')
      abline(0,1)
    })
    
    output$results <- renderPrint({
      model <- getModelPca()
      train_obs <- nrow(getTrainData())
      test_obs <- nrow(getTestData())
      params <- length(model$coefnames)
      cat(paste("Number of Training Observations of : ", paste(train_obs)))
      cat(paste("\nNumber of Testing Observations: ", paste(test_obs)))
      cat(paste("\nNumber of Parameters used for Modelling : ", paste(params)))
    })

    
    output$pca_hist_residuals <- renderPlot({
      model <- getModelPca()
      pred <-  predict(model, pcaCompTest())
      actual <- pcaCompTest()$Y[ rownames(pcaCompTest()) %in% rownames(as.data.frame(pred)) ]
      residuals <- actual - pred
      histogram(residuals)
    })
    
    output$uBox <- renderPlot({
      d <- getData()
      d <- d[,input$vc]
      hist(d , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="")
      boxplot(d, boxwex=3, main="", range=input$uMultiplier, horizontal=TRUE, col=rgb(0.8,0.8,0,0.5), frame=F, add=TRUE)
    })
    
    output$uText <- renderPrint({
      bx <-boxplot.stats(myData$Layer27, coef=input$uMultiplier)
      cat(paste("Outliers: \n", paste(bx$out, collapse="\n")))
    })
    
    output$rmse <- renderPrint({
      train_data <- pcaCompTrain()
      model <- lm(formula=as.formula(input$Formula1), data = train_data)
      pred <-  predict(model, pcaCompTest())
      actual <- pcaCompTest()$Y[ rownames(pcaCompTest()) %in% rownames(as.data.frame(pred)) ]
      rmse <- sqrt(mean((actual - pred)^2/length(actual)))
      cat(paste("Root Mean Square Error : ", paste(rmse)))
    })
    
    output$scree_explain <- renderPrint({
      cat(paste("Scree plot shows there are two Principal Components that can explain most of the variation in the model."))
    })
    
    output$mat_explain <- renderPrint({
      cat(paste("There is sharp difference in the first 12 observations which shows there is structural break in the model. We can remove them as it will have influence in the model."))
    })
    
    output$interaction_explain <- renderPrint({
      cat(paste("Random effects can be seen in categorical variables. We can consider them using interactions or we can model them using different modelling technique like Linear mixed effects modelling which considers random effects. While using Linear modelling we have not use interaction of  any variables as it was not producing any significant result."))
    })
    
    output$mis_explain <- renderPrint({
      cat(paste("There are 18 missing values. We can either remove them before applying linear model or let them through for modelling. Some models itself take care of NA values and with some we have to explicilty remove NA values like PCA."))
    })
    
    output$pls_explain <- renderPrint({
      cat(paste("PLS plot is supervised method for making principal components. It takes Y variable while making principal components.According to graph we need to consider 28 principal components for lower root mean square error. We applied PLS to make comparison with PCA model."))
    })
    
    output$pca_explain <- renderPrint({
      cat(paste("The model has high R Square value with 96 percent of variations explained. The ratio of observations to predictors is greater than 10."))
    })
    
    output$pls_rmse <- renderPrint({
      cat(paste(" The root mean square error is given below for each components. It decreases as we increase number of components and later it doesnt have much effect."))
    })
    output$pls_rsquare <- renderPrint({
      cat(paste(" The R Square value is given below for each component. It increases with the increase in components.We have taken all variables as a component(factors and numeric variables). We cannot directly compare PCA with PLS as model generated has different process. "))
    })
    
    output$resid <- renderPrint({
      cat(paste(" Histogram of residual looks normally distributed and has bell shaped curve which signify model is performing good and able to explain variances"))
    })
  })


