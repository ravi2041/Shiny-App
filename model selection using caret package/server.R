library(shiny)
library(DT)
library(caret)
library(doParallel)
library(pls)
library(nnet)
library(glmnet)
library(e1071)
library(randomForest)
library(frbs)
library(Cubist)


clus <- makeCluster(detectCores(all.tests = FALSE, logical = TRUE))
registerDoParallel(clus)  # this will work on windows
trControl <- trainControl("cv", number = 10)  # shared cross validation specification

shinyServer(function(input, output, session) {

  getData <- reactive({
    data <- read.csv(file="Ass4Data.csv")
    rownames(data) <- data$ID
    data$ID <- NULL
    data
  })
  
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    boxplot(d[,numeric], outline=TRUE, main="Boxplot using multiplier of 1.5")
  })
  
  output$DataSummary <- renderPrint({
    str(getData())
  })

  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  
  ############################################################################## 
  getSplit <- reactive({
    createDataPartition(y = getData()$Y, p = input$Split, list = FALSE)
  })
  
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })

  
  ##############################################################################  
  getGlmModels <- reactive({
    method <- "glmnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                  trControl = trControl,
                  tuneGrid = expand.grid(alpha = seq(0,1, 0.1), lambda = seq(0.1, 10, by = 0.1))
    ) # note glmnet does not support parameter "allowParallel"
    removeNotification(id=method)
    mods
  })
  
  output$GlmModelSummary1 <- renderTable({
    mods <- getGlmModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$GlmModelPlots <- renderPlot({
    mods <- getGlmModels()
    plot(mods$finalModel)
  })     
  
  output$GlmModelSummary2 <- renderPrint({
    print(getGlmModels())
  })
  
  
  ##############################################################################
  getPlsModels <- reactive({
    method <- "pls"
    showNotification(id = method, paste("Optimising", method,"hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", scale = FALSE, 
                  trControl = trControl,
                  tuneGrid = expand.grid(ncomp = seq(1, 10, by = 1)),
                  allowParallel = TRUE
                  )
    
    removeNotification(id=method)
    mods
  })
  
  output$PlsModelSummary1 <- renderTable({
    mods <- getPlsModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$PlsModelPlots <- renderPlot({
    plot(getPlsModels())
  })     
  
  output$PlsModelSummary2 <- renderPrint({
    mods <- getPlsModels()
    summary(mods$finalModel)
  })
  
  
  
  ##############################################################################
  getAnnModels <- reactive({
    method <- "nnet"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE", maxit = 1000, trace = F, linout = 1,
                  trControl = trControl,
                  tuneGrid = expand.grid(.decay = seq(0.3, 0.6, by=0.1), .size = seq(4, 8, by=1)),
                  allowParallel = TRUE
                  ) 
    removeNotification(id=method)
    mods
  })
  
  output$AnnModelSummary1 <- renderTable({
    mods <- getAnnModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$AnnModelPlots <- renderPlot({
    plot(getAnnModels())
  })     
  
  output$AnnModelSummary2 <- renderPrint({
    mods <- getAnnModels()
    print(mods$finalModel)
  })
  

  
  
  
  ##############################################################################  
  
  getRFModels <- reactive({
    method <- "rf"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",ntree=1000,
                         trControl = trControl,
                         tuneGrid = expand.grid(mtry = c(3,4,5,6,7,8,9,10)),
                         allowParallel = TRUE
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$rfSummary1 <- renderTable({
    mods <- getRFModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$rfPlots <- renderPlot({
    plot(getRFModels())
  })     
  
  output$rfSummary2 <- renderPrint({
    mods <- getRFModels()
    print(mods$finalModel)
  })
  
  
  
  
  
  ##############################################################################  
  
  getSVMModels <- reactive({
    method <- "svmRadial"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                         trControl = trControl,
                         tuneGrid = expand.grid(sigma = c(0.0001,0.001,0.01,0.05,0.0456,0.0577), C = c(500,1000,5000,10000,15000)),
                         allowParallel = TRUE
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$svmSummary1 <- renderTable({
    mods <- getSVMModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$svmPlots <- renderPlot({
    plot(getSVMModels())
  })     
  
  output$svmSummary2 <- renderPrint({
    mods <- getSVMModels()
    print(mods$finalModel)
  })
  
  
  ##############################################################################  
  
  getGradientboostModel<- reactive({
    method = "gbm"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    tg <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                      interaction.depth = c(1, 3, 7, 10),
                      n.minobsinnode = c(2, 5, 10),
                      n.trees = c(100, 300, 500, 1000))
    
    # Verbose is a parameter sent to the underlying modeling function
    mods <- train(Y ~ ., data = getTrainData(), 
                  method = "gbm", trControl = trControl, tuneGrid =tg, verbose = FALSE)
    
    removeNotification(id=method)
    mods
  })
  
  
  output$boostSummary1 <- renderTable({
    mods <- getGradientboostModel()
    as.data.frame(mods$bestTune)
  })  
  
  output$boostPlots <- renderPlot({
    plot(getGradientboostModel())
  })     
  
  output$boostSummary2 <- renderPrint({
    mods <- getGradientboostModel()
    print(mods$finalModel)
  })
  
  
  ############################################################################## 

  getCUBISTModels <- reactive({
    method <- "cubist"
    showNotification(id = method, paste("Optimising", method, "hyper-parameters using cross validation"), session = session, duration = NULL)
    mods <- caret::train(Y ~ ., data = getTrainData(), method = method, metric = "RMSE",
                         trControl = trControl,
                         tuneGrid = expand.grid(committees = seq(6,20,by=1), neighbors= seq(3,9,by=1)),
                         allowParallel = TRUE
    ) 
    removeNotification(id=method)
    mods
  })
  
  output$cubistSummary1 <- renderTable({
    mods <- getCUBISTModels()
    as.data.frame(mods$bestTune)
  })  
  
  output$cubistPlots <- renderPlot({
    plot(getCUBISTModels())
  })     
  
  output$cubistSummary2 <- renderPrint({
    mods <- getCUBISTModels()
    print(mods$finalModel)
  })
  
  
  ##############################################################################
  
  getAllModels <- reactive({
    list(GLMnet=getGlmModels(), PLS=getPlsModels(), ANN=getAnnModels(), RF = getRFModels(), 
         SVM= getSVMModels(), Boosting = getGradientboostModel() ,Cubist= getCUBISTModels())  # expand this list with further models
    })
  
  output$SelectionSummary <- renderPrint({
    results <- resamples(getAllModels())
    summary(results)
  })
  
  output$SelectionBoxPlot <- renderPlot({
    results <- caret::resamples(getAllModels())
    bwplot(results, notch=input$Notch)
  })
  
  
  ############################################################################## 
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })

  getTestResults <- reactive({
    test <- getTestData()
    mod <- getAllModels()[input$Choice]
    predictions <- predict(mod, newdata=test)
    d <- data.frame(test$Y, predictions)
    colnames(d) <- c("obs", "pred")
    d
    
  })
  
  output$TestSummary <- renderPrint({
    caret::defaultSummary(getTestResults())
  })

  output$TestPlot <- renderPlot({
    plot(getTestResults(), main="Predicted versus Observed")
    abline(a = 0, b=1)
  })

})
