## Import required libraries
suppressWarnings(library(alookr))
suppressWarnings(library(shiny))
suppressWarnings(library(caret))
suppressWarnings(library('highcharter'))
suppressWarnings(library(rpart))
suppressWarnings(library(plyr))
suppressWarnings(library(dplyr))
suppressWarnings(library(pROC))
suppressWarnings(library(xgboost))
suppressWarnings(library(sqldf))
suppressWarnings(library(tidyverse))
suppressWarnings(library(DiagrammeR))
suppressWarnings(library(DiagrammeRsvg))
suppressWarnings(library(rsvg))
suppressWarnings(library(matrixStats))
suppressWarnings(library(adabag))
suppressWarnings(library(klaR))
suppressWarnings(library(ggplot2))
suppressWarnings(library(plotly))
suppressWarnings(library(magrittr))
suppressWarnings(library(kernlab))
suppressWarnings(library(shinyalert))
suppressWarnings(library(readxl))
suppressWarnings(library(corrplot))

## Set seed
set.seed(2022)

## memory settings
# memory.limit(size=1800)

## Import the example datasets
dat1 <- readRDS("Nilsson_rare_notransform_2022.rds")
dat2 <- readRDS("Mosmann_rare_notransform_2022.rds")
dat03 <- read.csv("SIN3_Network.csv")

## Define the input function
input_func <- function(x){
  if (x=="Nilsson"){
    return(dat1)
  }else if(x=="Mosmann"){
    return(dat2)
  }else{
    return(dat03)
  }
}

## Define Tops
tops <- function(dataset){
  tops1 = dataset+ abs(min(dataset));
  m=sum(tops1)
  tops2 = data.frame()
  t = colSums(tops1, na.rm = FALSE, dims = 1)
  q = rowSums(tops1, na.rm = FALSE, dims = 1)

  for(i in 1:nrow(tops1)){
    for(j in 1:ncol(tops1)){
      #print(paste(i,j))
      if(t[j]==0 || q[i]==0 || tops1[i,j]==0){
        tops2[i,j]=0
      }else{
        tops2[i,j]=tops1[i,j]*log(tops1[i,j]/((t[j]*q[i])/m))
      }
    }
    #print(paste(i,j))
  }
  colnames(tops2) = colnames(dataset)
  return(tops2)
}

## Define the new normalization
newnorm <- function(dataset){
  tops1 = dataset+abs(min(dataset))
  # row_max = matrixStats::rowMaxs(as.matrix(tops1))
  row_max = apply(dataset, 1, max, na.rm=TRUE)
  tops2 = data.frame()
  for(i in 1:nrow(tops1)){
    for(j in 1:ncol(tops1)){
      if(tops1[i,j]==0){
        tops2[i,j]=0
      }else{
      tops2[i,j] = tops1[i,j]/row_max[i]
      }
    }
  }
  colnames(tops2) = colnames(dataset)
  return(tops2)
}

## Creating the X matrix
removecolumn <- function(df, nameofthecolumn){
  df[ , -which(names(df) %in% nameofthecolumn)]
}


server <- function(session,input, output) {
  
  ## Importing data set as per user's choice
  dataset <- reactive({
    if (is.null(input$dat3_csv)==FALSE || is.null(input$dat3_tab)==FALSE ||
        is.null(input$dat3_excel)==FALSE || is.null(input$dat3_rds)==FALSE){
      if(input$file_type=='csv'){
        read.csv(file=input$dat3_csv$datapath)#, header=input$header)
      }else if(input$file_type=="tab"){
        read.delim(file=input$dat3_tab$datapath)
      }else if(input$file_type=='excel'){
        read_excel(input$dat3_excel$datapath)
      }else{
        readRDS(input$dat3_rds$datapath)
      }
    }else if(is.null(input$dat)==FALSE){
      input_func(input$dat)
    }else if(input$dat == "Select your own" & is.null(input$dat3)){
      showNotification("This is a notification.")
    }
  })
  
  
  ## Select response
  output$var_ui <- renderUI({
    selectInput(
      "y", "Choose response", choices = names(dataset())
    )
  })
  
  y <- reactive({input$y}) #variable with names for response
  
  label <- reactive({
    dataset()%>% dplyr::select(c(y()))
      })

  ## Select unwanted fields
  output$unwanted <- renderUI({
    checkboxGroupInput(
      "rem", "Select unwanted variables", choices = names(dataset()))
  })
  
  observeEvent(input$y, {
    
    waiter_show( # show the waiter
      html = spin_fading_circles() # use a spinner
    )
    waiter_hide() # hide the waiter
  })
  
  
  rem <- reactive({input$rem})
  
  ## remove the labels
  X.set <- reactive({removecolumn(dataset(), c(y(), rem()))})

# if(is.null(input$tree) == TRUE || is.null(TRUEinput$nontree) == TRUE || is.null(input$linear) == TRUE){
#     Sys.sleep(3)
#     shinyalert::shinyalert("No model selected",
#                            type="error")
#     Sys.sleep(3)
#     session$reload()
#   }  
  
observeEvent(input$SubmitAll,
             {
               shinyalert::shinyalert("App running, please wait! This might take a while!!",
                                      type="info")
               if(is.null(input$tree) == TRUE && is.null(input$nontree) == TRUE && is.null(input$linear) == TRUE){
                   Sys.sleep(2)
                   shinyalert::shinyalert("No model selected",
                                          type="error")
                   # Sys.sleep(3)
                   # session$reload()
                 }else {

  ##Normalization methods: "Min-max", "Log Scaling", "Standard Scaling", "Arcsine", "None"
    norm.data <- reactive(
          { 
           if(input$norm == "Log Scaling"){
            log(as.data.frame(X.set())+input$logc)
          }else if(input$norm == "Min-max"){
            predict(caret::preProcess(as.data.frame(X.set()), method=c("range")), as.data.frame(X.set()))
          }else if(input$norm == "Standard Scaling"){
            as.data.frame(base::scale(as.data.frame(X.set())))
          }else if(input$norm == "Arcsine"){
            if(input$arcsine_h==TRUE){asinh(as.data.frame(X.set())/input$cofactor)
              }else{asin(as.data.frame(X.set())/max(X.set()))}
          }else if(input$norm == "TopS"){
            if(input$dat == "Nilsson"){
              readRDS("Tops_Nilsson.rds")
            }else if(input$dat == "Mosmann"){
              readRDS("Tops_Mosmann.rds")
            }else{
              tops(dataset=X.set())
            }
          }else if(input$norm == "Percentage Row"){
            if(input$dat == "Nilsson"){
              read.csv("Nilsson_Percentagerow_onlyX.csv")
            }else if(input$dat == "Mosmann"){
              readRDS("Mosmann_Percentrow_orig.rds")
            }else{
              newnorm(dataset=X.set())
            }
          }else if(input$norm == "No Normalization"){
            as.data.frame(X.set())
          }
    }
  )

    combined_data = reactive({
      data.frame(cbind(norm.data(), label()))
      })
    
    output$norm_col = renderTable(head(norm.data(),10))
    
    set.seed(2022)
    trainIndex <- reactive({
      ratio <- input$ratio/100
      caret::createDataPartition(unlist(label()),p=ratio, list=FALSE)
      })

    train <- reactive({
      as.data.frame(combined_data())[trainIndex(),]
    })
    
    test <- reactive({
      as.data.frame(combined_data())[-trainIndex(),]
      })
    
    ## Creating X.train and X.test
    X.train <- reactive({
      train() %>% select(-c(y()))
    })
    
    X.test <- reactive({
      test() %>% select(-c(y()))
    })
    
    ## Creating y.train and y.test
    y.train <- reactive({
      train() %>% select(y())
    })
    
    y.test <- reactive({
      test() %>% select(y())
    })
    
    ## Required for cross validation
    folds <- reactive({
        createFolds(y.train()[,1], k = input$kval)
    })


  ## Train control for cross validation
    train_control <- reactive({
      trainControl(method = "cv", number = input$kval, search = 'random')
    })
    
  ## Function for metrics
    EM <- function(pred,ytest){
      CM = caret::confusionMatrix(as.factor(pred), as.factor(ytest), positive="1")
      Metrics = c(Accuracy = CM$overall[[1]],
                     Sensitivity = CM$byClass[[1]],
                     Specificity = CM$byClass[[2]],
                     Kappa = CM$overall[[2]],
                     ROC = round(roc(as.numeric(ytest), as.numeric(pred))$auc,5))
      return(Metrics)
      
    }
    
    find_cutpt <- function(x,ytest){
      lim <- seq(0.1,1,0.1)
      cutpt <- matrix(NA, ncol=2, nrow = length(lim))
      colnames(cutpt) <- c("Cutoff", "ROCAUC")
      for (j in 1:length(lim)){
        a = as.numeric(lim[j])
        pred_class <- ifelse (x> a,1,0)
        AUC <- roc(ytest, as.numeric(unlist(pred_class)))
        #print(a)
        #print(AUC$auc)
        cutpt[j,1] <- a
        cutpt[j,2] <- AUC$auc
      }
      return(cutpt)
    }
    
    
  ## Selecting ML methods:Tree based: "Random Forest","XGBoost", "AdaBoost"
    ## Creating functions for DT with CV
    DT <- function(y, X.data, tc, ytest, Xtest){
      set.seed(2022)
      mod = rpart(y ~ ., X.data, control=rpart.control(xval=tc))
      varimp = caret::varImp(mod)
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred = predict(mod, Xtest)
      cutput <- find_cutpt(pred,ytest)
      pred1 = ifelse (pred > cutput[which.max(cutput[,2])],1,0) 
      metrics = EM(pred1, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics, Predict=pred1))
      #return(mod)
    }  
    
    ## XGboost with Cv
    XGB <- function(y, X, tc, ytest, Xtest){
      label_tr_xgb = as.numeric(unlist(y))
      label_ts_xgb = as.numeric(unlist(ytest))
      xgb_train <- xgb.DMatrix(data=as.matrix(X, nrow=nrow(X)),
                               label=label_tr_xgb)
      xgb_test <- xgb.DMatrix(data=as.matrix(Xtest, nrow=nrow(Xtest)),
                              label=label_ts_xgb)
      params <- list(booster = "gbtree", 
                     objective = "binary:logistic", 
                     eta=0.3, gamma=0, 
                     max_depth=6,
                     min_child_weight=1, 
                     subsample=1,
                     colsample_bytree=1)
      xgbcv <- xgb.cv(params = params, 
                      data = xgb_train, 
                      nrounds = 100, 
                      nfold = tc, 
                      showsd = T, 
                      stratified = T, 
                      print_every_n = 10, 
                      early_stop_rounds = 20, 
                      maximize = F, set.seed=2022)
      val <- which.min(xgbcv$evaluation_log$test_logloss_mean)
            mod <- xgb.train(params = params, data = xgb_train, nrounds = val, 
                       eval_metric = "error")
      xgbpred<- predict(mod, xgb_test)
      cutput <- find_cutpt(xgbpred,ytest)
      xgbpred1 <- ifelse (xgbpred > cutput[which.max(cutput[,2])],1,0)
      metrics = EM(xgbpred1, ytest)
      varimp = xgb.importance(feature_names = colnames(X),model = mod)
      varimp1 = as.data.frame(varimp[,c(1,2)])
      colnames(varimp1) = c("Variable", "Overall.0")
      c = data.frame(Variable = colnames(Xtest), Overall.1 = rep(0,ncol(Xtest)))
      d = merge(varimp1,c, by="Variable", all.y=TRUE)
      d$Overall = ifelse(is.na(d$Overall.0)==TRUE, d$Overall.1, d$Overall.0)
      varimp2 = d %>% select("Variable", "Overall")
      return(list(Varimp = varimp2, Eval_metrics = metrics,  Predict=xgbpred1))
    }
  
    ## RF
    RF <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y = y, x = X,
                   method = 'rf',
                   metric = 'Accuracy',
                   tuneLength  = 15, 
                   trControl = tc)
      varimp = caret::varImp(mod)$importance
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics, Predict=pred))
    }
    
    ## ADA 
    ADA <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y = y, x = X,
                          method = 'AdaBoost.M1',
                          metric = 'Accuracy',
                          trControl = tc)
      varimp = caret::varImp(mod)$importance
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred))
    }
    

    ## Selecting tree-based models
    mod1 = reactive({
      if(grep("Decision Tree", input$tree)>0){
        DT(y.train()[,1], X.train(), input$kval, y.test()[,1], X.test())
      }
    })
    
    
    mod2 = reactive({
      if(grep("Random Forest", input$tree)>0){
        RF(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod3 = reactive({
      if(grep("XGBoost", input$tree)>0){
        XGB(y.train()[,1], X.train(), input$kval, y.test()[,1], X.test())
      }
    })

    
    mod4 = reactive({
      if(grep("AdaBoost", input$tree)>0){
       ADA(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })


    ## Non-tree based models
    ## Naive Bayes
    NB <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y = y, x = X,
                          method = 'nb',
                          trControl = tc)
      varimp = caret::varImp(mod)$importance[1]
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      colnames(varimp1) <- c("Variable", "Overall")
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred))
    }
    
    ## SVM
    LSVM <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y=y, x=X,
                          method = "svmLinear", 
                          trControl = tc,
                          tuneLength=10)
      varimp = caret::varImp(mod)$importance[1]
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      colnames(varimp1) <- c("Variable", "Overall")
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred))
    }
    
    ## Non-linear SVM
    RADSVM <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y=y, x=X,
                          method = "svmRadial", 
                          trControl = tc,
                          tuneLength=10)
      varimp = caret::varImp(mod)$importance[1]
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      colnames(varimp1) <- c("Variable", "Overall")
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics, Predict=pred))
    }
    
    ##Polynomial
    POLYSVM <- function(y, X, tc, ytest, Xtest){
      mod <- caret::train(y = y, x=X,
                          method = "svmPoly", 
                          trControl = tc,
                          tuneLength=10)
      varimp = caret::varImp(mod)$importance[1]
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      colnames(varimp1) <- c("Variable", "Overall")
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics, Predict=pred))
    }
    
    
    ## Selecting the non-tree based model
    mod5 = reactive({
      if(grep("Naive Bayes", input$nontree)>0){
        NB(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod6 = reactive({
      if(grep("Linear SVM", input$nontree)>0){
        LSVM(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod7 = reactive({
      if(grep("Non-linear SVM", input$nontree)>0){
        RADSVM(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod8 = reactive({
      if(grep("Polynomial SVM", input$nontree)>0) {
        POLYSVM(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    ## Linear classifiers
    ## Logistic regression
    LR <- function(y, X, train_control, ytest, Xtest){
      mod <- caret::train(y=y, x= X,
                          trControl = train_control,
                          method = 'glmnet',
                          family='binomial')
      varimp = caret::varImp(mod)$importance
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred))
    }
    
    
    ## LDA
    LDA <- function(y, X, train_control, ytest, Xtest){
      mod <- caret::train(y=y, x= X,
                          trControl = train_control,
                          method = 'lda', metric="Accuracy")
      varimp = varImp(mod)$importance[1]
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      colnames(varimp1) <- c("Variable", "Overall")
      pred<- predict(mod, Xtest)
      metrics = EM(pred, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred))
    }
    
    ## Lasso
    LASSO <- function(y, X, kval, ytest, Xtest){
      mod <- glmnet::glmnet(as.matrix(X, ncol=ncol(X)),
                            unlist(y),
                            alpha = 1, family = "binomial", 
                            nfolds=kval)
      num <- which(mod$lambda== min(mod$lambda))
      varimp = varImp(mod,lambda = min(mod$lambda))
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred <- predict(mod, newx = as.matrix(Xtest, ncol=ncol(X)),
                      s=mod$lambda.min)
      cutpt <- find_cutpt(pred[,num],ytest)
      val = which.max(cutpt[,2])
      pred_classes <- ifelse(pred> cutpt[val], 1, 0)[,num]
      metrics = EM(pred_classes, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred_classes))
    }
    
    ## Ridge
    RIDGE <- function(y, X, kval, ytest, Xtest){
      mod <- glmnet::glmnet(as.matrix(X, ncol=ncol(X)),
                            unlist(y),
                            alpha = 0, family = "binomial", #standardize = T,
                            nfolds=kval)
      num <- which(mod$lambda== min(mod$lambda))
      varimp = varImp(mod,lambda = min(mod$lambda))
      varimp$Variable <- rownames(varimp)
      varimp1 = varimp[,c(2,1)]
      rownames(varimp1) <- NULL
      pred <- predict(mod, newx = as.matrix(Xtest, ncol=ncol(X)),
                      s=mod$lambda.min)
      cutpt <- find_cutpt(pred[,num],ytest)
      val = which.max(cutpt[,2])
      pred_classes <- ifelse(pred> cutpt[val], 1, 0)[,num]
      metrics = EM(pred_classes, ytest)
      return(list(Varimp = varimp1, Eval_metrics = metrics,  Predict=pred_classes))
    }
    
    ## Selecting the linear classifiers
    mod9 = reactive({
      if(grep("Logistic Regression", input$linear)>0){
        LR(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod10 = reactive({
      if(grep("LDA",input$linear)>0){
        LDA(as.factor(y.train()[,1]), X.train(), train_control(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod11 = reactive({
      if(grep("Lasso", input$linear)>0){
       LASSO(as.factor(y.train()[,1]), X.train(), input$kval(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    mod12 = reactive({
      if(grep("Ridge",input$linear)>0){
        RIDGE(as.factor(y.train()[,1]), X.train(), input$kval(), as.factor(y.test()[,1]), X.test())
      }
    })
    
    scores_data <- reactive({
      ## Intialize the variables
      dv1 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv2 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv3 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv4 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv5 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv6 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv7 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv8 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv9 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv10 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv11 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      dv12 = data.frame(V1 = colnames(X.train()), V2= rep(0,ncol(X.train())))
      Pred = data.frame(default = rep(0,nrow(X.test())))

      
      ## Rename columns
      colnames(dv1) <- c("Variable","Overall_DT")
      colnames(dv2) <- c("Variable","Overall_RF")
      colnames(dv3) <- c("Variable","Overall_XGB")
      colnames(dv4) <- c("Variable","Overall_ADAB")
      colnames(dv5) <- c("Variable","Overall_NB")
      colnames(dv6) <- c("Variable","Overall_LSVM")
      colnames(dv7) <- c("Variable","Overall_NLSVM")
      colnames(dv8) <- c("Variable","Overall_PSVM")
      colnames(dv9) <- c("Variable","Overall_LR")
      colnames(dv10) <- c("Variable","Overall_LDA")
      colnames(dv11) <- c("Variable","Overall_Lasso")
      colnames(dv12) <- c("Variable","Overall_Ridge")
      
      # Checking models
      if(sum(grepl("Decision Tree", input$tree))>0){
        dv1$Variable = mod1()$Varimp[,1]
        dv1$Overall_DT = mod1()$Varimp[,2]
        Pred$Pred_DT = mod1()$Predict
      }else{
        dv1$Variable = colnames(X.train())
        dv1$Overall_DT = rep(0, length(colnames(X.train())))
      }

      if(sum(grepl("Random Forest", input$tree))>0){
        dv2$Variable = mod2()$Varimp[,1]
        dv2$Overall_RF = mod2()$Varimp[,2]
        Pred$Pred_RF = mod2()$Predict
      }else {
        dv2$Variable = colnames(X.train())
        dv2$Overall_RF = rep(0, length(colnames(X.train())))
      }

      if(sum(grepl("XGBoost", input$tree))>0){
        temp = as.data.frame(mod3()$Varimp)
        dv3$Variable = unlist(temp[,1])
        dv3$Overall_XGB = unlist(temp[,2])
        Pred$Pred_XGB = mod3()$Predict
      }else{
        dv3$Variable = colnames(X.train())
        dv3$Overall_XGB = rep(0, length(colnames(X.train())))
      }

      if(sum(grepl("AdaBoost", input$tree))>0){
        temp = as.data.frame(mod4()$Varimp)
        dv4$Variable = unlist(temp[,1])
        dv4$Overall_ADAB = unlist(temp[,2])
        Pred$Pred_ADAB = mod4()$Predict
      }else{
        dv4$Variable = colnames(X.train())
        dv4$Overall_ADAB = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Naive Bayes", input$nontree))>0){
        temp = as.data.frame(mod5()$Varimp)
        dv5$Variable = unlist(temp[,1])
        dv5$Overall_NB = unlist(temp[,2])
        Pred$Pred_NB = mod5()$Predict
      }else{
        dv5$Variable = colnames(X.train())
        dv5$Overall_NB = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Linear SVM", input$nontree))>0){
        temp = as.data.frame(mod6()$Varimp)
        dv6$Variable = unlist(temp[,1])
        dv6$Overall_LSVM = unlist(temp[,2])
        Pred$Pred_LSVM = mod6()$Predict
      }else{
        dv6$Variable = colnames(X.train())
        dv6$Overall_LSVM = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Non-linear SVM", input$nontree))>0){
        temp = as.data.frame(mod7()$Varimp)
        dv7$Variable = unlist(temp[,1])
        dv7$Overall_NLSVM = unlist(temp[,2])
        Pred$Pred_NLSVM = mod7()$Predict
      }else{
        dv7$Variable = colnames(X.train())
        dv7$Overall_NLSVM = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Polynomial SVM", input$nontree))>0){
        temp = as.data.frame(mod8()$Varimp)
        dv8$Variable = unlist(temp[,1])
        dv8$Overall_PSVM = unlist(temp[,2])
        Pred$Pred_PSVM= mod8()$Predict
      }else{
        dv8$Variable = colnames(X.train())
        dv8$Overall_PSVM = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Logistic Regression", input$linear))>0){
        temp = as.data.frame(mod9()$Varimp)
        dv9$Variable = unlist(temp[,1])
        dv9$Overall_LR = unlist(temp[,2])
        Pred$Pred_LR = mod9()$Predict
      }else{
        dv9$Variable = colnames(X.train())
        dv9$Overall_LR = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("LDA", input$linear))>0){
        temp = as.data.frame(mod10()$Varimp)
        dv10$Variable = unlist(temp[,1])
        dv10$Overall_LDA = unlist(temp[,2])
        Pred$Pred_LDA = mod10()$Predict
      }else{
        dv10$Variable = colnames(X.train())
        dv10$Overall_LDA = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Lasso", input$linear))>0){
        temp = as.data.frame(mod11()$Varimp)
        dv11$Variable = unlist(temp[,1])
        dv11$Overall_Lasso = unlist(temp[,2])
        Pred$Pred_Lasso = mod11()$Predict
      }else{
        dv11$Variable = colnames(X.train())
        dv11$Overall_Lasso = rep(0, length(colnames(X.train())))
      }
      
      if(sum(grepl("Ridge", input$linear))>0){
        temp = as.data.frame(mod12()$Varimp)
        dv12$Variable = unlist(temp[,1])
        dv12$Overall_Ridge = unlist(temp[,2])
        Pred$Pred_Ridge = mod12()$Predict
      }else{
        dv12$Variable = colnames(X.train())
        dv12$Overall_Ridge = rep(0, length(colnames(X.train())))
      }

      df1 <- list(dv1, dv2, dv3, dv4, dv5, dv6, dv7, dv8, dv9, dv10, dv11, dv12)
      df <- df1 %>% purrr::reduce(full_join, by='Variable')
      
      # Checking zeroes and negative values in var imp
      for (i in 1:nrow(df)){
        ## Replace 0
        df$Overall_DT[i] <- ifelse(df$Overall_DT[i] == 0, 0.000000000001, df$Overall_DT[i])
        df$Overall_RF[i] <- ifelse(df$Overall_RF[i] == 0, 0.000000000001, df$Overall_RF[i])
        df$Overall_XGB[i] <- ifelse(df$Overall_XGB[i] == 0, 0.000000000001, df$Overall_XGB[i])
        df$Overall_ADAB[i] <- ifelse(df$Overall_ADAB[i] == 0, 0.000000000001, df$Overall_ADAB[i])
        df$Overall_NB[i] <- ifelse(df$Overall_NB[i] == 0, 0.000000000001, df$Overall_NB[i])
        df$Overall_LSVM[i] <- ifelse(df$Overall_LSVM[i] == 0, 0.000000000001, df$Overall_LSVM[i])
        df$Overall_NLSVM[i] <- ifelse(df$Overall_NLSVM[i] == 0, 0.000000000001, df$Overall_NLSVM[i])
        df$Overall_PSVM[i] <- ifelse(df$Overall_PSVM[i] == 0, 0.000000000001, df$Overall_PSVM[i])
        df$Overall_LR[i] <- ifelse(df$Overall_LR[i] == 0, 0.000000000001, df$Overall_LR[i])
        df$Overall_LDA[i] <- ifelse(df$Overall_LDA[i] == 0, 0.000000000001, df$Overall_LDA[i])
        df$Overall_Lasso[i] <- ifelse(df$Overall_Lasso[i] == 0, 0.000000000001, df$Overall_Lasso[i])
        df$Overall_Ridge[i] <- ifelse(df$Overall_Ridge[i] == 0, 0.000000000001, df$Overall_Ridge[i])
        ## Replace negative
        df$Overall_DT[i] <- ifelse(df$Overall_DT[i] == 0, 0.000000000000001, df$Overall_DT[i])
        df$Overall_RF[i] <- ifelse(df$Overall_RF[i] == 0, 0.000000000000001, df$Overall_RF[i])
        df$Overall_XGB[i] <- ifelse(df$Overall_XGB[i] == 0, 0.000000000000001, df$Overall_XGB[i])
        df$Overall_ADAB[i] <- ifelse(df$Overall_ADAB[i] == 0, 0.000000000000001, df$Overall_ADAB[i])
        df$Overall_NB[i] <- ifelse(df$Overall_NB[i] == 0, 0.000000000000001, df$Overall_NB[i])
        df$Overall_LSVM[i] <- ifelse(df$Overall_LSVM[i] == 0, 0.000000000000001, df$Overall_LSVM[i])
        df$Overall_NLSVM[i] <- ifelse(df$Overall_NLSVM[i] == 0, 0.000000000000001, df$Overall_NLSVM[i])
        df$Overall_PSVM[i] <- ifelse(df$Overall_PSVM[i] == 0, 0.000000000000001, df$Overall_PSVM[i])
        df$Overall_LR[i] <- ifelse(df$Overall_LR[i] == 0, 0.000000000000001, df$Overall_LR[i])
        df$Overall_LDA[i] <- ifelse(df$Overall_LDA[i] == 0, 0.000000000000001, df$Overall_LDA[i])
        df$Overall_Lasso[i] <- ifelse(df$Overall_Lasso[i] == 0, 0.000000000000001, df$Overall_Lasso[i])
        df$Overall_Ridge[i] <- ifelse(df$Overall_Ridge[i] == 0, 0.000000000000001, df$Overall_Ridge[i])
      }


      # ## Finding the probability
      df$prob_DT = as.numeric(df$Overall_DT)/sum(as.numeric(df$Overall_DT))
      df$prob_RF = as.numeric(df$Overall_RF)/sum(as.numeric(df$Overall_RF))
      df$prob_XGB = as.numeric(df$Overall_XGB)/sum(as.numeric(df$Overall_XGB))
      df$prob_ADAB = as.numeric(df$Overall_ADAB)/sum(as.numeric(df$Overall_ADAB))
      df$prob_NB = as.numeric(df$Overall_NB)/sum(as.numeric(df$Overall_NB))
      df$prob_LSVM = as.numeric(df$Overall_LSVM)/sum(as.numeric(df$Overall_LSVM))
      df$prob_NLSVM = as.numeric(df$Overall_NLSVM)/sum(as.numeric(df$Overall_NLSVM))
      df$prob_PSVM = as.numeric(df$Overall_PSVM)/sum(as.numeric(df$Overall_PSVM))
      df$prob_LR = as.numeric(df$Overall_LR)/sum(as.numeric(df$Overall_LR))
      df$prob_LDA = as.numeric(df$Overall_LDA)/sum(as.numeric(df$Overall_LDA))
      df$prob_Lasso = as.numeric(df$Overall_Lasso)/sum(as.numeric(df$Overall_Lasso))
      df$prob_Ridge = as.numeric(df$Overall_Ridge)/sum(as.numeric(df$Overall_Ridge))

      ## Calculate the entropy
      if(sum(grepl("Decision Tree", input$tree))==0){
        df$Entropy_DT = rep(0, length(colnames(X.train())))
        df$prob_DT = rep(0, length(colnames(X.train())))
        df$Overall_DT = rep(0, length(colnames(X.train())))
        df$rank_DT = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_DT = -df$prob_DT*log2(df$prob_DT)
        df$rank_DT = rank(df$Overall_DT, ties.method="average")
      }
      
      if(sum(grepl("Random Forest", input$tree))==0){
        df$Entropy_RF = rep(0, length(colnames(X.train())))
        df$prob_RF = rep(0, length(colnames(X.train())))
        df$Overall_RF = rep(0, length(colnames(X.train())))
        df$rank_RF = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_RF = -df$prob_RF*log2(df$prob_RF)
        df$rank_RF = rank(df$Overall_RF, ties.method="average")
      }
      
      if(sum(grepl("XGBoost", input$tree))==0){
        df$Entropy_XGB = rep(0, length(colnames(X.train())))
        df$prob_XGB = rep(0, length(colnames(X.train())))
        df$Overall_XGB = rep(0, length(colnames(X.train())))
        df$rank_XGB = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_XGB = -df$prob_XGB*log2(df$prob_XGB)
        df$rank_XGB = rank(df$Overall_XGB, ties.method="average")
      }
      
      if(sum(grepl("AdaBoost", input$tree))==0){
        df$Entropy_ADAB = rep(0, length(colnames(X.train())))
        df$prob_ADAB = rep(0, length(colnames(X.train())))
        df$Overall_ADAB = rep(0, length(colnames(X.train())))
        df$rank_ADAB = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_ADAB = -df$prob_ADAB*log2(df$prob_ADAB)
        df$rank_ADAB = rank(df$Overall_ADAB, ties.method="average")
      }
      
      if(sum(grepl("Naive Bayes", input$nontree))==0){
        df$Entropy_NB = rep(0, length(colnames(X.train())))
        df$prob_NB = rep(0, length(colnames(X.train())))
        df$Overall_NB = rep(0, length(colnames(X.train())))
        df$rank_NB = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_NB = -df$prob_NB*log2(df$prob_NB)
        df$rank_NB = rank(df$Overall_NB, ties.method="average")
      }
      
      if(sum(grepl("Linear SVM", input$nontree))==0){
        df$Entropy_LSVM = rep(0, length(colnames(X.train())))
        df$prob_LSVM = rep(0, length(colnames(X.train())))
        df$Overall_LSVM = rep(0, length(colnames(X.train())))
        df$rank_LSVM = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_LSVM = -df$prob_LSVM*log2(df$prob_LSVM)
        df$rank_LSVM = rank(df$Overall_LSVM, ties.method="average")
      }
      
      if(sum(grepl("Non-linear SVM", input$nontree))==0){
        df$Entropy_NLSVM = rep(0, length(colnames(X.train())))
        df$prob_NLSVM = rep(0, length(colnames(X.train())))
        df$Overall_NLSVM = rep(0, length(colnames(X.train())))
        df$rank_NLSVM = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_NLSVM = -df$prob_NLSVM*log2(df$prob_NLSVM)
        df$rank_NLSVM = rank(df$Overall_NLSVM, ties.method="average")
      }
      
      if(sum(grepl("Polynomial SVM", input$nontree))==0){
        df$Entropy_PSVM = rep(0, length(colnames(X.train())))
        df$prob_PSVM = rep(0, length(colnames(X.train())))
        df$Overall_PSVM = rep(0, length(colnames(X.train())))
        df$rank_PSVM = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_PSVM = -df$prob_PSVM*log2(df$prob_PSVM)
        df$rank_PSVM = rank(df$Overall_PSVM, ties.method="average")
      }
      
      if(sum(grepl("Logistic Regression", input$linear))==0){
        df$Entropy_LR = rep(0, length(colnames(X.train())))
        df$prob_LR = rep(0, length(colnames(X.train())))
        df$Overall_LR = rep(0, length(colnames(X.train())))
        df$rank_LR = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_LR = -df$prob_LR*log2(df$prob_LR)
        df$rank_LR = rank(df$Overall_LR, ties.method="average")
      }
      
      if(sum(grepl("LDA", input$linear))==0){
        df$Entropy_LDA = rep(0, length(colnames(X.train())))
        df$prob_LDA = rep(0, length(colnames(X.train())))
        df$Overall_LDA = rep(0, length(colnames(X.train())))
        df$rank_LDA = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_LDA = -df$prob_LDA*log2(df$prob_LDA)
        df$rank_LDA = rank(df$Overall_LDA, ties.method="average")
      }
      
      if(sum(grepl("Lasso", input$linear))==0){
        df$Entropy_Lasso = rep(0, length(colnames(X.train())))
        df$prob_Lasso = rep(0, length(colnames(X.train())))
        df$Overall_Lasso = rep(0, length(colnames(X.train())))
        df$rank_Lasso = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_Lasso = -df$prob_Lasso*log2(df$prob_Lasso)
        df$rank_Lasso = rank(df$Overall_Lasso, ties.method="average")
      }
      
      if(sum(grepl("Ridge", input$linear))==0){
        df$Entropy_Ridge = rep(0, length(colnames(X.train())))
        df$prob_Ridge = rep(0, length(colnames(X.train())))
        df$Overall_Ridge = rep(0, length(colnames(X.train())))
        df$rank_Ridge = rep(0, length(colnames(X.train())))
      }else{
        df$Entropy_Ridge = -df$prob_Ridge*log2(df$prob_Ridge)
        df$rank_Ridge = rank(df$Overall_Ridge, ties.method="average")
      }
      
      ## Entropy score
      df$EntropyScore <- df$Entropy_DT +
                          df$Entropy_RF +
                          df$Entropy_XGB +
                          df$Entropy_ADAB +
                          df$Entropy_NB +
                          df$Entropy_LSVM +
                          df$Entropy_NLSVM +
                          df$Entropy_PSVM +
                          df$Entropy_LR +
                          df$Entropy_LDA +
                          df$Entropy_Lasso +
                          df$Entropy_Ridge 
        

      ## Rank score
      df$RankScore = df$rank_DT + df$rank_RF + df$rank_XGB + df$rank_ADAB +
                      df$rank_NB + df$rank_LSVM + df$rank_NLSVM + df$rank_PSVM +
                      df$rank_LR + df$rank_LDA + df$rank_Lasso + df$rank_Ridge

      scores = df %>% select(c("Variable", "EntropyScore", "RankScore"))
      
      if(sum(grepl("Decision Tree", input$tree))>0 |
         sum(grepl("Random Forest", input$tree))>0 |
         sum(grepl("XGBoost", input$tree))>0 |
         sum(grepl("AdaBoost", input$tree))>0 |
         sum(grepl("Naive Bayes", input$nontree))>0 |
         sum(grepl("Linear SVM", input$nontree))>0 |
         sum(grepl("Non-linear SVM", input$nontree))>0 |
         sum(grepl("Polynomial SVM", input$nontree))>0 |
         sum(grepl("Logistic Regression", input$linear))>0 |
         sum(grepl("LDA", input$linear))>0 |
         sum(grepl("Lasso", input$linear))>0 |
         sum(grepl("Ridge", input$linear))>0){
          Pred1 = Pred %>% select(-c(default))
          Pred1 = data.frame(lapply(Pred1, function(x) as.numeric(as.character(x))))
          Pred1$Pred_final = ifelse(rowSums(Pred1)/ncol(Pred1)>0.5,1,0)
      }else{
        Pred1$"No_Model" = rep(0, nrow(X.test()))
      }

      #return(list(df=df, Predict=Pred, Scores=scores))
      return(list(df=df, Scores=scores, Predict=Pred1))
    })  
    
    ## Download the scores
    scores_output = reactive({
      arrange(as.data.frame(scores_data()$Scores), desc(EntropyScore))
    })
    output$Scores_Data <- downloadHandler(
      filename = function() {
        paste("Scores_", Sys.Date(),".csv", sep="")
      },
      content = function(file) {
        write.csv(arrange(as.data.frame(scores_data()$df), desc(EntropyScore)), file, row.names = FALSE)}
    )  
    output$scores = renderTable(scores_output())
    
    ## Evaluation metrices output
    ### All models
    Eval_Tab  <- reactive({
      df = data.frame(Metrices = c("Accuracy", "Sensitivity", "Specificity",
                                   "Kappa", "AUC"),
                      Integrative_ML = rep(0, 5))

      ## If none of the models were selected
      if(sum(grepl("Decision Tree", input$tree))==0 &
         sum(grepl("Random Forest", input$tree))==0 &
         sum(grepl("XGBoost", input$tree))==0 &
         sum(grepl("AdaBoost", input$tree))==0 &
         sum(grepl("Naive Bayes", input$nontree))==0 &
         sum(grepl("Linear SVM", input$nontree))==0 &
         sum(grepl("Non-linear SVM", input$nontree))==0 &
         sum(grepl("Polynomial SVM", input$nontree))==0 &
         sum(grepl("Logistic Regression", input$linear))==0 &
         sum(grepl("LDA", input$linear))==0 &
         sum(grepl("Lasso", input$linear))==0 &
         sum(grepl("Ridge", input$linear))==0){
            df$Integrative_ML = rep(0,5) 
      }else{
        if(sum(grepl("Decision Tree", input$tree))>0){
          df$DT = mod1()$Eval_metrics
        }
        
        if(sum(grepl("Random Forest", input$tree))>0){
          df$RF = mod2()$Eval_metrics
        }
        
        if(sum(grepl("XGBoost", input$tree))>0){
          df$XGB = mod3()$Eval_metrics
        }
        
        if(sum(grepl("AdaBoost", input$tree))>0){
          df$ADAB = mod4()$Eval_metrics
        }
        
        if(sum(grepl("Naive Bayes", input$nontree))>0){
          df$NB = mod5()$Eval_metrics
        }
        
        if(sum(grepl("Linear SVM", input$nontree))>0){
          df$LSVM = mod6()$Eval_metrics
        }
        
        if(sum(grepl("Non-linear SVM", input$nontree))>0){
          df$NLSVM = mod7()$Eval_metrics
        }
        
        if(sum(grepl("Polynomial SVM", input$nontree))>0){
          df$PSVM = mod8()$Eval_metrics
        }
        
        if(sum(grepl("Logistic Regression", input$linear))>0){
          df$LR = mod9()$Eval_metrics
        }
        
        if(sum(grepl("LDA", input$linear))>0){
          df$LDA = mod10()$Eval_metrics
        }
        
        if(sum(grepl("Lasso", input$linear))>0){
          df$Lasso = mod11()$Eval_metrics
        }
        
        if(sum(grepl("Ridge", input$linear))>0){
          df$Ridge = mod12()$Eval_metrics
        }
        
        Pred = scores_data()$Predict[, ncol(scores_data()$Predict)]
        df$Integrative_ML = EM(pred = Pred, as.factor(y.test()[,1]))
      }
      return(df)
    })
    
    ## Download the evaluation metrics
    output$Eval_Tab <- downloadHandler(
      filename = function() {
        paste("EvaluationMetrics_", Sys.Date(),".csv", sep="")
      },
      content = function(file) {
        write.csv(Eval_Tab(), file, row.names = FALSE)}
    )
    output$eval = renderTable(Eval_Tab())
    
    ## Calculating the persistent marker using user-specified cutoff 
    Persistent <- reactive({
      df = as.data.frame(scores_data()$Scores)
      cutoff = as.numeric(input$Cutpt/100)*ncol(X.train()) 
      df$rank1 = rank(df[,2], ties.method="average")
      df$rank2 = rank(df[,3], ties.method = "average")
      df$Pers1 = as.numeric(df$rank1>=cutoff)
      df$Pers2 = as.numeric(df$rank2>=cutoff)
      df$Pers3 = df$Pers1 + df$Pers2
      df$Selected = ifelse((df$Pers3)==2, 1, 0)
      df$Unselected = ifelse((df$Pers3)==0, 1, 0)
      df$Fluctuating = ifelse((df$Pers3)==1, 1, 0)
      # 
      # Pers_output = df %>% select("Variable", 
      #                             "Selected",
      #                             "Unselected",
      #                             "Fluctuating")
      
      df1 = df %>% select(-c("rank1", "rank2", "Pers1", "Pers2", "Pers3"))
      return(df1)
    })
      
    
    ## Download the feature structure
    output$Feature_Structure <- downloadHandler(
      filename = function() {
        paste("FeatureStructure_", Sys.Date(),".csv", sep="")
      },
      content = function(file) {
        write.csv(Persistent(), file, row.names = FALSE)}
    )
    
    output$PFS = renderTable(Persistent())
    
  ## Output Tab    
    
    ## Download the normalized data output
    output$Normal_Table <- downloadHandler(
      filename = function() {
        paste("Normalized_data_", Sys.Date(),".csv", sep="")
    },
      content = function(file) {
        write.csv(combined_data(), file, row.names = FALSE)}
    )
    

    ## Normalization plot
    plot.data <- reactive({
      reshape2::melt(norm.data())
    })
    
    plot1 <- reactive({
      ggplot2::ggplot(data=plot.data(), aes(x=variable, y=value, fill=variable)) +
      stat_boxplot(geom = "errorbar",
                     width = 0.15) +
      geom_boxplot(outlier.shape = NA) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black"),
              legend.position = "none",
              axis.title = element_text(face="bold", size=12),
              axis.text =  element_text(face="bold", size=11, 
                                        colour="darkslategrey"),
              axis.text.x = element_text(angle = 45, hjust=1)) +
        title(main="Normalized data") 
    })
    
    output$plot1 <- renderPlot({
        plot1()
    })
    
    output$plot1download <- downloadHandler(
      filename = function() {
        paste("Normalized_boxplot_", Sys.Date(),".png", sep="")
      },
      content = function(file) {
        # ggplot2::ggsave(file,plot = plot1(),
        #                 scale=1, dpi = 300)
        png(file)
        plot1()
        dev.off()
        }
    )
    
  
    ## Plot 2
    plot.data1 <- reactive({
      data.frame(lapply(na.omit(norm.data()), function(x) as.numeric(as.character(x))))
    })
    
    plot2 <- reactive({
      corr = cor(plot.data1(), method=c("spearman"))
      corrplot::corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
                         tl.cex=0.8)
    })
    
    output$plot2 <- renderPlot({
      plot2()
    })
    
    output$plot2download <- downloadHandler(
      filename = function() {
        paste("Correlation_plot_", Sys.Date(),".png", sep="")
      },
      content = function(file) {
        png(file)
        corr = cor(plot.data1(), method=c("spearman"))
        corrplot::corrplot(corr, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
                           tl.cex=0.8)
        dev.off()
        }
    )
      
  ## Feature structure
      plot3 <- reactive({

      grViz("digraph{
      graph[layout=dot, rankdir = TD, fontname='Arial', fontsize='10']
      A[label= < <B> Rare Event </B> >, shape=circle, fillcolor= Coral, style=filled, 
            width=1.5, fixedsize=true, color=coral2]
      
      B1[label ='Persistently\\nSelected', shape=plaintext, width=2, fixedsize=true]
      B2[label = 'Persistently\\nFluctuating', shape=plaintext, width=2, fixedsize=true]
      B3[label = 'Persistently\\nUnselected', shape=plaintext, width=2, fixedsize=true]
      

      C1 [label = '@@1', shape = doubleoctagon, height = 4, width = 2, style = filled, 
            color = darkslategray1,  fillcolor  = darkslategray2, fixedsize=true]
      C2 [label = '@@2', shape = doubleoctagon, height = 4, width = 2, style = filled, 
            color = deepskyblue2, fillcolor = deepskyblue, fixedsize=true]
      C3 [label = '@@3', shape = doubleoctagon, height = 4, width = 2, style = filled, 
            color = dodgerblue3, fillcolor = dodgerblue2, fixedsize=true]


      edge []
      A -> B1 [arrowhead=none] 
      B1 -> C1
      A -> B2 [arrowhead=none] 
      B2 -> C2 
      A -> B3 [arrowhead=none] 
      B3 -> C3
      
     
}
      
      [1]:  paste0(Persistent()$Variable[which(Persistent()$Selected==1)], collapse='\\n')
      [2]:  paste0(Persistent()$Variable[which(Persistent()$Fluctuating==1)], collapse='\\n')
      [3]:  paste0(Persistent()$Variable[which(Persistent()$Unselected==1)], collapse='\\n')
") 
      })

    output$plot3 <- renderGrViz({
      plot3()
    })
    
    output$plot3download <- downloadHandler(
      filename = function() {
        paste("Persistent_Feature_Structure", Sys.Date(),".png", sep="")
      },
      content = function(file) {
        plot3() %>%
          export_svg() %>%
          charToRaw %>%
          rsvg_png(file, width = 600, height =600)
      }
    )
   
  corr <- reactive({
      cor(plot.data1(), method=c("spearman"))
  })
  
  output$corrtable <- downloadHandler(
    filename = function() {
      paste("Spearman_Correlatation_", Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.csv(corr(), file, row.names = FALSE)}
  )
  
  # output$table0 = renderTable(arrange(mod1()$Varimp, desc(Overall)))
  # output$table1 = renderTable(arrange(mod2()$Varimp, desc(Overall)))
  # output$table2 = renderTable(arrange(mod3()$Varimp, desc(Overall)))
  # output$table3 = renderTable(arrange(mod4()$Varimp, desc(Overall)))
  # output$table4 = renderTable(arrange(mod5()$Varimp, desc(Overall)))
  # output$table5 = renderTable(arrange(mod6()$Varimp, desc(Overall)))
  # output$table6 = renderTable(arrange(mod7()$Varimp, desc(Overall)))
  # output$table7 = renderTable(arrange(mod8()$Varimp, desc(Overall)))
  # output$table8 = renderTable(arrange(mod9()$Varimp, desc(Overall)))
  # output$table9 = renderTable(arrange(mod10()$Varimp, desc(Overall)))
  # output$table10 = renderTable(arrange(mod11()$Varimp, desc(Overall)))
  # output$table11 = renderTable(arrange(mod12()$Varimp, desc(Overall)))
  # output$table12 = renderTable(input$tree)
  # output$table13 = renderTable(input$nontree)
  # output$table14 = renderTable(input$linear)
  # output$table15 = renderTable(scores_data()$df)
  # output$table16 = renderTable(y())
  # output$table17 = renderTable(Persistent())
   }
  }) ## End of the submit action button
  
## Reset button
observeEvent(input$resetAll, {
  shinyalert::shinyalert("Restarting awesomeness!!", type="warning")  
  
  session$reload()
  })


}## end of server file

  
