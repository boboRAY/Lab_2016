require('caret')
require('randomForest')
## tdata = read.table('data/abalone/abalone_R.dat')
set.seed(5278)

# for parallel process
library(doMC)
registerDoMC(cores=4)


## # tuning set
## dat <- file('data/abalone/conxuntos.dat')
## valid <- strsplit(readLines(dat), " ")
## valid = lapply(valid, as.integer)

getresult <- function(path, conxuntos_path, conxuntos_k_path){
  tdata = read.table(path)
  dat <- file(conxuntos_path)
  valid <- strsplit(readLines(dat), " ")
  valid = lapply(valid, as.integer)

  # find best parameters
  tdata[,"clase"] <- as.factor(tdata[,"clase"])
  trControl = trainControl(method="cv",number=2, index=list(Fold1=valid[[1]]), allowParallel=T,savePredictions=T)
  grid = expand.grid(mtry=c(seq(2,29,by=3)))
  rf_model = train(clase ~ . ,data=tdata, method='rf', trainControl=trControl, tuenGrid=grid) 
  
  tc <- trainControl(method="cv",number=1,index=list(Fold1=valid[[1]]),savePredictions=T)
  (rf_model <- train(clase ~ ., data=tdata,method="rf",trControl=tc, tuneGrid=grid))
  
  # training and testing 
  conxuntos_dat = file(conxuntos_k_path)
  conxuntos_kfold = strsplit(readLines(conxuntos_dat)," ")
  conxuntos_kfold = lapply(conxuntos_kfold, as.integer)
  best_mtry = 2
  
  for(i in seq(1,7,by=2)){
    inTraining = matrix(unlist(conxuntos_kfold[i]),ncol=1, byrow=T)
    inTesting = matrix(unlist(conxuntos_kfold[i+1]),ncol=1, byrow=T)
    
    training = tdata[inTraining,]
    testing = tdata[inTesting,]
    
    trControl = trainControl(method='none',allowParallel=T)
    rf = train(clase ~ ., data=training, method='rf', ntree=500, trControl=trControl,tuneGrid=data.frame(mtry=best_mtry))
    
    pred = predict(rf, newdata = testing)
    print(postResample(pred, testing$clase))
  }
}
