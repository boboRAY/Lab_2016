require('caret')
require('randomForest')
## tdata = read.table('data/abalone/abalone_R.dat')
set.seed(5278)

# for parallel process
library(doMC)
registerDoMC(cores = 4)

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
  grid = expand.grid(mtry=c(seq(2,29,by=3)))
  
  tc <- trainControl(method="cv",number=1,index=list(Fold1=valid[[1]]),savePredictions=T,allowParallel=T)
  rf_model <- train(clase ~ ., data=tdata,method="rf",trControl=tc, tuneGrid=grid)
  print(rf_model)
  

  # training and testing 
  conxuntos_dat = file(conxuntos_k_path)
  conxuntos_kfold = strsplit(readLines(conxuntos_dat)," ")
  conxuntos_kfold = lapply(conxuntos_kfold, as.integer)
  best_mtry = rf_model$bestTune[[1]]
  
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


#----- start -----#
ptm <- proc.time()

# loop all file
dirs = list.dirs('data',full.names=FALSE,recursive=FALSE)
for(dir in dirs){
  root_path = paste('data/',dir,sep='')
  dat_path = paste(root_path,'/',dir,'_R.dat',sep='')
  conx_path = paste(root_path,'/conxuntos.dat',sep='')
  conxk_path = paste(root_path,'/conxuntos_kfold.dat',sep='')
  if(all(file.exists(dat_path,conx_path,conxk_path))){
    print(root_path)
    print(match(dir,dirs))
    tryCatch({
      getresult(path=dat_path, conxuntos_path=conx_path, conxuntos_k_path=conxk_path)
    }, error = function(e){
      print(e)
    })
  }
}

#----- stop -----#
proc.time() - ptm


## # for test
## dir = 'arrhythmia'
## root_path = paste('data/',dir,sep='')
## path = paste(root_path,'/',dir,'_R.dat',sep='')
## conxuntos_path = paste(root_path,'/conxuntos.dat',sep='')
## conxuntos_k_path_path = paste(root_path,'/conxuntos_kfold.dat',sep='')

## tdata = read.table(path)
## dat <- file(conxuntos_path)
## valid <- strsplit(readLines(dat), " ")
## valid = lapply(valid, as.integer)

##   # find best parameters
##   tdata[,"clase"] <- as.factor(tdata[,"clase"])
##   grid = expand.grid(mtry=c(seq(2,5,by=3)))
##   tc <- trainControl(method="cv",number=1,index=list(Fold1=valid[[1]]),indexOut=valid[[2]],savePredictions=T)
##   rf_model <- train(clase ~ ., data=tdata,method="rf",trControl=tc, tuneGrid=grid)
  

##   # training and testing 
##   conxuntos_dat = file(conxuntos_k_path)
##   conxuntos_kfold = strsplit(readLines(conxuntos_dat)," ")
##   conxuntos_kfold = lapply(conxuntos_kfold, as.integer)
##   best_mtry = rf_model$bestTune[[1]]
  
## i=1
##   for(i in seq(1,7,by=2)){
##     inTraining = matrix(unlist(conxuntos_kfold[i]),ncol=1, byrow=T)
##     inTesting = matrix(unlist(conxuntos_kfold[i+1]),ncol=1, byrow=T)
    
##     training = tdata[inTraining,]
##     testing = tdata[inTesting,]
    
##     trControl = trainControl(method='none',allowParallel=T)
##     rf_pred = train(clase ~ ., data=training, method='rf', ntree=500, trControl=trControl,tuneGrid=data.frame(mtry=best_mtry))
    
##     pred = predict(rf, newdata = testing)
##     print(postResample(pred, testing$clase))
##   }
