require('caret')
require('randomForest')
require('klaR')
set.seed(5278)

# for parallel process
library('doParallel')
cl <- makeCluster(3)
registerDoParallel(cl)


getclassifier<- function(name, grid){
  function(path, conxuntos_path, conxuntos_k_path){
    tdata = read.table(path)
    dat <- file(conxuntos_path)
    valid <- strsplit(readLines(dat), " ")
    valid = lapply(valid, as.integer)
    
   # find best parameters
    tdata[,"clase"] <- as.factor(tdata[,"clase"])
   #todo: open it
    
    tc <- trainControl(method="cv",number=1,index=list(Fold1=valid[[1]]),savePredictions=T,allowParallel=T)
    rf_model <- train(clase ~ ., data=tdata,method=name,trControl=tc, tuneGrid=grid)
    ## test_model = rf_model
    
    # training and testing 
    conxuntos_dat = file(conxuntos_k_path)
    conxuntos_kfold = strsplit(readLines(conxuntos_dat)," ")
    conxuntos_kfold = lapply(conxuntos_kfold, as.integer)
    best = data.frame(rf_model$bestTune)
    
    mean_accuracy = 0
    for(i in seq(1,7,by=2)){
      inTraining = matrix(unlist(conxuntos_kfold[i]),ncol=1, byrow=T)
      inTesting = matrix(unlist(conxuntos_kfold[i+1]),ncol=1, byrow=T)
      
      training = tdata[inTraining,]
      testing = tdata[inTesting,]
      
      trControl = trainControl(method='none',allowParallel=T)
      rf = train(clase ~ ., data=training, method=name, ntree=500, trControl=trControl,tuneGrid=best)
      
      pred = predict(rf, newdata = testing)
      accuracy = postResample(pred, testing$clase)['Accuracy']
      mean_accuracy = mean_accuracy + accuracy/4
    }
    rm(tdata, dat, valid, conxuntos_dat, conxuntos_kfold)
    gc()
    return(list(accuracy = accuracy))
  }
}




rf_grid = expand.grid(mtry=c(seq(2,29,by=3)))
rf = list(grid = rf_grid, name = 'rf')

svmPoly_grid = expand.grid(scale = c(0.001, 0.01, 0.1), degree = c(1,2,3), C = c(0.25, 0.5,1))
svmPoly = list(grid = svmPoly_grid, name = 'svmPoly')

svmRadial_grid = expand.grid(C = c(2^(-2:2)), sigma = c(10^(-2:2)))
svmRadial = list(grid = svmRadial_grid, name = 'svmRadial')

parRF_grid = expand.grid(mtry= c(seq(2,8,by=2)))
parRF = list(grid = parRF_grid, name = 'parRF')

#classifier list
cfs = list(rf, svmPoly, svmRadial, parRF)

#----- start -----#
ptm <- proc.time()




# loop all file
dirs = list.dirs('data',full.names=FALSE,recursive=FALSE)

#small data for testing
## dirs = list.dirs('smalldata',full.names=FALSE,recursive=FALSE)

#save all accuracy of classifier(row) to data set(column)
result_accuracy = data.frame(matrix(ncol = length(dirs), nrow = 0))
names(result_accuracy) = dirs

for(cf in cfs){
  for(dir in dirs){
    print(c(cf$name, dir))
    root_path = paste('data/',dir,sep='')
    dat_path = paste(root_path,'/',dir,'_R.dat',sep='')
    conx_path = paste(root_path,'/conxuntos.dat',sep='')
    conxk_path = paste(root_path,'/conxuntos_kfold.dat',sep='')
    if(all(file.exists(dat_path,conx_path,conxk_path))){
      getresult = getclassifier(cf$name, cf$grid)
      tryCatch({
        result_accuracy[cf$name, dir] = getresult(path=dat_path, conxuntos_path=conx_path, conxuntos_k_path=conxk_path)
      }, error = function(e){
        print(e)
      })
    }
    gc()
    ## print(proc.time()-ptm)
  }
}

write.table(result_accuracy, 'result')

#----- stop -----#
used_time = proc.time() - ptm
