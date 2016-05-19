require('caret')
require('randomForest')
require('klaR')
set.seed(5278)

library('kernlab')
# for parallel process
library('doParallel')
require('psych')

cores = 4 

getclassifier<- function(name, grid){
  function(path, conxuntos_path, conxuntos_k_path){
    cl = makeCluster(cores)
    registerDoParallel(cl)
    tdata = read.table(path)
    dat <- file(conxuntos_path)
    valid <- strsplit(readLines(dat), " ")
    valid = lapply(valid, as.integer)
    
   # find best parameters
    tdata[,"clase"] <- as.factor(tdata[,"clase"])
   #todo: open it
    
    print('tuning')
    tc <- trainControl(method="cv",number=1,index=list(Fold1=valid[[1]]),savePredictions=T,allowParallel=T)
    rf_model <- train(clase ~ ., data=tdata,method=name,trControl=tc, tuneGrid=grid)
    
    stopCluster(cl=cl)

    # training and testing 
    conxuntos_dat = file(conxuntos_k_path)
    conxuntos_kfold = strsplit(readLines(conxuntos_dat)," ")
    conxuntos_kfold = lapply(conxuntos_kfold, as.integer)
    best = data.frame(rf_model$bestTune)

    #testing get accuracy
    cl = makeCluster(cores)
    registerDoParallel(cl)
    print('get accuracy')

    mean_accuracy = 0
    all_test = c()
    all_pred = c()
    for(i in seq(1,7,by=2)){inTraining = matrix(unlist(conxuntos_kfold[i]),ncol=1, byrow=T)
      inTesting = matrix(unlist(conxuntos_kfold[i+1]),ncol=1, byrow=T)
      
      training = tdata[inTraining,]
      testing = tdata[inTesting,]
      
      trControl = trainControl(method='none',allowParallel=T)
      rf = train(clase ~ ., data=training, method=name, ntree=500, trControl=trControl,tuneGrid=best)
      
      pred = predict(rf, newdata = testing)
      all_test = c(all_test, testing$clase)
      all_pred = c(all_pred, pred)
      accuracy = postResample(pred, testing$clase)['Accuracy']
      mean_accuracy = mean_accuracy + accuracy/4
    }
    kappa =  cohen.kappa(x = cbind(all_test, all_pred))$kappa
    stopCluster(cl=cl)
    print(' ')
    gc()
    return(list(accuracy = mean_accuracy, kappa = kappa))
    }
}





#-------------------------------- classifier --------------------------#

# done

# ongoing
rf_grid = expand.grid(mtry=c(seq(2,29,by=3)))
rf = list(grid = rf_grid, name = 'rf')

svmPoly_grid = expand.grid(scale = c(0.001, 0.01, 0.1), degree = c(1,2,3), c = c(0.25, 0.5,1))
svmPoly = list(grid = svmPoly_grid, name = 'svmPoly')

svmRadial_grid = expand.grid(c = c(2^(-2:2)), sigma = c(10^(-2:2)))
svmRadial = list(grid = svmRadial_grid, name = 'svmRadial')

parRF_grid = expand.grid(mtry= c(seq(2,8,by=2)))
parRF = list(grid = parRF_grid, name = 'parRF')

c50_grid = expand.grid(winnow = c(TRUE,FALSE), trials=c(1,10,20), model=c("rules","tree") )
c50 = list(grid = c50_grid, name = "c5.0")

## cfs = list(rf, svmPoly, svmRadial, parRF, c50)
#6
#todo
nnet_grid = expand.grid(size = c(seq(1,9,by=2)), decay = c(0,0.1,0.01,0.001,0.0001))
nnet = list(grid = nnet_grid, name = "nnet")

svmRadialCost_grid = expand.grid(c = c(2^(-2:2)))
svmRadialCost = list(grid = svmRadialCost_grid, name = "svmRadialCost")

avNNet_grid = expand.grid(size = c(1,3,5), decay = c(0, 0.1, 10^-4), bag = FALSE)
avNNet = list(grid = avNNet_grid, name = "avNNet")

pcaNNet_grid = expand.grid(size = c(seq(1,9,by=2)), decay = c(0,0.1,0.01,0.001,0.0001))
pcaNNet = list(grid = pcaNNet_grid, name = "pcaNNet")

mlp_grid = expand.grid(size = c(seq(1,19,by=2)))
mlp = list(grid=mlp_grid, name = "mlp")

RRF_grid = expand.grid(mtry = 2, coefreg=c(0.01,0.5,1), coefimp = c(0, 0.5, 1))
RRF = list(grid=RRF_grid, name = "RRF")

RRFglobal_grid = expand.grid(mtry = 2, coefreg=c(seq(0.01,1,by=0.12)))
RRFglobal = list(grid=RRFglobal_grid, name = "RRFglobal")


#new todo

## stepLDA_grid = expand.grid(s = 1)
## stepLDA = list(grid = stepLDA

## cfs = list(stepLDA)

# loop all file
dirs = list.dirs('data',full.names=FALSE,recursive=FALSE)

#small data for testing
#dirs = list.dirs('smalldata',full.names=FALSE,recursive=FALSE)

#save all accuracy of classifier(row) to data set(column)
result_accuracy = data.frame(matrix(ncol = length(dirs), nrow = 0))
result_kappa = data.frame(matrix(ncol = length(dirs), nrow = 0))
names(result_accuracy) = dirs
names(result_kappa) = dirs

for(cf in cfs){
  for(dir in dirs){
    print(c(cf$name, dir))
    root_path = paste('data/',dir,sep='')
    path = paste(root_path,'/',dir,'_R.dat',sep='')
    conxuntos_path = paste(root_path,'/conxuntos.dat',sep='')
    conxuntos_k_path = paste(root_path,'/conxuntos_kfold.dat',sep='')
    if(all(file.exists(path,conxuntos_path,conxuntos_k_path))){
      getresult = getclassifier(cf$name, cf$grid)
      tryCatch({
        r = getresult(path=path, conxuntos_path=conxuntos_path, conxuntos_k_path=conxuntos_k_path)
        accuracy = r$accuracy
        result_accuracy[cf$name, dir] =  accuracy

        kappa = r$kappa
        result_kappa[cf$name, dir] = kappa

        rm(getresult)
        gc()
      }, error = function(e){
        print(e)
      })
    }
    gc()
  }
  write.table(result_accuracy, 'allresult/15accuracy')
  write.table(result_kappa, 'allresult/15kappa')

}

## cf_names =  c()
## for(cf in cfs){
##   cf_names = c(cf_names, c(cf$name))
## }


## accuracies = read.table('result/top12_result')
## average_accuracy = data.frame(matrix(ncol = 0 ,nrow = 1))
## cf_names = row.names(accuracies)
## names(average_accuracy) = cf_names
## for(cf in 1:nrow(accuracies)){
##   total = 0
##   na_count = 0
##   for(dat in 1:ncol(accuracies)){
##     accu = accuracies[cf,dat]
##     if(is.na(accu)){
##       na_count = na_count + 1
##     }else{
##       total = total + accu
##     }
##   }
##   average_accuracy[1,cf_names[cf]] = total/(ncol(accuracies)-na_count)
## }
## print(average_accuracy)
## write.table(average_accuracy,"result/top12_average_accuracy")

