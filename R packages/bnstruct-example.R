library(bnstruct)
library(xlsx)
library(bnlearn)
library(tictoc)
library(writexl)
library(hydroGOF)
library(forecast)


writeLines("\nloading libraries  -->  done")

tic("Run time  ")

setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

data <- read.xlsx(file = "TWWTP_raw_data.xlsx",
                  sheetIndex = 1,
                  header = TRUE)

var_input = colnames(data)[12:32]
var_output = colnames(data)[1:11]

# Create result table
result_table <- data.frame(matrix(ncol = length(var_output),
                                  nrow = 6))
colnames(result_table) <- var_output
row.names(result_table) <- c('RMSE _ train',
                             'RMSE _ test',
                             'R2 _ train',
                             'R2 _ test',
                             'NSE _ train',
                             'NSE _ test')

# var_output = 'BOD_eff'
for (target in var_output){
  
  print(target)
  train_data=data[1:550,c(var_input, target)]
  train_data=data.matrix(train_data)
  
  test_data=data[551:690,c(var_input, target)]
  test_data=data.frame(data.matrix(test_data))
  
  if (target=='Q_eff'||target=='BOD_eff'||target=='NH4_eff'||target=='TN_eff'){
    node_size_target=5
  } else {
    node_size_target=6
  }
    
  train_data <- BNDataset(data = train_data,
                          starts.from =1,
                          discreteness = rep('C',22),
                          node.size = c(6,7,7,7,7,
                                        7,7,7,7,7,
                                        7,7,7,7,7,
                                        7,7,7,7,7,7,
                                        node_size_target),
                          variables = c(var_input, target))
  
  bn <- BN(train_data)
  bn <- learn.network(bn,
                      train_data,
                      algo='sem',
                      scoring.func = "BIC")
  # sem - hc - mmpc - mmhc - sm
  
  # write adjacency matrix in txt file
  write.table(bn@dag, 'dag.txt', append = FALSE, sep = ",", dec = ".",
              row.names = TRUE, col.names = TRUE)
  
  #### get the model string to be used in bnlearn
  dag = read.csv('dag.txt')
  modelstring = ""
  i=1
  for (var1 in c(var_input, target)){
    j=1
    modelstring = paste0(modelstring , "[",var1,"|")
    for (var2 in c(var_input, target)){
      if (dag[j,i] == 1){
        modelstring = paste0(modelstring , var2 , ":")}
      if (j<length(c(var_input, target))){
        j=j+1
      }
      else{break}
    }
    modelstring = paste0(substr(modelstring, 1, nchar(modelstring)-1), "]")
    
    if (i<length(c(var_input, target))){
      i=i+1
    }
    else{break}
  }
  
  res = model2network(modelstring)
  plot(res)
  
  #### fit and predict train
  train_data=data[1:550,c(var_input, target)]
  train_data=data.frame(data.matrix(train_data))
  
  for (row in 1:nrow(train_data)){
    for (col in 1:ncol(train_data)){
      train_data[row,col] <- as.double(train_data[row,col])
    }
  }
  
  #### learning of parameters  
  fitted = bn.fit(res,
                  train_data)
  
  #### Predict train
  pred_train = predict(fitted,
                       target,
                       impute(fitted, train_data),
                       method = "bayes-lw")
  
  RMSE.train <- round(accuracy(object = pred_train,
                               x = train_data[, target])[1:1, 2:2]
                      , 2)
  
  R2.train <- (cor(pred_train, train_data[, target],
                   method = "pearson", use = "complete.obs")) ^ 2
  R2.train <- round(R2.train, 2)
  writeLines(paste("\ntarget variable  --> ",
                   target))
  writeLines(paste("\nTrain  --> ",
                   R2.train))
  NSE.train <- round(NSE(sim = pred_train,
                         obs = train_data[, target])
                     , 2)
  
  #### predict test
  for (row in 1:nrow(test_data)){
    for (col in 1:ncol(test_data)){
      test_data[row,col] <- as.double(test_data[row,col])
    }
  }
  
  pred_test = predict(fitted,
                      target,
                      impute(fitted, test_data),
                      method = "bayes-lw")
  
  RMSE.test <- round(accuracy(object = pred_test,
                              x = test_data[, target])[1:1, 2:2]
                     , 2)
  
  R2.test <- (cor(pred_test, test_data[, target],
                  method = "pearson", use = "complete.obs")) ^ 2
  R2.test <- round(R2.test, 2)
  writeLines(paste("\nTest  --> ",
                   R2.test))
  NSE.test <- round(NSE(sim = pred_test,
                        obs = test_data[, target])
                    , 2)
  
  #### writing result in table
  result_table[1, target] <- RMSE.train
  result_table[2, target] <- RMSE.test
  result_table[3, target] <- R2.train
  result_table[4, target] <- R2.test
  result_table[5, target] <- NSE.train
  result_table[6, target] <- NSE.test
  
}

write.xlsx(x = result_table,
           file = "result bnstruct.xlsx",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE,
           showNA = TRUE)

toc()

