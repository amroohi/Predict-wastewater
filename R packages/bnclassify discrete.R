# Load the package in R
library(bnclassify)
library(xlsx)
library(dplyr)
# library(qgraph)
# library(Rgraphviz)
# library(tidyverse)
library(tictoc)
library(yarrr)
library(writexl)

writeLines("\nloading libraries  -->  done")


# set working directory
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")


# calculate run time (start)
tic("Run time  ")


# loading data
df <- read.xlsx(file = "WWTP_data.xlsx",
                sheetIndex = 1,
                header = TRUE)
writeLines("\nloading data  -->  done")


# Read column names of data and use it as input and output variables
output_variables <- colnames(df)[1:11]
input_variables <- colnames(df)[12:25]



##############  discrete data  ############## 
writeLines("
____________________
   discrete data
____________________  ")

           
i = 1
for (row in df$Q_in) {
  
  if (row < 280000 & !is.na(row)) {
    df$Q_in[i] = 'VL'}  
  else if (row >= 280000 & row < 330000 & !is.na(row)) {
    df$Q_in[i] = 'L'}  
  else if (row >= 330000 & row < 380000 & !is.na(row)) {
    df$Q_in[i] = 'M'}  
  else if (row >= 380000 & row < 430000 & !is.na(row)) {
    df$Q_in[i] = 'H'}  
  else if (row >= 430000 & !is.na(row)){
    df$Q_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$Temp_in) {
  
  if (row < 17 & !is.na(row)) {
    df$Temp_in[i] = 'VL'}  
  else if (row >= 17 & row < 20 & !is.na(row)) {
    df$Temp_in[i] = 'L'}
  else if (row >= 20 & row < 23 & !is.na(row)) {
    df$Temp_in[i] = 'M'} 
  else if (row >= 23 & row < 26 & !is.na(row)) {
    df$Temp_in[i] = 'H'} 
  else if (row >= 26 & !is.na(row)){
    df$Temp_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$pH_in) {
  
  if (row < 6 & !is.na(row)) {
    df$pH_in[i] = 'VL'}  
  else if (row >= 6 & row < 6.7 & !is.na(row)) {
    df$pH_in[i] = 'L'} 
  else if (row >= 6.7 & row < 7.3 & !is.na(row)) {
    df$pH_in[i] = 'M'} 
  else if (row >= 7.3 & row < 7.9 & !is.na(row)) {
    df$pH_in[i] = 'H'} 
  else if (row >= 7.9 & !is.na(row)){
    df$pH_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$COD_in) {
  
  if (row < 300 & !is.na(row)) {
    df$COD_in[i] = 'VL'}  
  else if (row >= 300 & row < 400 & !is.na(row)) {
    df$COD_in[i] = 'L'} 
  else if (row >= 400 & row < 500 & !is.na(row)) {
    df$COD_in[i] = 'M'}
  else if (row >= 500 & row < 700 & !is.na(row)) {
    df$COD_in[i] = 'H'}
  else if (row >= 700 & !is.na(row)){
    df$COD_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$BOD_in) {
  
  if (row < 200 & !is.na(row)) {
    df$BOD_in[i] = 'VL'}  
  else if (row >= 200 & row < 260 & !is.na(row)) {
    df$BOD_in[i] = 'L'}  
  else if (row >= 260 & row < 330 & !is.na(row)) {
    df$BOD_in[i] = 'M'}  
  else if (row >= 330 & row < 370 & !is.na(row)) {
    df$BOD_in[i] = 'H'}  
  else if (row >= 370 & !is.na(row)){
    df$BOD_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TSS_in) {
  
  if (row < 100 & !is.na(row)) {
    df$TSS_in[i] = 'VL'}  
  else if (row >= 100 & row < 180 & !is.na(row)) {
    df$TSS_in[i] = 'L'}  
  else if (row >= 180 & row < 250 & !is.na(row)) {
    df$TSS_in[i] = 'M'} 
  else if (row >= 250 & row < 400 & !is.na(row)) {
    df$TSS_in[i] = 'H'} 
  else if (row >= 400 & !is.na(row)){
    df$TSS_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$NH4_in) {
  
  if (row < 30 & !is.na(row)) {
    df$NH4_in[i] = 'VL'}  
  else if (row >= 30 & row < 40 & !is.na(row)) {
    df$NH4_in[i] = 'L'}  
  else if (row >= 40 & row < 55 & !is.na(row)) {
    df$NH4_in[i] = 'M'}  
  else if (row >= 55 & row < 70 & !is.na(row)) {
    df$NH4_in[i] = 'H'}  
  else if (row >= 70 & !is.na(row)){
    df$NH4_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TN_in) {
  
  if (row < 40 & !is.na(row)) {
    df$TN_in[i] = 'VL'}  
  else if (row >= 40 & row < 50 & !is.na(row)) {
    df$TN_in[i] = 'L'} 
  else if (row >= 50 & row < 65 & !is.na(row)) {
    df$TN_in[i] = 'M'}  
  else if (row >= 65 & row < 80 & !is.na(row)) {
    df$TN_in[i] = 'H'}  
  else if (row >= 80 & !is.na(row)){
    df$TN_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$PO4_in) {
  
  if (row < 3 & !is.na(row)) {
    df$PO4_in[i] = 'VL'}  
  else if (row >= 3 & row < 4 & !is.na(row)) {
    df$PO4_in[i] = 'L'}  
  else if (row >= 4 & row < 5 & !is.na(row)) {
    df$PO4_in[i] = 'M'}  
  else if (row >= 5 & row < 7 & !is.na(row)) {
    df$PO4_in[i] = 'H'}  
  else if (row >= 7 & !is.na(row)){
    df$PO4_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TP_in) {
  
  if (row < 5.5 & !is.na(row)) {
    df$TP_in[i] = 'VL'}  
  else if (row >= 5.5 & row < 6.5 & !is.na(row)) {
    df$TP_in[i] = 'L'} 
  else if (row >= 6.5 & row < 7.5 & !is.na(row)) {
    df$TP_in[i] = 'M'} 
  else if (row >= 7.5 & row < 10 & !is.na(row)) {
    df$TP_in[i] = 'H'} 
  else if (row >= 10 & !is.na(row)){
    df$TP_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$MLSS_AT) {
  
  if (row < 2600 & !is.na(row)) {
    df$MLSS_AT[i] = 'VL'}  
  else if (row >= 2600 & row < 3300 & !is.na(row)) {
    df$MLSS_AT[i] = 'L'}  
  else if (row >= 3300 & row < 3800 & !is.na(row)) {
    df$MLSS_AT[i] = 'M'}
  else if (row >= 3800 & row < 4500 & !is.na(row)) {
    df$MLSS_AT[i] = 'H'}
  else if (row >= 4500 & !is.na(row)){
    df$MLSS_AT[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$DO_AT) {
  
  if (row < 0.7 & !is.na(row)) {
    df$DO_AT[i] = 'VL'}  
  else if (row >= 0.7 & row < 0.9 & !is.na(row)) {
    df$DO_AT[i] = 'L'}  
  else if (row >= 0.9 & row < 1.2 & !is.na(row)) {
    df$DO_AT[i] = 'M'}  
  else if (row >= 1.2 & row < 2 & !is.na(row)) {
    df$DO_AT[i] = 'H'}  
  else if (row >= 2 & !is.na(row)){
    df$DO_AT[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$MLSS_re) {
  
  if (row < 4500 & !is.na(row)) {
    df$MLSS_re[i] = 'VL'}  
  else if (row >= 4500 & row < 5600 & !is.na(row)) {
    df$MLSS_re[i] = 'L'}
  else if (row >= 5600 & row < 6400 & !is.na(row)) {
    df$MLSS_re[i] = 'M'} 
  else if (row >= 6400 & row < 7000 & !is.na(row)) {
    df$MLSS_re[i] = 'H'} 
  else if (row >= 7000 & !is.na(row)){
    df$MLSS_re[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$Temp_air) {
  
  if (row < 0 & !is.na(row)) {
    df$Temp_air[i] = 'VL'}  
  else if (row >= 0 & row < 10 & !is.na(row)) {
    df$Temp_air[i] = 'L'}  
  else if (row >= 10 & row < 20 & !is.na(row)) {
    df$Temp_air[i] = 'M'}  
  else if (row >= 20 & row < 28 & !is.na(row)) {
    df$Temp_air[i] = 'H'}  
  else if (row >= 28 & !is.na(row)){
    df$Temp_air[i] = 'VH'}
  
  i = i + 1
}



i = 1
for (row in df$Q_eff) {
  
  if (row < 280000 & !is.na(row)) {
    df$Q_eff[i] = 'VL'}  
  else if (row >= 280000 & row < 330000 & !is.na(row)) {
    df$Q_eff[i] = 'L'}  
  else if (row >= 330000 & row < 380000 & !is.na(row)) {
    df$Q_eff[i] = 'M'}  
  else if (row >= 380000 & row < 430000 & !is.na(row)) {
    df$Q_eff[i] = 'H'}  
  else if (row >= 430000 & !is.na(row)){
    df$Q_eff[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$Temp_eff) {
  
  if (row < 17 & !is.na(row)) {
    df$Temp_eff[i] = 'VL'}  
  else if (row >= 17 & row < 20 & !is.na(row)) {
    df$Temp_eff[i] = 'L'}
  else if (row >= 20 & row < 23 & !is.na(row)) {
    df$Temp_eff[i] = 'M'} 
  else if (row >= 23 & row < 26 & !is.na(row)) {
    df$Temp_eff[i] = 'H'} 
  else if (row >= 26 & !is.na(row)){
    df$Temp_eff[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$pH_eff) {
  
  if (row < 6.5 & !is.na(row)) {
    df$pH_eff[i] = 'VL'}  
  else if (row >= 6.5 & row < 6.7 & !is.na(row)) {
    df$pH_eff[i] = 'L'} 
  else if (row >= 6.7 & row < 6.9 & !is.na(row)) {
    df$pH_eff[i] = 'M'} 
  else if (row >= 6.9 & row < 7.1 & !is.na(row)) {
    df$pH_eff[i] = 'H'} 
  else if (row >= 7.1 & !is.na(row)){
    df$pH_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$COD_eff) {
  
  if (row < 10 & !is.na(row)) {
    df$COD_eff[i] = 'VL'}  
  else if (row >= 10 & row < 20 & !is.na(row)) {
    df$COD_eff[i] = 'L'}  
  else if (row >= 20 & row < 40 & !is.na(row)) {
    df$COD_eff[i] = 'M'} 
  else if (row >= 40 & row < 70 & !is.na(row)) {
    df$COD_eff[i] = 'H'} 
  else if (row >= 70 & !is.na(row)){
    df$COD_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$BOD_eff) {
  
  if (row < 5 & !is.na(row)) {
    df$BOD_eff[i] = 'VL'}  
  else if (row >= 5 & row < 15 & !is.na(row)) {
    df$BOD_eff[i] = 'L'} 
  else if (row >= 15 & row < 30 & !is.na(row)) {
    df$BOD_eff[i] = 'M'} 
  else if (row >= 30 & row < 50 & !is.na(row)) {
    df$BOD_eff[i] = 'H'} 
  else if (row >= 50 & !is.na(row)){
    df$BOD_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TSS_eff) {
  
  if (row < 10 & !is.na(row)) {
    df$TSS_eff[i] = 'VL'}  
  else if (row >= 10 & row < 40 & !is.na(row)) {
    df$TSS_eff[i] = 'L'}  
  else if (row >= 40 & row < 50 & !is.na(row)) {
    df$TSS_eff[i] = 'M'} 
  else if (row >= 50 & row < 100 & !is.na(row)) {
    df$TSS_eff[i] = 'H'} 
  else if (row >= 100 & !is.na(row)){
    df$TSS_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$NH4_eff) {
  
  if (row < 1 & !is.na(row)) {
    df$NH4_eff[i] = 'VL'}  
  else if (row >= 1 & row < 2.5 & !is.na(row)) {
    df$NH4_eff[i] = 'L'}  
  else if (row >= 2.5 & row < 5 & !is.na(row)) {
    df$NH4_eff[i] = 'M'} 
  else if (row >= 5 & row < 15 & !is.na(row)) {
    df$NH4_eff[i] = 'H'} 
  else if (row >= 15 & !is.na(row)){
    df$NH4_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TN_eff) {
  
  if (row < 10 & !is.na(row)) {
    df$TN_eff[i] = 'VL'}  
  else if (row >= 10 & row < 18 & !is.na(row)) {
    df$TN_eff[i] = 'L'}  
  else if (row >= 18 & row < 25 & !is.na(row)) {
    df$TN_eff[i] = 'M'}  
  else if (row >= 25 & row < 32 & !is.na(row)) {
    df$TN_eff[i] = 'H'}  
  else if (row >= 32 & !is.na(row)){
    df$TN_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$PO4_eff) {
  
  if (row < 3 & !is.na(row)) {
    df$PO4_eff[i] = 'VL'}  
  else if (row >= 3 & row < 4 & !is.na(row)) {
    df$PO4_eff[i] = 'L'}  
  else if (row >= 4 & row < 5 & !is.na(row)) {
    df$PO4_eff[i] = 'M'} 
  else if (row >= 5 & row < 7 & !is.na(row)) {
    df$PO4_eff[i] = 'H'} 
  else if (row >= 7 & !is.na(row)){
    df$PO4_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TP_eff) {
  
  if (row < 3 & !is.na(row)) {
    df$TP_eff[i] = 'VL'}  
  else if (row >= 3 & row < 4.5 & !is.na(row)) {
    df$TP_eff[i] = 'L'}  
  else if (row >= 4.5 & row < 5.5 & !is.na(row)) {
    df$TP_eff[i] = 'M'}  
  else if (row >= 5.5 & row < 7.5 & !is.na(row)) {
    df$TP_eff[i] = 'H'}  
  else if (row >= 7.5 & !is.na(row)){
    df$TP_eff[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$FC_eff) {
  
  if (row < 10 & !is.na(row)) {
    df$FC_eff[i] = 'VL'}  
  else if (row >= 10 & row < 130 & !is.na(row)) {
    df$FC_eff[i] = 'L'}  
  else if (row >= 130 & row < 250 & !is.na(row)) {
    df$FC_eff[i] = 'M'}  
  else if (row >= 250 & row < 500 & !is.na(row)) {
    df$FC_eff[i] = 'H'}  
  else if (row >= 500 & !is.na(row)){
    df$FC_eff[i] = 'VH'}
  
  i = i + 1
}




# Convert all data frame character columns to factors
df = df %>% 
    mutate_if(is.character, as.factor)

  
# another method for discretize
# data_discrete = discretize(data, method = "interval", breaks = 4)




# Add the history of input variables to the data
t <- 10   # previous time steps (t) for input variables
n <- 692



# df <- df[(t + 1):(n), ]
# 
# for (var in input_variables) {
#   a <- t
#   b <- n - 1
#   
#   for (i in 1:t) {
#     name_history <- paste(var, "_", i, sep = "")
#     df[name_history] <- assign(name_history, df[a:b , var])
#     a <- a - 1
#     b <- b - 1
#     input_2 = c(name_history)
#   }
# }
# 
# input_variables <- colnames(df)[12:(25 + 14 * t)]




# create result table
result_table <- data.frame(matrix(ncol = length(output_variables), nrow = 2))
colnames(result_table) <- output_variables
row.names(result_table) <- c('accuracy _ train',
                             'accuracy _ test')





# run bnclassify for each target variable in output variables related to effluent

writeLines("
_______________________________________________________
   Learning Bayesian network with discrete variables   
_______________________________________________________  ")






# split data into train and test sets
train_size <- 540
test_size <- n - t - 540

train.split <- df[1:train_size, ]
test.split  <- df[(train_size + 1):n - t, ]

writeLines(paste("\ntrain-test: ", round((train_size*100 / (train_size + test_size)),0)
                 ,"-", round((test_size *100 / (train_size + test_size)),0)))




# output_variables <- c("BOD_eff")

for (target_variable in output_variables) {
  
  
  
  # This is train set to learn the structure and parameters
  train.set <- train.split[, c(input_variables, target_variable)]
  
  # This is test set to predict target variable
  test.set  <- test.split[, c(input_variables, target_variable)]
  
  
  
  
  # ok: tan_cl, nb
  
  # network = tan_cl(target_variable, train.set, score = 'bic')
  network = nb(target_variable, train.set)
  # network = bnc('tan_cl', target_variable, train.set, smooth = 1)    # learn both structure and params
  
  
  # not ok:
  # network = fssj(target_variable, train.set, k = 10)
  # network = bsej(target_variable, train.set, k = 5)
  # network = tan_hc(target_variable, train.set, k = 5)
  # network = tan_hcsp(target_variable, train.set, score = 'aic')
  # network = aode(target_variable, train.set)
  
  
  
  network = lp(network, train.set, smooth = 0.01, awnb_trees = 1)
  
  network
  
  
  
  # predict test set
  
  predict.test = predict(network, test.set)
  
  cbind(predicted = predict.test, actual = test.set[target_variable]) 
  
  accuracy.test = accuracy(predict.test, test.set[, target_variable])
  # print(accuracy.test)
  
  
  
  
  
  # predict train set
  
  predict.train <- predict(network, train.set)
  
  cbind(predicted = predict.train, actual = train.set[target_variable]) 
  
  accuracy.train = accuracy(predict.train, train.set[, target_variable])
  # print(accuracy.train)
  
  
  
  i = 1
  sum = 0
  nan = 0
  
  for (row in predict.test) {
    if (!is.na(test.set[i,target_variable])) {
      if (row == test.set[i,target_variable]){
        sum = sum + 1}
      if (is.na(test.set[i,target_variable])) {
        nan = nan + 1}
    }
    i = i + 1
  }
  
  print("accuracy test: ")
  print(sum / (length(predict.test)-nan))
  
  
  
  
  
  
  
  i = 1
  sum = 0
  nan = 0
  
  for (row in predict.train) {
    if (!is.na(train.set[i,target_variable])) {
      if (row == train.set[i,target_variable]){
        sum = sum + 1}
      if (is.na(train.set[i,target_variable])) {
        nan = nan + 1}
    }
    i = i + 1
  }
  
  print("accuracy train: ")
  print(sum / (length(predict.train)-nan))
  
  
  
  
  
  
  # writing result in table
  result_table[1, target_variable] = round(accuracy.train, 2)*100
  result_table[2, target_variable] = round(accuracy.test, 2)*100
  
  
  
  
  
  
  
  # modelstring(network)   # bnlearn
  
  # print(network)
  
  
  plot(network, fontsize = 40)
  # graphviz.plot(network)
  
  
  params(network)
  # nparams(tan)
  AIC(network, test.set)
  BIC(network, test.set)
  
  
  
  
  # Filter the data we need
  tree_1 <- filter(Orange, Tree == 1)
  # Graph the data
  # ggplot(tree_1) +
  #   geom_line(aes(x = age, y = circumference))
  
  
  
}




# Convert all data frame factors columns to character
test.set[, target_variable] <- as.character(test.set[, target_variable])


i = 1
for (row in test.set[, target_variable]) {
  
  if (row == 'VL' & !is.na(row)) {
    test.set[, target_variable][i] = 5}  
  else if (row == "L" & !is.na(row)) {
    test.set[, target_variable][i] = 4}
  else if (row == 'M' & !is.na(row)) {
    test.set[, target_variable][i] = 3} 
  else if (row == "H" & !is.na(row)) {
    test.set[, target_variable][i] = 2} 
  else if (row == 'VH' & !is.na(row)){
    test.set[, target_variable][i] = 1}
  
  i = i + 1
}






typeof(predict.test)

# Convert all data frame factors columns to character
predict.test <- as.character(predict.test)


i = 1
for (row in predict.test) {
  
  if (row == 'VL' & !is.na(row)) {
    predict.test[i] = 5}  
  else if (row == "L" & !is.na(row)) {
    predict.test[i] = 4}
  else if (row == 'M' & !is.na(row)) {
    predict.test[i] = 3} 
  else if (row == "H" & !is.na(row)) {
    predict.test[i] = 2} 
  else if (row == 'VH' & !is.na(row)){
    predict.test[i] = 1}
  
  i = i + 1
}









# plot
v1 <- test.set[, target_variable]
v2 <- predict.test

plot(
  v1,
  col = "blue",
  ann = FALSE,
  lwd = 3,
  ylim = c(1,5),
) 

points(v2, col = "red")

title(xlab = "Days")
title(ylab = target_variable)



legend("bottomright",
       legend = c("Females", "Males"),
       col = transparent(c('red', 'blue'), .1),
       pch = c(16, 16),
       cex = 0.8,
       bg = "lightblue",
)




write_xlsx(result_table, "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnclassify.xlsx")



# calculate run time (end)
toc()