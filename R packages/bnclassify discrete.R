# Load the package in R
library(bnclassify)
library(xlsx)
library(dplyr)
library(qgraph)
# library(Rgraphviz)
library(tictoc)
library(yarrr)
library(writexl)
library(pROC)
writeLines("\nloading libraries  -->  done")

# set working directory
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

# calculate run time (start)
tic("Run time  ")

# loading data
data <- read.xlsx(file = "TWWTP_impute_data_discrete.xlsx",
                sheetIndex = 1,
                header = TRUE)
writeLines("\nloading data  -->  done")

# Read column names of data and use it as input and output variables
output_variables <- colnames(data)[1:7]
input_variables <- colnames(data)[8:27]

# Convert all data frame character columns to factors
data = data %>% 
  mutate_if(is.character, as.factor)

# Add the history of input variables to the data
t <- 2   # previous time steps (t) for input variables
n <- 692

# data <- data[(t + 1):(n), ]
# 
# for (var in input_variables) {
#   a <- t
#   b <- n - 1
#   
#   for (i in 1:t) {
#     name_history <- paste(var, "_", i, sep = "")
#     data[name_history] <- assign(name_history, data[a:b , var])
#     a <- a - 1
#     b <- b - 1
#     input_2 = c(name_history)
#   }
# }
# 
# input_variables <- colnames(data)[12:(25 + 14 * t)]


# create result table
result_table <- data.frame(matrix(ncol = length(output_variables), nrow = 4))
colnames(result_table) <- output_variables
row.names(result_table) <- c('train accuracy',
                             'test accuracy',
                             'AUC train',
                             'AUC test')

# run bnclassify for each target in output variables related to effluent
writeLines("
_______________________________________________________
   Learning Bayesian network with discrete variables   
_______________________________________________________  ")

# split data into train and test sets
train_size <- 550
test_size <- n - t - 550

train.split <- data[1:train_size, ]
test.split  <- data[(550 + 1):n - t, ]

writeLines(paste("\ntrain-test: ",
                 round((train_size*100 / (train_size + test_size)),0)
                 ,"-",
                 round((test_size *100 / (train_size + test_size)),0)))

# output_variables <- c("TSS_eff")
for (target_variable in output_variables) {
  
  writeLines(paste("\ntarget variable  --> ",
                   target_variable))
  
  # This is train set to learn the structure and parameters
  train.set <- train.split[, c(input_variables, target_variable)]
  
  # This is test set to predict target variable
  test.set  <- test.split[, c(input_variables, target_variable)]
  
  # Structure learning with nb or tan-cl algorithms
  # network = tan_cl(class = target_variable,
  #                  dataset = train.set,
  #                  score = 'bic')
  network = nb(class = target_variable,
               dataset = train.set)

  # Parameter learning
  network = lp(x = network,
               dataset = train.set,
               smooth = 0.01)
  network
  
  # learn both structure and params
  # network = bnc('tan_cl', target_variable, train.set, smooth = 1)
  
  # plot(network, layoutType = "dot", fontsize = 50)
  # graphviz.plot(network)
  
  # predict test set
  predict.test = predict(network, test.set)
  # cbind(predicted = predict.test, actual = test.set[target_variable]) 
  accuracy.test = accuracy(predict.test, test.set[, target_variable])
  # print(accuracy.test)
  # print(cbind(predicted = predict.test, actual = test.set[target_variable]))
  auc.test = auc(roc(as.numeric(test.set[, target_variable]),
                     as.numeric(predict.test)))

    # predict train set
  predict.train <- predict(network, train.set)
  # cbind(predicted = predict.train, actual = train.set[target_variable]) 
  accuracy.train = accuracy(predict.train, train.set[, target_variable])
  # print(accuracy.train)
  auc.train = auc(roc(as.numeric(train.set[, target_variable]),
                      as.numeric(predict.train)))
  
  
  # writing result in table
  result_indices = (c(accuracy.train = round(accuracy.train, 2),
                      accuracy.test = round(accuracy.test, 2),
                      auc.train = round(auc.train, 2),
                      auc.test = round(auc.test, 2)))
  result_table[1, target_variable] = result_indices['accuracy.train']
  result_table[2, target_variable] = result_indices['accuracy.test']
  result_table[3, target_variable] = result_indices['auc.train']
  result_table[4, target_variable] = result_indices['auc.test']
  
  # modelstring(network)   # bnlearn
  # print(network)
  
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

write.xlsx(x = result_table,
           file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnclassify.xlsx",
           col.names = TRUE,
           row.names = TRUE,
           append = FALSE,
           showNA = TRUE)

writeLines(paste("\naverage train accuracy --> ",
                 rowMeans(result_table)[1]))
writeLines(paste("\naverage test accuracy --> ",
                 rowMeans(result_table)[2], "\n"))

# calculate run time (end)
toc()


















# 
# # Convert all data frame factors columns to character
# test.set[, target_variable] <- as.character(test.set[, target_variable])
# 
# i = 1
# for (row in test.set[, target_variable]) {
#   
#   if (row == 'VL' & !is.na(row)) {
#     test.set[, target_variable][i] = 5}  
#   else if (row == "L" & !is.na(row)) {
#     test.set[, target_variable][i] = 4}
#   else if (row == 'M' & !is.na(row)) {
#     test.set[, target_variable][i] = 3} 
#   else if (row == "H" & !is.na(row)) {
#     test.set[, target_variable][i] = 2} 
#   else if (row == 'VH' & !is.na(row)){
#     test.set[, target_variable][i] = 1}
#   
#   i = i + 1
# }
# 
# # Convert all data frame factors columns to character
# predict.test <- as.character(predict.test)
# # 
# i = 1
# for (row in predict.test) {
#   
#   if (row == 'VL' & !is.na(row)) {
#     predict.test[i] = 5}  
#   else if (row == "L" & !is.na(row)) {
#     predict.test[i] = 4}
#   else if (row == 'M' & !is.na(row)) {
#     predict.test[i] = 3} 
#   else if (row == "H" & !is.na(row)) {
#     predict.test[i] = 2} 
#   else if (row == 'VH' & !is.na(row)){
#     predict.test[i] = 1}
#   
#   i = i + 1
# }
# 
# # plot
# v1 <- test.set[, target_variable]
# v2 <- predict.test
# plot(v1,
#      col = "blue",
#      ann = FALSE,
#      lwd = 3,
#      ylim = c(1,5),
#      ) 
# points(v2, col = "red")
# title(xlab = "Days")
# title(ylab = target_variable)
# legend("bottomright",
#        legend = c("predict", "real"),
#        col = transparent(c('red', 'blue'), .1),
#        pch = c(16, 16),
#        cex = 0.8,
#        bg = "lightblue"
#        )