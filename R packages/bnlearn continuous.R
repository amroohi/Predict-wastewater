# Load the package in R
library(bnlearn)
library(forecast)
library(xlsx)
library(qgraph)
library(hydroGOF)
library(Rgraphviz)
library(tictoc)
library(writexl)

writeLines("\nloading libraries  -->  done")


# set working directory
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

# calculate run time (start)
tic("Run time  ")

# loading data
data <- read.xlsx(file = "WWTP_data.xlsx",
                  sheetIndex = 1,
                  header = TRUE)
writeLines("\nloading data  -->  done")


# Read column names of data and use it as input and output variables
output_variables <- colnames(data)[1:11]
input_variables <- colnames(data)[12:25]



# Add the history of input variables to the data
t <- 2   # previous time steps (t) for input variables
n <- 692


# 
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
result_table <- data.frame(matrix(ncol = length(output_variables), nrow = 6))
colnames(result_table) <- output_variables
row.names(result_table) <- c('MAPE _ train',
                             'MAPE _ test',
                             'R2 _ train',
                             'R2 _ test',
                             'NSE _ train',
                             'NSE _ test')


# run bnlearn for each target variable in output variables related to effluent

writeLines("
_________________________________________________________
   Learning Bayesian network with continuous variables   
_________________________________________________________  ")




# split data into train and test sets
train_size <- 540
test_size <- n - t - 540

train.split <- data[1:train_size, ]
test.split  <- data[(train_size + 1):n - t, ]

writeLines(paste("\ntrain-test: ", round((train_size*100 / (train_size + test_size)),0)
            ,"-", round((test_size *100 / (train_size + test_size)),0)))




# output_variables <- c("BOD_eff")


for (target_variable in output_variables) {
  
  writeLines(paste("\ntarget variable  --> ", target_variable))
  
  

  # This is train set to learn the structure and parameters
  train.set <- train.split[, c(input_variables, target_variable)]
  
  # This is test set to predict target variable
  test.set  <- test.split[, c(input_variables, target_variable)]
  
  
  
  
  # learn Bayesian network structure on train set
  network <- structural.em(
    x = train.set,
    maximize = "hc",
    fit = "mle",
    debug = FALSE,
    max.iter = 10,
    maximize.args = list(
      restart = 1000,
      max.iter = 20,
      score = "bic-g"
    )
  )
  # print(network)
  
  
  
  # if you want to drop, add or set the direction of an arc or an edge, uncomment below line
  # network <- set.arc(network, from = "TSS1_eff", to = var)
  
  # distance
  
  # estimate the parameters of the Bayesian network
  fitted <- bn.fit(x = network, data = train.set, method = "mle")
  
  # print(fitted)
  # print(fitted$BOD_eff)
  # print(fitted$COD_eff)
  # print(fitted$TSS_eff)
  
  
  # Plotting networks with the Rgraphviz package
  g = graphviz.plot(
    network,
    layout = "dot",
    shape = "circle",
    render = FALSE,
    highlight = list(
      nodes = nodes(network),
      fill = "lightgreen",
      col = "black"
    )
  )
  
  
  nodeRenderInfo(g) <-
    list(
      fontsize = 100,
      arrowsize = 1000,
      distortion = 200,
      width = 100
    )
  edgeRenderInfo(g) <-
    list(
      fontsize = 400,
      arrowsize = 1000,
      distortion = 200,
      width = 100
    )
  graphRenderInfo(g) <-
    list(
      fontsize = 400,
      arrowsize = 1000,
      distortion = 200,
      width = 100
    )
  parRenderInfo(g) <-
    list(
      edges = list(lwd = 200, lty = "dashed"),
      nodes = list(col = "gray", fill = "gray")
    )
  
  renderGraph(g, drawNodes="renderNodes")
  
  
  
  
  # plot other graph
  bn.fit.qqplot(fitted$BOD_in)
  bn.fit.xyplot(fitted$BOD_in)
  bn.fit.histogram(fitted$BOD_in)
  plot(network)
  # qgraph(network, layout = "circular", vsize = 10, asize = 4, edge.color = "black")
  
  
  
  ###########################
  ########  predict  ########  
  ###########################
  
  
  
  
  # predicts target variable given train and test set
  predict.test <- predict(
    object = fitted,
    node = target_variable,
    data = impute(fitted, test.set),
    method = "bayes-lw"
  )
  
  predict.train <- predict(
    object = fitted,
    node = target_variable,
    data = impute(fitted, train.set),
    method = "bayes-lw"
  )
  
  
  
  # show the actual and predicted in test set
  cbind(predicted = predict.test, actual = test.set[, target_variable])
  
  
  # compare the actual and predicted in test set using three indices
  # print(target_variable)
  # print('<<<<<< test >>>>>>')
  
  MAPE.test <- accuracy(object = predict.test, x = test.set[, target_variable])[1:1, 5:5]
  MAPE.test <- round(MAPE.test, 2)
  
  # git
  
  R2.test <- (cor(predict.test, test.set[, target_variable],
                  method = "pearson", use = "complete.obs")) ^ 2 * 100
  R2.test <- round(R2.test, 2)
  
  
  
  NSE.test <- NSE(sim = predict.test, obs = test.set[, target_variable])
  NSE.test <- round(NSE.test, 2)
  
  
  
  # compare the actual and predicted in train set using three indices
  # print("")
  # print('<<<<<< train >>>>>>')
  
  MAPE.train <- accuracy(object = predict.train, x = train.set[, target_variable])[1:1, 5:5]
  MAPE.train <- round(MAPE.train, 2)
  
  
  
  R2.train <- (cor(predict.train, train.set[, target_variable],  
                   method = "pearson", use = "complete.obs")) ^ 2 * 100
  R2.train <- round(R2.train, 2)
  
  
  
  NSE.train <- NSE(sim = predict.train, obs = train.set[, target_variable])
  NSE.train <- round(NSE.train, 2)
  
  
  
  # writing result in table
  result_table[1, target_variable] = MAPE.train
  result_table[2, target_variable] = MAPE.test
  result_table[3, target_variable] = R2.train
  result_table[4, target_variable] = R2.test
  result_table[5, target_variable] = NSE.train
  result_table[6, target_variable] = NSE.test
  
  
  
  # Computing a network score
  score(network,
        impute(fitted, train.set),
        type = "bic-g",
        by.node = TRUE)
  
  
  writeLines("\ndone\n")
  
}




# plot
v1 <- test.set[, target_variable]
v2 <- predict.test

plot(
  v1,
  type = "l",
  col = "blue",
  ylim = range(min(min(v1, na.rm = TRUE), min(v2)), max(max(v1, na.rm = TRUE), max(v2))),
  ann = FALSE,
  lwd = 3
)

lines(v2, type = "l", col = "red", lwd = 3)

title(xlab = "Days")
title(ylab = target_variable)

legend(
  "topright",
  c("real", "predict"),
  cex = 0.8,
  col = c("blue", "red"),
  lty = 1:1,
  lwd = 3,
  bty = "n",
  bg='lightblue',
  box.lty=1
)


box()




write_xlsx(result_table, "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnlearn.xlsx")

# calculate run time (end)
toc()
