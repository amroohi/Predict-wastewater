# Load the package in R
library(bnlearn)
# library(bnviewer)
# library(visNetwork)
# library(igraph)
# library(networkD3)
library(forecast)
library(xlsx)
library(qgraph)
library(hydroGOF)
library(Rgraphviz)
library(tictoc)
library(writexl)
writeLines("\nloading libraries  -->  done")

####################################
#####  Define three functions  #####
####################################

# 1. train_test_split:
#    This function adjusts train and test dataset to a specific size.
train_test_split <- function(data,
                             size,
                             test_size,
                             length_data,
                             previous_time_steps) {
  
  # Split data into train and test sets
  train.split <- data[1:size, ]
  test.split <- data[(length_data - test_size + 1):
                       (length_data - previous_time_steps), ]
  
  writeLines(paste("\ntrain - test: ",
                   round((size*100 / (size + test_size)),0),
                   "% -",
                   round((test_size*100 / (size + test_size)),0),
                   "%"))
  writeLines(paste("\ntrain size: ", size))
  
  return (list(train.split,
               test.split))
}


# 2. random_missing_data:
#    This function generates a random missing data in dataset.
random_missing_data <- function(size,
                                train.split,
                                missing) {
  
  # portion_missing_data <- portion_missing_data[-1]
  writeLines(paste("\nstart function to apply  --> ",
                   missing * 100,
                   "% portion of missing data"))
  
  for (variable in colnames(train.split)) {
    initial_missing_data <- colSums(is.na(train.split))[variable] / size
    
    if ((missing - initial_missing_data) > 0) {
      
      i <- 1
      while (i < ((missing - initial_missing_data) * size)){
        random_number <- sample(1:(size + 1),
                                1,
                                replace = FALSE)
        if (!is.na(train.split[random_number, variable])) {
          train.split[random_number, variable] <- NA
          i <- i + 1
        }
      }
    }
  }
  return (train.split)
}


# 3. bnlearn:
#    Learning Bayesian network for continuous variables using bnlearn package.
bnlearn <- function(target_variable,
                    input_variables,
                    train.split,
                    test.split) {
  
  writeLines(paste("\ntarget variable  --> ",
                   target_variable))
  
  # This is train set to learn the structure and parameters
  train.set <- train.split[, c(input_variables, target_variable)]
  
  # This is test set to predict target variable
  test.set  <- test.split[, c(input_variables, target_variable)]
  
  for (run in 1:1) {
    
    # Learn Bayesian network structure on train set
    network <- structural.em(x = train.set,
                             maximize = "hc",
                             fit = "mle",
                             debug = FALSE,
                             max.iter = 10,
                             maximize.args = list(restart = 1000,
                                                  max.iter = 20,
                                                  score = "bic-g"))
    # print(network)
    
    # For drop, add or set the direction of an arc or an edge, use below line
    # network <- set.arc(network, from = "TSS1_eff", to = var)
    
    # Estimate the parameters of the Bayesian network
    fitted <- bn.fit(x = network,
                     data = train.set,
                     method = "mle")
    print(fitted[target_variable])
    
    # Plotting networks with the Rgraphviz package
    graphviz_plot = graphviz.plot(network,
                                  layout = "dot",
                                  shape = "circle",
                                  render = FALSE,
                                  highlight = list(nodes = nodes(network),
                                                   fill = "lightgreen",
                                                   col = "black"))
    
    nodeRenderInfo(graphviz_plot) <- list(fontsize = 100,
                                          arrowsize = 1000,
                                          distortion = 200,
                                          width = 100)
    edgeRenderInfo(graphviz_plot) <- list(fontsize = 400,
                                          arrowsize = 1000,
                                          distortion = 200,
                                          width = 100)
    graphRenderInfo(graphviz_plot) <- list(fontsize = 400,
                                           arrowsize = 1000,
                                           distortion = 200,
                                           width = 100)
    parRenderInfo(graphviz_plot) <- list(edges = list(lwd = 200,
                                                      lty = "dashed"),
                                         nodes = list(col = "gray",
                                                      fill = "gray"))
    renderGraph(graphviz_plot,
                drawNodes="renderNodes")
    
    # Plot other graph
    # bn.fit.qqplot(fitted$BOD_in)
    # bn.fit.xyplot(fitted$BOD_in)
    # bn.fit.histogram(fitted$BOD_in)
    plot(network)
    # graphviz.plot(network)
    # qgraph(network,
    #        layout = "circular",
    #        vsize = 10,
    #        asize = 4,
    #        edge.color = "black")
    
    ###########################
    ########  predict  ########  
    ###########################
    
    # Predicts target variable given train and test set
    predict.test <- predict(object = fitted,
                            node = target_variable,
                            data = impute(fitted,
                                          test.set),
                            method = "bayes-lw")
    
    predict.test <- as.data.frame(predict.test)
    if (run==1){
      dataframe.predict.test <- predict.test
    } else {
      dataframe.predict.test <- cbind(dataframe.predict.test, predict.test)
    }
    
    predict.train <- predict(object = fitted,
                             node = target_variable,
                             data = impute(fitted,
                                           train.set),
                             method = "bayes-lw")
    
    predict.train <- as.data.frame(predict.train)
    if (run==1){
      dataframe.predict.train <- predict.train
    } else {
      dataframe.predict.train <- cbind(dataframe.predict.train, predict.train)
    }
  }
  
  
  predict.test <- rowMeans(dataframe.predict.test)
  predict.train <- rowMeans(dataframe.predict.train)
  
  # predict.test <- as.numeric(unlist(rowmean))


  # Show the actual and predicted in test set
  cbind(predicted = predict.test,
        actual = test.set[, target_variable])
  
  # Compare the actual and predicted in train and test sets using 3 indices:
  # 1. MAPE -> mean absolute percentage error
  # 2. R^2 -> coefficient of determination
  # 3. NSE -> Nash-Sutcliffe efficiency
  MAPE.test <- round(accuracy(object = predict.test,
                        x = test.set[, target_variable])[1:1, 5:5]
                     , 2)

  R2.test <- round((cor(predict.test,
                         test.set[, target_variable],
                         method = "pearson",
                         use = "complete.obs")) ^ 2
                   , 2)

  NSE.test <- round(NSE(sim = predict.test,
                        obs = test.set[, target_variable])
                    , 2)
  
  MAPE.train <- round(accuracy(object = predict.train,
                               x = train.set[, target_variable])[1:1, 5:5]
                      , 2)
  
  R2.train <- round((cor(predict.train,
                         train.set[, target_variable],
                         method = "pearson",
                         use = "complete.obs")) ^ 2
                    , 2)
  
  NSE.train <- round(NSE(sim = predict.train,
                         obs = train.set[, target_variable])
                     , 2)
  
  
  # Computing a network score
  score(network,
        impute(fitted,
               train.set),
        type = "bic-g",
        by.node = TRUE)
  
  from = arcs(network)[,'from']
  to = arcs(network)[,'to']
  
  writeLines("\ndone\n")
  
  return (c(MAPE.train = MAPE.train,
            MAPE.test = MAPE.test,
            R2.train = R2.train,
            R2.test = R2.test,
            NSE.train = NSE.train,
            NSE.test = NSE.test,
            from = from,
            to = to))
}

writeLines("\nDefine functions  -->  done")


#######################
#####  Main code  #####
#######################


# set working directory
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

# calculate run time (start)
tic("Run time  ")

# loading data
data <- read.xlsx(file = "WWTP_data_jadid.xlsx",
                  sheetIndex = 1,
                  header = TRUE)
writeLines("\nloading data  -->  done")

# Read column names of data and use it as input and output variables
output_variables <- colnames(data)[1:11]
input_variables <- colnames(data)[12:31]

# Add the history of input variables to the data
previous_time_steps <- 2   # previous time steps for input variables
length_data <- nrow(data) - previous_time_steps

# Uncomment below lines to use the history of variables
# data <- data[(previous_time_steps+1): (length_data),]
# for (var in output_variables) {
#   a <- previous_time_steps
#   b <- length_data - 1
#   for (i in 1:previous_time_steps) {
#     history_variable_name <- paste(var,
#                                    "_",
#                                    i,
#                                    sep = "")
#     data[history_variable_name] <- assign(history_variable_name,
#                                           data[a:b, var])
#     a <- a - 1
#     b <- b - 1
#   }
# }
# input_variables <- colnames(data)[12: (25 + 11 * previous_time_steps)]

# Create result table
result_table <- data.frame(matrix(ncol = length(output_variables),
                                  nrow = 6))
colnames(result_table) <- output_variables
row.names(result_table) <- c('MAPE _ train',
                             'MAPE _ test',
                             'R2 _ train',
                             'R2 _ test',
                             'NSE _ train',
                             'NSE _ test')

# Run bnlearn for each target variable in output variables related to effluent
writeLines("
_________________________________________________________
   Learning Bayesian network with continuous variables   
_________________________________________________________  ")

train_size <- seq.int(from = 550,
                     to = 549,
                     by = -50)
train_size <- 550

test_size <- 140
k <- 0
for (size in train_size) {
  
  # First function
  train_test_splitting <- train_test_split(data,
                                           size,
                                           test_size,
                                           length_data,
                                           previous_time_steps)
  
  train.split <- as.data.frame(do.call(cbind,
                                       train_test_splitting[1]))
  test.split <- as.data.frame(do.call(cbind,
                                      train_test_splitting[2])) 
  
  # Second function
  percentage_missing_data = seq.int(from = 0,
                               to = 1,
                               by = 5) / 100
  
  for (missing in percentage_missing_data) {
    
    train.split <- random_missing_data(size,
                                       train.split,
                                       missing)
    
    # Third function
    output_variables <- c('Q_eff')
    for (target_variable in output_variables) {
      
      result.indices = bnlearn(target_variable,
                               input_variables,
                               train.split,
                               test.split)
      
      
      # writing result in table
      result_table[1, target_variable] <- result.indices['MAPE.train']
      result_table[2, target_variable] <- result.indices['MAPE.test']
      result_table[3, target_variable] <- result.indices['R2.train']
      result_table[4, target_variable] <- result.indices['R2.test']
      result_table[5, target_variable] <- result.indices['NSE.train']
      result_table[6, target_variable] <- result.indices['NSE.test']
      
    }
    
    sheet_name = paste0("mis_", missing*100, "%  size_", size)
    if (k == 0) {
      write.xlsx(x = result_table,
                 file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnlearn2.xlsx",
                 sheetName = sheet_name,
                 col.names = TRUE,
                 row.names = TRUE,
                 append = FALSE,
                 showNA = TRUE)
      k <- k + 1
    } else {
      write.xlsx(x = result_table,
                 file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnlearn2.xlsx",
                 sheetName = sheet_name,
                 col.names = TRUE,
                 row.names = TRUE,
                 append = TRUE,
                 showNA = TRUE)
      }
  }
}

# print(rowMeans(result_table))

# calculate run time (end)
toc()


# from = result.indices[7]
# to = result.indices[8]
arcdataframe <- data.frame(matrix(ncol = 2,
                                  nrow = length(from)))
i=1
while (i <= length(from) ) {
  arcdataframe[i, 1] <- from[i]
  arcdataframe[i, 2] <- to[i]
  i = i+1
}
write.xlsx(x = arcdataframe,
           file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\arc_node.xlsx",
           sheetName = target_variable,
           col.names = TRUE,
           row.names = FALSE)








# # plot
# test.set  <- test.split[, c(input_variables, target_variable)]
# v1 <- test.set[, target_variable]
# v2 <- predict.test
# 
# plot(
#   v1,
#   type = "l",
#   col = "blue",
#   ylim = range(min(min(v1, na.rm = TRUE), min(v2)), max(max(v1, na.rm = TRUE), max(v2))),
#   ann = FALSE,
#   lwd = 3
# )
# 
# lines(v2, type = "l", col = "red", lwd = 3)
# 
# title(xlab = "Days")
# title(ylab = target_variable)
# 
# legend(
#   "topright",
#   c("real", "predict"),
#   cex = 0.8,
#   col = c("blue", "red"),
#   lty = 1:1,
#   lwd = 3,
#   bty = "n",
#   bg='lightblue',
#   box.lty=1
# )
# 
# 
# box()