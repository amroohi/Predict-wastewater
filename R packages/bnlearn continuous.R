# options(timeout=100)   # to install library

# Load the package in R
# library(bnviewer)
# library(visNetwork)
library(igraph)
# library(networkD3)
library(bnlearn)
library(forecast)
library(xlsx)
library(qgraph)
library(hydroGOF) 
# library(Rgraphviz)  # is not available for this version of R
library(tictoc)
library(writexl)
writeLines("\nloading libraries  -->  done")

###################################
#####  Define four functions  #####
###################################

# 1. train_test_split:
#    This function adjusts train and test dataset to a specific size.
train_test_split <- function(data,
                             size,
                             test_size,
                             length_data,
                             previous_time_steps) {
  
  # Split data into train and test sets
  # train.split <- data[(length_data - test_size - size +1):
  #                       (length_data - test_size), ]
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
  
  writeLines(paste("\nstart missing function to apply  --> ",
                   as.integer(missing * 100),
                   "% portion of missing data"))
  initial_missing_data <- mean(colSums(is.na(train.split[, input_variables]))
                               /size)
  i <- 1
  while (i < ((missing - initial_missing_data) * size *
              ncol(train.split[, input_variables]))){
    random_column <- sample(colnames(train.split[, input_variables]), 1)
    random_row <- sample(1:(size + 1), 1, replace = FALSE)
    if (!is.na(train.split[random_row, random_column])) {
      train.split[random_row, random_column] <- NA
      i <- i + 1
    }
  }
  
  return (train.split)
}

# 3. random impute data
#    This function imputes a random missing data in dataset.
random_impute_data <- function(size,
                               train.split,
                               missing,
                               data.imputation) {
  
  writeLines(paste("\nstart impute function to apply  --> ",
                   as.integer(missing * 100),
                   "% portion of missing data"))
  initial_missing_data <- mean(colSums(is.na(train.split[, input_variables]))
                               /size)
  i <- 1
  while (i < ((initial_missing_data - missing) * size * 
              ncol(train.split[, input_variables]))){
    random_column <- sample(colnames(train.split[, input_variables]), 1)
    random_row <- sample(1:(size + 1), 1, replace = FALSE)
    if (is.na(train.split[random_row, random_column])) {
      train.split[random_row, random_column] <- data.imputation[random_row,
                                                                random_column]
      i <- i + 1
    }
  }
  
  return (train.split)
}


# 4. bnlearn:
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
  
  iteration=1
  for (run in 1:iteration) {
    
    writeLines(paste("run  --> ",
                     run))
    
    # Learn Bayesian network structure on train set using hc and tabu algorithms
    network <- structural.em(x = train.set,
                             maximize = "hc",
                             fit = "mle-g",
                             debug = FALSE,
                             max.iter = 10,
                             maximize.args = list(max.iter = 20,
                                                  restart = 1000,
                                                  score = "bic-g"))
    
    # network <- structural.em(x = train.set,
    #                          maximize = "tabu",
    #                          fit = "mle-g",
    #                          debug = FALSE,
    #                          max.iter = 10,
    #                          maximize.args = list(max.iter = 20,
    #                                               tabu = 50,
    #                                               score = "bic-g"))
    
    # print(network)
    
    # For drop, add or set the direction of an arc or an edge, use below line
    # network <- set.arc(network, from = "TSS1_eff", to = var)

    # Estimate the parameters of the Bayesian network
    fitted <- bn.fit(x = network,
                     data = train.set,
                     method = "mle-g")
    # 
    # 
    # library(shapr)
    # explainer <- shapr(train.set, fitted)
    # p <- mean(train.set[1,1])
    # explanation <- explain(test.set,
    #                        approach = "empirical",
    #                        model = fitted,
    #                        # explainer = explainer,
    #                        prediction_zero = p)
    # print(explanation$dt)
    # plot(explanation, plot_phi0 = FALSE, index_x_test = c(1, 6))
    # 
    
    # print(fitted[target_variable])
    # arc.strength(network, impute(fitted, train.set))

    # Plotting networks with the Rgraphviz package
    # graphviz_plot = graphviz.plot(network,
    #                               layout = "dot",
    #                               shape = "circle",
    #                               render = FALSE,
    #                               highlight = list(nodes = nodes(network),
    #                                                fill = "lightgreen",
    #                                                col = "black"))
    # 
    # nodeRenderInfo(graphviz_plot) <- list(fontsize = 100,
    #                                       arrowsize = 1000,
    #                                       distortion = 200,
    #                                       width = 100)
    # edgeRenderInfo(graphviz_plot) <- list(fontsize = 400,
    #                                       arrowsize = 1000,
    #                                       distortion = 200,
    #                                       width = 100)
    # graphRenderInfo(graphviz_plot) <- list(fontsize = 400,
    #                                        arrowsize = 1000,
    #                                        distortion = 200,
    #                                        width = 100)
    # parRenderInfo(graphviz_plot) <- list(edges = list(lwd = 200,
    #                                                   lty = "dashed"),
    #                                      nodes = list(col = "gray",
    #                                                   fill = "gray"))
    # renderGraph(graphviz_plot,
    #             drawNodes="renderNodes")
    # 
    # 
    # plot(as.graphNEL(fitted),
    #      attrs=list(graph = list(rankdir ="TB"),
    #                 node = list(fillcolor = "azure2",
    #                             width=2,
    #                             fontsize = 30,
    #                             shape = "rectangle"),
    #                 edge = list(color = "darkslategray",
    #                             arrowsize =2.5)))

    # Plot other graph
    # bn.fit.qqplot(fitted$BOD_in)
    # bn.fit.xyplot(fitted$BOD_in)
    # bn.fit.histogram(fitted$BOD_in)
    # plot(network)
    # graphviz.plot(network)
    # qgraph(network,
    #        layout = "circular",
    #        vsize = 10,
    #        asize = 4,
    #        edge.color = "black")
    
    
    # write.xlsx(x = arc.strength(network, impute(fitted, train.set)),
    #            file = "arc strength.xlsx",
    #            col.names = TRUE,
    #            row.names = TRUE,
    #            append = FALSE,
    #            showNA = TRUE)
    
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
  # 1. RMSE -> Root mean square error
  # 2. R^2 -> coefficient of determination
  # 3. NSE -> Nash-Sutcliffe efficiency
  RMSE.test <- round(accuracy(object = predict.test,
                        x = test.set[, target_variable])[1:1, 2:2]
                     , 2)
  
  R2.test <- round((cor(predict.test,
                         test.set[, target_variable],
                         method = "pearson",
                         use = "complete.obs")) ^ 2
                   , 2)

  NSE.test <- round(NSE(sim = predict.test,
                        obs = test.set[, target_variable])
                    , 2)

  sumnum=0
  for (tr in 1:length(predict.test)){
    if (is.na(test.set[, target_variable][tr])=="FALSE"&
        test.set[, target_variable][tr]==round(predict.test[tr],0)){
      sumnum = sumnum +1
    }
  }
  SA.test = sumnum/length(predict.test)
  
  RMSE.train <- round(accuracy(object = predict.train,
                               x = train.set[, target_variable])[1:1, 2:2]
                      , 2)
  
  R2.train <- round((cor(predict.train,
                         train.set[, target_variable],
                         method = "pearson",
                         use = "complete.obs")) ^ 2
                    , 2)
  
  NSE.train <- round(NSE(sim = predict.train,
                         obs = train.set[, target_variable])
                     , 2)
  
  sumnum=0
  for (tr in 1:length(predict.train)){
    if (is.na(train.set[, target_variable][tr])=="FALSE"&
        train.set[, target_variable][tr]==round(predict.train[tr],0)){
      sumnum = sumnum +1
    }
  }
  SA.train = sumnum/length(predict.train)
  
  print(R2.train)
  print(R2.test)
  
  
  # Computing a network score
  score(network,
        impute(fitted,
               train.set),
        type = "bic-g",
        by.node = FALSE)
  
  # save "from" "to" network
  # from = arcs(network)[,'from']
  # to = arcs(network)[,'to']
  # arcdataframe <- data.frame(matrix(ncol = 2,
  #                                   nrow = length(from)))
  # i=1
  # while (i <= length(from) ) {
  #   arcdataframe[i, 1] <- from[i]
  #   arcdataframe[i, 2] <- to[i]
  #   i = i+1
  # }
  # write.xlsx(x = arcdataframe,
  #            file = "arc_node.xlsx",
  #            sheetName = target_variable,
  #            col.names = TRUE,
  #            row.names = FALSE)
  
  writeLines("\ndone\n")
  
  return (c(RMSE.train = RMSE.train,
            RMSE.test = RMSE.test,
            R2.train = R2.train,
            R2.test = R2.test,
            NSE.train = NSE.train,
            NSE.test = NSE.test,
            SA.train = SA.train,
            SA.test = SA.test))
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
data <- read.xlsx(file = "MWWTP_raw_data.xlsx",
                  sheetIndex = 1,
                  header = TRUE)
# data=scale(data)

# drop column of data
# data <- subset(data,
#                select = -c(H))

# data[data == 'VL'] <- 1
# data[data == 'L'] <-  2
# data[data == 'LM'] <- 3
# data[data == 'M'] <-  4
# data[data == 'HM'] <- 5
# data[data == 'H'] <-  6
# data[data == 'VH'] <- 7
# for (ja in colnames(data)){
#   data[,ja] = as.numeric(data[,ja])
# }

data.imputation <- read.xlsx(file = "MWWTP_impute_data.xlsx",
                             sheetIndex = 1,
                             header = TRUE)
# data.imputation <- data
# data.imputation=scale(data.imputation)

writeLines("\nloading data  -->  done")

# Read column names of data and use it as input and output variables
output_variables <- colnames(data)[0:2]
input_variables <- colnames(data)[3:14]
# input_variables <- c('Q_in','BOD_in','COD_in','TSS_in','DO_at')

# input_variables <- c('Q_in','BOD_in','COD_in','TSS_in','DO_at',
#                      'pH_in','MLSS_at','MLSS_re','T_in','W')

# input_variables <- c('Q_in','BOD_in','COD_in','TSS_in','DO_at',
#                      'pH_in','MLSS_at','MLSS_re','T_in','W',
#                      'TN_in','TP_in','NH4_in','PO4_in','T_air_avg')

# input_variables <- c('Q_in','BOD_in','COD_in','TSS_in','DO_at',
#                      'pH_in','MLSS_at','MLSS_re','T_in','W',
#                      'TN_in','TP_in','NH4_in','PO4_in','T_air_avg',
#                      'T_air_min','T_air_max','R','H','EC_in')


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
                                  nrow = 8))
colnames(result_table) <- output_variables
row.names(result_table) <- c('RMSE _ train',
                             'RMSE _ test',
                             'R2 _ train',
                             'R2 _ test',
                             'NSE _ train',
                             'NSE _ test',
                             'SA_train',
                             'SA_test')

# Run bnlearn for each target variable in output variables related to effluent
writeLines("
_________________________________________________________
   Learning Bayesian network with continuous variables   
_________________________________________________________  ")

train_size <- seq.int(from = 1100,
                      to = 1099,
                      by = -50)
test_size <- 280
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
  percentage_missing_data <- numeric()
  # percentage_missing_data <- seq.int(from = 0,
  #                                    to = 50,
  #                                    by = 5) / 100
  initial_missing_data <- mean(colSums(is.na(train.split[, input_variables]))
                               /size)
  writeLines(paste("\nInitial missing data  --> ",
                   as.integer(initial_missing_data*100),
                   "%"))
  
  need_impute <- numeric()
  need_missing <- initial_missing_data
  for (missing in percentage_missing_data){
    if (missing<initial_missing_data){
      need_impute <- c(need_impute, missing)
    } else if (missing>initial_missing_data){
      need_missing <- c(need_missing, missing)
    }
  }
  
  flag <- 0
  z <- 0
  d <- 1
  train.split.copy <- train.split
  for (m in c(need_impute, need_missing)) {
    
    if ((flag==0) && (length(need_missing)!=0)){
      missing <- need_missing[d]
      train.split <- random_missing_data(size,
                                         train.split,
                                         missing)
    } else {
      missing <- rev(need_impute)[d]
      train.split <- random_impute_data(size,
                                        train.split,
                                        missing,
                                        data.imputation)   
    }
    d <- d+1

    # Third function
    # output_variables <- c('pH_eff')
    for (target_variable in output_variables) {

      if (typeof(try(silent = TRUE, expr = {
          result.indices = bnlearn(target_variable,
                                   input_variables,
                                   train.split,
                                   test.split)
          }
      ))=='character') {
        writeLines(paste("\nError ***can not learn BN***"))
        # writing result in table
        result_table[1, target_variable] <- '___'
        result_table[2, target_variable] <- '___'
        result_table[3, target_variable] <- '___'
        result_table[4, target_variable] <- '___'
        result_table[5, target_variable] <- '___'
        result_table[6, target_variable] <- '___'
        result_table[7, target_variable] <- '___'
        result_table[8, target_variable] <- '___'
      } else {
        
        # writing result in table
        result_table[1, target_variable] <- result.indices['RMSE.train']
        result_table[2, target_variable] <- result.indices['RMSE.test']
        result_table[3, target_variable] <- result.indices['R2.train']
        result_table[4, target_variable] <- result.indices['R2.test']
        result_table[5, target_variable] <- result.indices['NSE.train']
        result_table[6, target_variable] <- result.indices['NSE.test']
        result_table[7, target_variable] <- result.indices['SA.train']
        result_table[8, target_variable] <- result.indices['SA.test']
      }
    }
    
    sheet_name = paste0("mis_", round(missing*100,2), "%  size_", size)
    if (k == 0) {
      write.xlsx(x = result_table,
                 file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnlearn TWWTP.xlsx",
                 sheetName = sheet_name,
                 col.names = TRUE,
                 row.names = TRUE,
                 append = FALSE,
                 showNA = TRUE)
      k <- k + 1
    } else {
      write.xlsx(x = result_table,
                 file = "C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder\\result bnlearn TWWTP.xlsx",
                 sheetName = sheet_name,
                 col.names = TRUE,
                 row.names = TRUE,
                 append = TRUE,
                 showNA = TRUE)
      }
    z <- z+1
    if (z==length(need_missing)){
      train.split <- train.split.copy
      flag <- 1
      d <- 1
    }
  }
}


# calculate run time (end)
toc()











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