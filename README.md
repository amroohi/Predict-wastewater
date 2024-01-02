# Predict-wastewater

This project aims to predict the effluent characteristics of a wastewater treatment plant (WWTP) using several machine learning models implemented in both R and Python. The models included are:

1. Bayesian network: Implemented using Hill Climbing and Tabu Search algorithms in the `bnlearn` R package, as well as Max-Min Hill Climbing in the `bnstruct` R package for continuous variables and `bnclassify` for discrete variables.

2. LSTM for time-varying variables: Implemented in the `Keras` Python package.

3. `sklearn` models: Including KNN, SVM, DT, MLP, RF, ET, and GBM.
