# Predict-wastewater

This project aims to forecast the effluent characteristics of wastewater treatment plant (WWTP) using several machine-learning models implemented in both R and Python. The models included are:

1. Bayesian network: Implemented using Hill Climbing and Tabu Search algorithms in the `bnlearn` R package, as well as Max-Min Hill Climbing in the `bnstruct` R package for continuous variables and `bnclassify` for discrete variables.

2. LSTM for time-varying variables: Implemented in the `Keras` Python package.

3. `sklearn` models: Including KNN, SVM, DT, MLP, RF, ET, and GBM.


## Citation

- If you use the data or models in a scientific publication, we would appreciate citations: [Tackling data challenges in forecasting effluent characteristics of wastewater treatment plants](https://www.sciencedirect.com/science/article/abs/pii/S0301479724003104?via%3Dihub)
