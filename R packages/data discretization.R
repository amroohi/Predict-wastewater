# Load the package in R
library(xlsx)
library(writexl)

# set working directory
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

# loading data
df <- read.xlsx(file = "TWWTP_impute_data.xlsx",
                sheetIndex = 1,
                header = TRUE)
writeLines("\nloading data  -->  done")

# Read column names of data and use it as input and output variables
output_variables <- colnames(df)[1:11]
input_variables <- colnames(df)[12:31]


##############  discrete data  ############## 
writeLines("
____________________
   discrete data
____________________  ")


i = 1
for (row in df$Q_in) {
  
  if (row < 270000 & !is.na(row)) {
    df$Q_in[i] = 'VL'}  
  else if (row >= 270000 & row < 310000 & !is.na(row)) {
    df$Q_in[i] = 'L'}  
  else if (row >= 310000 & row < 350000 & !is.na(row)) {
    df$Q_in[i] = 'LM'}  
  else if (row >= 350000 & row < 390000 & !is.na(row)) {
    df$Q_in[i] = 'M'}  
  else if (row >= 390000 & row < 430000 & !is.na(row)) {
    df$Q_in[i] = 'HM'}  
  else if (row >= 430000 & row < 460000 & !is.na(row)) {
    df$Q_in[i] = 'H'}  
  else if (row >= 460000 & !is.na(row)){
    df$Q_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$T_in) {
  
  if (row < 16 & !is.na(row)) {
    df$T_in[i] = 'VL'}  
  else if (row >= 16 & row < 19 & !is.na(row)) {
    df$T_in[i] = 'L'}
  else if (row >= 19 & row < 21 & !is.na(row)) {
    df$T_in[i] = 'LM'}
  else if (row >= 21 & row < 23 & !is.na(row)) {
    df$T_in[i] = 'M'} 
  else if (row >= 23 & row < 25 & !is.na(row)) {
    df$T_in[i] = 'HM'}
  else if (row >= 25 & row < 27 & !is.na(row)) {
    df$T_in[i] = 'H'} 
  else if (row >= 27 & !is.na(row)){
    df$T_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$pH_in) {
  
  if (row < 6.9 & !is.na(row)) {
    df$pH_in[i] = 'VL'}  
  else if (row >= 6.9 & row < 7.1 & !is.na(row)) {
    df$pH_in[i] = 'L'} 
  else if (row >= 7.1 & row < 7.3 & !is.na(row)) {
    df$pH_in[i] = 'LM'}
  else if (row >= 7.3 & row < 7.5 & !is.na(row)) {
    df$pH_in[i] = 'M'} 
  else if (row >= 7.5 & row < 7.7 & !is.na(row)) {
    df$pH_in[i] = 'HM'}
  else if (row >= 7.7 & row < 7.9 & !is.na(row)) {
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
    df$COD_in[i] = 'LM'}
  else if (row >= 500 & row < 600 & !is.na(row)) {
    df$COD_in[i] = 'M'}
  else if (row >= 600 & row < 700 & !is.na(row)) {
    df$COD_in[i] = 'HM'}
  else if (row >= 700 & row < 800 & !is.na(row)) {
    df$COD_in[i] = 'H'}
  else if (row >= 800 & !is.na(row)){
    df$COD_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$BOD_in) {
  
  if (row < 150 & !is.na(row)) {
    df$BOD_in[i] = 'VL'}  
  else if (row >= 150 & row < 200 & !is.na(row)) {
    df$BOD_in[i] = 'L'}  
  else if (row >= 200 & row < 250 & !is.na(row)) {
    df$BOD_in[i] = 'LM'}  
  else if (row >= 250 & row < 300 & !is.na(row)) {
    df$BOD_in[i] = 'M'}  
  else if (row >= 300 & row < 350 & !is.na(row)) {
    df$BOD_in[i] = 'HM'}  
  else if (row >= 350 & row < 400 & !is.na(row)) {
    df$BOD_in[i] = 'H'}  
  else if (row >= 400 & !is.na(row)){
    df$BOD_in[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$TSS_in) {
  
  if (row < 70 & !is.na(row)) {
    df$TSS_in[i] = 'VL'}  
  else if (row >= 70 & row < 100 & !is.na(row)) {
    df$TSS_in[i] = 'L'}  
  else if (row >= 100 & row < 150 & !is.na(row)) {
    df$TSS_in[i] = 'LM'} 
  else if (row >= 150 & row < 250 & !is.na(row)) {
    df$TSS_in[i] = 'M'} 
  else if (row >= 250 & row < 400 & !is.na(row)) {
    df$TSS_in[i] = 'HM'} 
  else if (row >= 400 & row < 600 & !is.na(row)) {
    df$TSS_in[i] = 'H'} 
  else if (row >= 600 & !is.na(row)){
    df$TSS_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$NH4_in) {
  
  if (row < 30 & !is.na(row)) {
    df$NH4_in[i] = 'VL'}  
  else if (row >= 30 & row < 35 & !is.na(row)) {
    df$NH4_in[i] = 'L'}  
  else if (row >= 35 & row < 40 & !is.na(row)) {
    df$NH4_in[i] = 'LM'}  
  else if (row >= 40 & row < 50 & !is.na(row)) {
    df$NH4_in[i] = 'M'}  
  else if (row >= 50 & row < 60 & !is.na(row)) {
    df$NH4_in[i] = 'HM'}  
  else if (row >= 60 & row < 75 & !is.na(row)) {
    df$NH4_in[i] = 'H'}  
  else if (row >= 75 & !is.na(row)){
    df$NH4_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TN_in) {
  
  if (row < 35 & !is.na(row)) {
    df$TN_in[i] = 'VL'}  
  else if (row >= 35 & row < 40 & !is.na(row)) {
    df$TN_in[i] = 'L'} 
  else if (row >= 40 & row < 45 & !is.na(row)) {
    df$TN_in[i] = 'LM'}  
  else if (row >= 45 & row < 50 & !is.na(row)) {
    df$TN_in[i] = 'M'}  
  else if (row >= 50 & row < 60 & !is.na(row)) {
    df$TN_in[i] = 'HM'}  
  else if (row >= 60 & row < 80 & !is.na(row)) {
    df$TN_in[i] = 'H'}  
  else if (row >= 80 & !is.na(row)){
    df$TN_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$PO4_in) {
  
  if (row < 3 & !is.na(row)) {
    df$PO4_in[i] = 'VL'}  
  else if (row >= 3 & row < 3.5 & !is.na(row)) {
    df$PO4_in[i] = 'L'}  
  else if (row >= 3.5 & row < 4 & !is.na(row)) {
    df$PO4_in[i] = 'LM'}  
  else if (row >= 4 & row < 5 & !is.na(row)) {
    df$PO4_in[i] = 'M'}  
  else if (row >= 5 & row < 7 & !is.na(row)) {
    df$PO4_in[i] = 'HM'}  
  else if (row >= 7 & row < 9 & !is.na(row)) {
    df$PO4_in[i] = 'H'}  
  else if (row >= 9 & !is.na(row)){
    df$PO4_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$TP_in) {
  
  if (row < 5 & !is.na(row)) {
    df$TP_in[i] = 'VL'}  
  else if (row >= 5 & row < 6 & !is.na(row)) {
    df$TP_in[i] = 'L'} 
  else if (row >= 6 & row < 6.5 & !is.na(row)) {
    df$TP_in[i] = 'LM'} 
  else if (row >= 6.5 & row < 7 & !is.na(row)) {
    df$TP_in[i] = 'M'} 
  else if (row >= 7 & row < 8.5 & !is.na(row)) {
    df$TP_in[i] = 'HM'} 
  else if (row >= 8.5 & row < 10 & !is.na(row)) {
    df$TP_in[i] = 'H'} 
  else if (row >= 10 & !is.na(row)){
    df$TP_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$EC_in) {
  
  if (row < 750 & !is.na(row)) {
    df$EC_in[i] = 'VL'}  
  else if (row >= 750 & row < 1000 & !is.na(row)) {
    df$EC_in[i] = 'L'}
  else if (row >= 1000 & row < 1200 & !is.na(row)) {
    df$EC_in[i] = 'LM'} 
  else if (row >= 1200 & row < 1400 & !is.na(row)) {
    df$EC_in[i] = 'M'} 
  else if (row >= 1400 & row < 1600 & !is.na(row)) {
    df$EC_in[i] = 'HM'} 
  else if (row >= 1600 & row < 1800 & !is.na(row)) {
    df$EC_in[i] = 'H'} 
  else if (row >= 1800 & !is.na(row)){
    df$EC_in[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$MLSS_at) {
  
  if (row < 1500 & !is.na(row)) {
    df$MLSS_at[i] = 'VL'}  
  else if (row >= 1500 & row < 2000 & !is.na(row)) {
    df$MLSS_at[i] = 'L'}  
  else if (row >= 2000 & row < 2500 & !is.na(row)) {
    df$MLSS_at[i] = 'LM'}
  else if (row >= 2500 & row < 3000 & !is.na(row)) {
    df$MLSS_at[i] = 'M'}
  else if (row >= 3000 & row < 3500 & !is.na(row)) {
    df$MLSS_at[i] = 'HM'}
  else if (row >= 3500 & row < 4000 & !is.na(row)) {
    df$MLSS_at[i] = 'H'}
  else if (row >= 4000 & !is.na(row)){
    df$MLSS_at[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$DO_at) {
  
  if (row < 0.7 & !is.na(row)) {
    df$DO_at[i] = 'VL'}  
  else if (row >= 0.7 & row < 1 & !is.na(row)) {
    df$DO_at[i] = 'L'}  
  else if (row >= 1 & row < 1.3 & !is.na(row)) {
    df$DO_at[i] = 'LM'}  
  else if (row >= 1.3 & row < 1.6 & !is.na(row)) {
    df$DO_at[i] = 'M'}  
  else if (row >= 1.6 & row < 1.9 & !is.na(row)) {
    df$DO_at[i] = 'HM'}  
  else if (row >= 1.9 & row < 2.2 & !is.na(row)) {
    df$DO_at[i] = 'H'}  
  else if (row >= 2.2 & !is.na(row)){
    df$DO_at[i] = 'VH'}
  
  i = i + 1
}




i = 1
for (row in df$MLSS_re) {
  
  if (row < 4000 & !is.na(row)) {
    df$MLSS_re[i] = 'VL'}  
  else if (row >= 4000 & row < 4700 & !is.na(row)) {
    df$MLSS_re[i] = 'L'}
  else if (row >= 4700 & row < 5400 & !is.na(row)) {
    df$MLSS_re[i] = 'LM'} 
  else if (row >= 5400 & row < 6100 & !is.na(row)) {
    df$MLSS_re[i] = 'M'} 
  else if (row >= 6100 & row < 6800 & !is.na(row)) {
    df$MLSS_re[i] = 'HM'} 
  else if (row >= 6800 & row < 7500 & !is.na(row)) {
    df$MLSS_re[i] = 'H'} 
  else if (row >= 7500 & !is.na(row)){
    df$MLSS_re[i] = 'VH'}
  
  i = i + 1
}



i = 1
for (row in df$T_air_avg) {
  
  if (row < 0 & !is.na(row)) {
    df$T_air_avg[i] = 'VL'}  
  else if (row >= 0 & row < 6 & !is.na(row)) {
    df$T_air_avg[i] = 'L'} 
  else if (row >= 6 & row < 12 & !is.na(row)) {
    df$T_air_avg[i] = 'LM'}  
  else if (row >= 12 & row < 18 & !is.na(row)) {
    df$T_air_avg[i] = 'M'}  
  else if (row >= 18 & row < 24 & !is.na(row)) {
    df$T_air_avg[i] = 'HM'}  
  else if (row >= 24 & row < 30 & !is.na(row)) {
    df$T_air_avg[i] = 'H'}  
  else if (row >= 30 & !is.na(row)){
    df$T_air_avg[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$T_air_min) {
  
  if (row < 0 & !is.na(row)) {
    df$T_air_min[i] = 'VL'}  
  else if (row >= 0 & row < 5 & !is.na(row)) {
    df$T_air_min[i] = 'L'}  
  else if (row >= 5 & row < 10 & !is.na(row)) {
    df$T_air_min[i] = 'LM'}  
  else if (row >= 10 & row < 15 & !is.na(row)) {
    df$T_air_min[i] = 'M'}  
  else if (row >= 15 & row < 20 & !is.na(row)) {
    df$T_air_min[i] = 'HM'}  
  else if (row >= 20 & row < 25 & !is.na(row)) {
    df$T_air_min[i] = 'H'}  
  else if (row >= 25 & !is.na(row)){
    df$T_air_min[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$T_air_max) {
  
  if (row < 0 & !is.na(row)) {
    df$T_air_max[i] = 'VL'}  
  else if (row >= 0 & row < 7 & !is.na(row)) {
    df$T_air_max[i] = 'L'} 
  else if (row >= 7 & row < 14 & !is.na(row)) {
    df$T_air_max[i] = 'LM'}  
  else if (row >= 14 & row < 21 & !is.na(row)) {
    df$T_air_max[i] = 'M'}  
  else if (row >= 21 & row < 28 & !is.na(row)) {
    df$T_air_max[i] = 'HM'}  
  else if (row >= 28 & row < 35 & !is.na(row)) {
    df$T_air_max[i] = 'H'}  
  else if (row >= 35 & !is.na(row)){
    df$T_air_max[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$R) {
  
  if (row < 0.01 & !is.na(row)) {
    df$R[i] = 'VL'}  
  else if (row >= 0.01 & row < 0.1 & !is.na(row)) {
    df$R[i] = 'L'}  
  else if (row >= 0.1 & row < 0.25 & !is.na(row)) {
    df$R[i] = 'LM'}
  else if (row >= 0.25 & row < 0.4 & !is.na(row)) {
    df$R[i] = 'M'}
  else if (row >= 0.4 & row < 5 & !is.na(row)) {
    df$R[i] = 'HM'}  
  else if (row >= 5 & row < 10 & !is.na(row)) {
    df$R[i] = 'H'}  
  else if (row >= 10 & !is.na(row)){
    df$R[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$W) {
  
  if (row < 0.5 & !is.na(row)) {
    df$W[i] = 'VL'}  
  else if (row >= 0.5 & row < 1 & !is.na(row)) {
    df$W[i] = 'L'}  
  else if (row >= 1 & row < 1.5 & !is.na(row)) {
    df$W[i] = 'LM'}  
  else if (row >= 1.5 & row < 2 & !is.na(row)) {
    df$W[i] = 'M'}  
  else if (row >= 2 & row < 4 & !is.na(row)) {
    df$W[i] = 'HM'}  
  else if (row >= 4 & row < 7 & !is.na(row)) {
    df$W[i] = 'H'}  
  else if (row >= 7 & !is.na(row)){
    df$W[i] = 'VH'}
  
  i = i + 1
}



i = 1
for (row in df$H) {
  
  if (row < 15 & !is.na(row)) {
    df$H[i] = 'VL'}  
  else if (row >= 15 & row < 25 & !is.na(row)) {
    df$H[i] = 'L'} 
  else if (row >= 25 & row < 35 & !is.na(row)) {
    df$H[i] = 'LM'}  
  else if (row >= 35 & row < 45 & !is.na(row)) {
    df$H[i] = 'M'}  
  else if (row >= 45 & row < 60 & !is.na(row)) {
    df$H[i] = 'HM'}  
  else if (row >= 60 & row < 75 & !is.na(row)) {
    df$H[i] = 'H'}  
  else if (row >= 75 & !is.na(row)){
    df$H[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$Q_eff) {
  
  if (row < 270000 & !is.na(row)) {
    df$Q_eff[i] = 'VL'}  
  else if (row >= 270000 & row < 310000 & !is.na(row)) {
    df$Q_eff[i] = 'L'}  
  else if (row >= 310000 & row < 350000 & !is.na(row)) {
    df$Q_eff[i] = 'LM'}  
  else if (row >= 350000 & row < 390000 & !is.na(row)) {
    df$Q_eff[i] = 'M'}  
  else if (row >= 390000 & row < 430000 & !is.na(row)) {
    df$Q_eff[i] = 'HM'}  
  else if (row >= 430000 & row < 460000 & !is.na(row)) {
    df$Q_eff[i] = 'H'}  
  else if (row >= 460000 & !is.na(row)){
    df$Q_eff[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$T_eff) {
  
  if (row < 18 & !is.na(row)) {
    df$T_eff[i] = 'VL'}  
  else if (row >= 18 & row < 22 & !is.na(row)) {
    df$T_eff[i] = 'L'}
  else if (row >= 22 & row < 25 & !is.na(row)) {
    df$T_eff[i] = 'LM'}
  else if (row >= 25 & row < 26 & !is.na(row)) {
    df$T_eff[i] = 'M'} 
  else if (row >= 26 & row < 27 & !is.na(row)) {
    df$T_eff[i] = 'HM'}
  else if (row >= 27 & row < 28 & !is.na(row)) {
    df$T_eff[i] = 'H'} 
  else if (row >= 28 & !is.na(row)){
    df$T_eff[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$pH_eff) {
  
  if (row < 5 & !is.na(row)) {
    df$pH_eff[i] = 'VL'}  
  else if (row >= 5 & row < 6 & !is.na(row)) {
    df$pH_eff[i] = 'L'}
  else if (row >= 6 & row < 6.5 & !is.na(row)) {
    df$pH_eff[i] = 'LM'}
  else if (row >= 6.5 & row < 8.4 & !is.na(row)) {
    df$pH_eff[i] = 'M'} 
  else if (row >= 8.4 & row < 8.5 & !is.na(row)) {
    df$pH_eff[i] = 'HM'}
  else if (row >= 8.5 & row < 9 & !is.na(row)) {
    df$pH_eff[i] = 'H'} 
  else if (row >= 9 & !is.na(row)){
    df$pH_eff[i] = 'VH'}
  
  i = i + 1
}



i = 1
for (row in df$COD_eff) {
  
  if (row < 23 & !is.na(row)) {
    df$COD_eff[i] = 'VL'}  
  else if (row >= 23 & row < 60 & !is.na(row)) {
    df$COD_eff[i] = 'L'} 
  else if (row >= 60 & row < 75 & !is.na(row)) {
    df$COD_eff[i] = 'M'} 
  else if (row >= 75 & row < 200 & !is.na(row)) {
    df$COD_eff[i] = 'H'} 
  else if (row >= 200 & !is.na(row)){
    df$COD_eff[i] = 'VH'}
  
  i = i + 1
}


i = 1
for (row in df$BOD_eff) {
  
  if (row < 30 & !is.na(row)) {
    df$BOD_eff[i] = 'L'}  
  else if (row >= 30 & row < 31 & !is.na(row)) {
    df$BOD_eff[i] = 'M'} 
  else if (row >= 31 & row < 100 & !is.na(row)) {
    df$BOD_eff[i] = 'H'} 
  else if (row >= 100 & !is.na(row)){
    df$BOD_eff[i] = 'VH'}

  
  i = i + 1
}


i = 1
for (row in df$TSS_eff) {
  
  if (row < 40 & !is.na(row)) {
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
  
  if (row < 7 & !is.na(row)) {
    df$NH4_eff[i] = 'L'}  
  else if (row >= 7 & !is.na(row)){
    df$NH4_eff[i] = 'H'}
  
  i = i + 1
}


i = 1
for (row in df$TN_eff) {
  
  if (row < 7 & !is.na(row)) {
    df$TN_eff[i] = 'L'}  
  else if (row >= 7 & !is.na(row)){
    df$TN_eff[i] = 'H'}
  
  i = i + 1
}


i = 1
for (row in df$PO4_eff) {
  
  if (row < 6 & !is.na(row)) {
    df$PO4_eff[i] = 'L'}  
  else if (row >= 6 & row < 50 & !is.na(row)) {
    df$PO4_eff[i] = 'M'} 
  else if (row >= 50 & !is.na(row)){
    df$PO4_eff[i] = 'H'}
  
  i = i + 1
}


i = 1
for (row in df$TP_eff) {
  
  if (row < 4 & !is.na(row)) {
    df$TP_eff[i] = 'L'}  
  else if (row >= 4 & row < 6 & !is.na(row)) {
    df$TP_eff[i] = 'M'}  
  else if (row >= 6 & !is.na(row)){
    df$TP_eff[i] = 'H'}
  
  i = i + 1
}


i = 1
for (row in df$FC_eff) {
  
  if (row < 400 & !is.na(row)) {
    df$FC_eff[i] = 'L'}  
  else if (row >= 400 & row < 1000 & !is.na(row)) {
    df$FC_eff[i] = 'M'} 
  else if (row >= 1000 & !is.na(row)){
    df$FC_eff[i] = 'H'}
  
  i = i + 1
}

write_xlsx(df, "TWWTP_impute_data_discrete.xlsx")

