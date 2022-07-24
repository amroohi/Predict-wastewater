library(xlsx)
library(mice)
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

data <- read.xlsx(file = "STWWTP_raw_data.xlsx",
                  sheetIndex = 1,
                  header = TRUE)

# Regression imputation with mice package
data.imputation = complete(mice(data=data,
                                method="norm.predict",
                                m=1,
                                maxit=1))

write.xlsx(x = data.imputation,
           file = "STWWTP_impute_data.xlsx",
           sheetName = sheet_name,
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE,
           showNA = TRUE)