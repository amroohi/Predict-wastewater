library(xlsx)
library(mice)
# library(dplyr)
setwd("C:\\Users\\sh\\Desktop\\term 3 ut\\Thesis and Paper\\R\\New folder")

data <- read.xlsx(file = "BWWTP_raw_data.xlsx",
                  sheetIndex = 1,
                  header = TRUE)

# md.pattern(data)

data.imputation <- complete(mice(data=data,
                                 method="norm.predict",
                                 m=10,
                                 maxit=1))

write.xlsx(x = data.imputation,
           file = "BWWTP_impute_data.xlsx",
           col.names = TRUE,
           row.names = FALSE,
           append = FALSE,
           showNA = TRUE)

# 
# library(tidyverse)
# 
# data %>%
#   head(10)
# 
# theme_set(theme_bw(base_size=16))
# 
# missing.values <- data %>%
#   gather(key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   group_by(key) %>%
#   mutate(total = n()) %>%
#   group_by(key, total, isna) %>%
#   summarise(num.isna = n()) %>%
#   mutate(pct = num.isna / total * 100)
# 
# levels <-
#   (missing.values  %>% filter(isna == F) %>% arrange(desc(pct)))$key
# 
# data  %>%
#   mutate(id = row_number()) %>%
#   gather(-id, key = "key", value = "val") %>%
#   mutate(isna = is.na(val)) %>%
#   ggplot(aes(key, id, fill = isna)) +
#   geom_raster(alpha=0.8) +
#   scale_fill_manual(name = "",
#                     values = c('black', 'azure4'),
#                     labels = c("Observe value", "Missing value")) +
#   scale_x_discrete(limits = levels) +
#   labs(x = "Variables",
#        y = "Row Number", title = "Missing values in rows") +
#   coord_flip()
# 
# # load the library
# library(finalfit)
# 
# # Plot missing data
# missing_plot(data)

