library(tidyverse)
library(broom)
library(ggplot2)
library(ggfortify)
#Read in data
data <- read_dta("support.dta")

#For 105 patients the total cost is not entered
#This cannot be used as training data for the continuous outcome
#Suppose this is missing at random
cont_data <- data[!is.na(data$totcst), ]

#For better linear regression change labels 2 and 3
cont_data$dzclass_new <- cont_data$dzclass
cont_data$dzclass_new[cont_data$dzclass == 2] <- 3
cont_data$dzclass_new[cont_data$dzclass == 3] <- 2

#Exclude outlier totcst=0
cont_data <- cont_data[-730, ]
#retain 80% data

model <- lm(log(totcst)~scale(age), data=cont_data)
summary(model)
autoplot(model)
# Cook's distance
plot(model, 4)

