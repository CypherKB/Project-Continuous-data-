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
set.seed(100)
training <- sample_frac(cont_data, size=0.8)

slr_model <- lm(totcst~scale(age), data=training)
summary(slr_model)
confint(slr_model)
autoplot(slr_model)
# Cook's distance
plot(slr_model, 4)

#log transform totcst
slr_model <- lm(log(totcst)~scale(age), data=training)
summary(slr_model)
confint(slr_model)
autoplot(slr_model)
# Cook's distance
plot(slr_model, 4)

#Multiple linear regression
pred <- c("age", "sex", "dzclass_new", "num_co", "edu", "logcst")
mlr_model <- lm(log(totcst)~scale(age)+sex+dzclass_new+num_co+edu, data=training)
summary(mlr_model)
confint(mlr_model)
autoplot(mlr_model)

plot(mlr_model, 4)
