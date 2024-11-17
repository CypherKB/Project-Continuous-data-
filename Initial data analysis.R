library(haven)
library(tidyverse)
library(reshape2)
library(ggplot2)

#Read in data
data <- read_dta("support.dta")

#Check for duplicate values
sum(duplicated(data)) #There are 0 duplicate values

#Check for NA values
sum(is.na(data$age)) #Ages are always entered
sum(is.na(data$hospdead)) #Death in hospital is always entered
sum(is.na(data$sex)) #Sex is always entered
sum(is.na(data$dzclass)) #Disease class is always entered
sum(is.na(data$num_co)) #Comorbidities is always entered

sum(is.na(data$edu)) #Has 202 missing values for years of education
#Needs to be taken into account when including this in the model

sum(is.na(data$totcst))
#For 105 patients the total cost is not entered
#This cannot be used as training data for the continuous outcome

cont_data <- data[!is.na(data$totcst), ]

boxplot(cont_data$age) #All values for age are plausible

barplot(table((cont_data$sex))) #Gender

hist(cont_data$totcst)
boxplot(cont_data$totcst)
summary(cont_data$totcst)
#Heavily skewed distribution
#max = 390 000, median = 15 110

barplot(table((cont_data$hospdead))) #most people not dead in hospital

barplot(table((cont_data$dzclass))) #Most people in class 1, least in 3

barplot(table((cont_data$num_co))) #Most people 1 comorbidity
#Ranges to 7 comorbidities

hist((cont_data$edu))
boxplot(cont_data$edu)
summary(cont_data$edu)
#Most people 12 years of education
# 78 NA
#Max = 30

#Correlation matrix
#Correlations between primary predictors and totalcst
pred <- c("age", "sex", "dzclass", "num_co", "edu", "totcst")
cormat <- round(cor(cont_data[, pred], use="pairwise.complete.obs"), 2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))