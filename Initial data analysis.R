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

#log transform of cost
#Add small constant such that we don't have log(0)
cont_data$logcst <- log(cont_data[, "totcst"]+1) 
#Correlation matrix
#Correlations between primary predictors and totalcst
pred <- c("age", "sex", "dzclass", "num_co", "edu", "totcst", "logcst")
cormat <- round(cor(cont_data[, pred], use="pairwise.complete.obs"), 2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#Plot of correlation matrix
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


#plot all combinations of variables

#All combinations of age

#Independent of sex
ggplot(cont_data, aes(x=factor(sex), y=age)) + 
  geom_boxplot() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

#Mean independent of dzclass
#Note higher ranges of age for dzclass=2 + non-constant variances between classes 
ggplot(cont_data, aes(x=factor(dzclass), y=age)) + 
  geom_boxplot() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

#Slight upward trend, less datapoints for num_co > 4
ggplot(cont_data, aes(x=factor(num_co), y=age)) + 
  geom_boxplot() + geom_smooth(method="loess")

#Slight downward trend, most values at edu=12: high school degree
#Some older people had to start work at earlier age
#People age < 25 might still be in school
ggplot(cont_data, aes(x=factor(edu), y=age)) + 
  geom_boxplot() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

#Downward trend but not clearly linear due to huge variation totcst
ggplot(cont_data, aes(y=totcst, x=age)) + 
  geom_point() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

#Downward trend, linear approximation better suited
ggplot(cont_data, aes(y=log(totcst+1), x=age)) + 
  geom_point() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

c("age", "sex", "dzclass", "num_co", "edu", "totcst", "logcst")
#All combinations of sex
#No influence expected

ggplot(cont_data, aes(x=dzclass, fill=factor(sex))) + 
  geom_bar(position="dodge")

ggplot(cont_data, aes(x=num_co, fill=factor(sex))) + 
  geom_bar(position="dodge")

ggplot(cont_data, aes(x=edu, fill=factor(sex))) + 
  geom_bar(position="dodge")

#num_co against totcst
ggplot(cont_data, aes(x=factor(num_co), y=totcst)) +
  geom_boxplot()

ggplot(cont_data, aes(x=factor(num_co), y=log(totcst))) +
  geom_boxplot() + geom_smooth(method="lm")

#edu against totcst
#No clear trend
ggplot(cont_data, aes(y=totcst, x=edu)) + 
  geom_point() + geom_smooth(method="loess") + geom_smooth(method="lm", color="red")

#disease class against total cost
#totcst dependent of disease class
ggplot(cont_data, aes(x=factor(dzclass), y=log(totcst+1))) + 
  geom_boxplot() + geom_smooth(method="lm", aes(group=-1))
#For better linear regression change labels 2 and 3
cont_data$dzclass_new <- cont_data$dzclass
cont_data$dzclass_new[cont_data$dzclass == 2] <- 3
cont_data$dzclass_new[cont_data$dzclass == 3] <- 2

ggplot(cont_data, aes(x=factor(dzclass_new), y=log(totcst+1))) + 
  geom_boxplot() + geom_smooth(method="lm", aes(group=-1))

#Correlations of new variables
#Correlation od dzclass_new and logcst is high
pred <- c("age", "sex", "dzclass_new", "num_co", "edu", "logcst")
cormat <- round(cor(cont_data[, pred], use="pairwise.complete.obs"), 2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

#Plot of correlation matrix
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