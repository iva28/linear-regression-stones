#loading "The Rolling Stones" dataset
dataset <- read.csv('stones_analysis.csv',stringsAsFactors = FALSE,check.names = FALSE)

#examining the structure of dataset
str(dataset)
#examining the summary of dataset
summary(dataset)       

#transforming dataset
source('Utility.R')
dataset <- transormed.dataframe(dataset = dataset)

#checking for NA values
all(complete.cases(dataset))

# Linear Regression algorithm works with numeric variables
num.vars <- which(sapply(dataset, is.numeric))
num.vars.names <- names(num.vars)
num.vars.names
# "Year Recorded"    "Year Released"    "Track number"     "Song duration"    "acousticness"     "danceability"     "energy"          
# "instrumentalness" "liveness"         "loudness"         "speechiness"      "tempo"            "valence"  are variables that are numeric
# The variable "energy" will be used as outcome variable

dataset <- dataset[,num.vars]
# checking for NA values once again
apply(dataset, 2, function(x) sum(is.na(x)))

# computing correlation matrix
corr.matrix <- cor(dataset)
library(corrplot)
# plotting the correlation matrix to see relevant predictors for variable 'energy'
corrplot.mixed(corr.matrix,tl.cex = 0.75, number.cex = 0.75)
# Predictor loudness and valence have the highest correlation with the outcome variable
# Correlation between loudness and energy is high and amounts to 0.72
# Correlation between valence and energy  is at threshold value and amounts to 0.49, but it will be imcluded in model

# plot 'loudness' against the response variable
library(ggplot2)
ggplot(data = dataset, mapping = aes(x = loudness, y = energy)) +
  geom_point(shape = 1) +
  theme_classic()
# From the plot we can see that the louder the song is the more energetic it is

# plot 'valence' against the response variable
library(ggplot2)
ggplot(data = dataset, mapping = aes(x = valence, y = energy)) +
  geom_point(shape = 1) +
  theme_classic()

# creating train and test data sets
library(caret)
# set seed
set.seed(1)
# create train and test sets
train.indices <- createDataPartition(dataset$energy, p = 0.8, list = FALSE)
train.data <- dataset[train.indices,]
test.data <- dataset[-train.indices,]

# bulding Linear Regression model
lm1 <- lm(energy ~ loudness + valence, data = train.data)
summary(lm1)
# Based on the coefficient of the loudness variable, with each unit increase in loudness, energy value increases by 0.040826 units
# Based on the coefficient of the valence variable, with each unit increase in valence, energy value increases by 0.284907 units
# Both variables( loudness and valence) are significant predictors of the response variable energy, since the coefficents that are 
# associated with these input variables are significantly different than zero

# Residual standard error represents the difference between predicted and real values and it amounts to 0.1109

# Based on the R-squared value, this model explains 65.93% of the variability in the energy value of songs
# Based on the F statistic( 228.3) and the associated p -value( < 0.05), there is a significant relationship between the
# predictors and the response variable 

# Since there are two predictor variables, the multicolinearity should be checked
library(car)
sort(sqrt(vif(lm1)))
# there is no multicolinearity

# printing the diagnostic plots
graphics.off()
par(mfrow = c(2,2))
plot(lm1)
# resetting plotting area
par(mfrow = c(1,1))
# The first plot, Residual vs Fitted value, is used for checking if the linearity assumption is satisfied.
# A pattern could show up in this plot if there is a non-linear relationship between the dependent and independent variables.
# In this case, the plot indicates a non-linear relationship between the predictors( and the response variable.

# The second plot, Normal Q-Q plot, tells us if residuals are normally distributed.
# The residuals should be lined well on the straight dashed line.
# This is the case for the lm1 model, so we can conlude that the assumption for normally distributed residuals is met

# The third plot, Scale-Location, is used for checking the assumption of the equal variance of residuals, homoscedasticity 
# In this case, the variance of the residuals tends to differ. So, the assumption is not fulfiled.

# The forth plot, Residuals vs Leverage, is used for spotting the presence of high leverage points, since their presence can seriously affect the estimation of the regression coefficients
# In this case, there aren't any such observations.

# Taken all together, the four plots indicate that our linear model( lm1), and thus its predictions, are trustworthy enough

# making predictions with model lm1
lm1.pred <- predict(lm1, newdata = test.data)
head(lm1.pred)

# To examine the predicted against the real values of the response variable (energy), 
# we can plot their distributions one against the other.
test.data.lm1 <- cbind(test.data, predicted = lm1.pred)
# plot actual (energy) vs. predicted values
library(ggplot2)
ggplot(data = test.data.lm1) +
  geom_density(mapping = aes(x=energy, color = 'real')) +
  geom_density(mapping = aes(x=predicted, color = 'predicted')) +
  scale_colour_discrete(name ="energy distribution") +
  theme_classic()
# To evaluate the predictive power of the model, we’ll compute R-squared on the test data
# calculate RSS
lm1.test.RSS <- sum((lm1.pred - test.data$energy)^2)
# calculate TSS
lm1.test.TSS <- sum((mean(train.data$energy) - test.data$energy)^2)
# calculate R-squared on the test data
lm1.test.R2 <- 1 - lm1.test.RSS/lm1.test.TSS
lm1.test.R2
# R - squared value on the test set( 59.55433%) is less than the one on the train set, whish is around 65%
# calculate RMSE
lm1.test.RMSE <- sqrt(lm1.test.RSS/nrow(test.data))
lm1.test.RMSE
# compare energy mean to the RMSE
lm1.test.RMSE/mean(test.data$energy)
# The error we are making in predictions is not small, it is around 15% of mean value

# making another model with all variables as predictors
lm2 <- lm(energy ~ ., data = train.data)
summary(lm2)
# R-squared value is better, 78.32% of the variability in the energy value of songs
# variables acousticness, danceability, loudness, speechiness and valence have been found to be significant predictors

# checking for multicolinearity
sort(sqrt(vif(lm2)))
# varibles Year Released and Year Recorded are causing multicollinearity, so we will make another model 
# First the variable Year Released will be exluded
lm3 <- lm(energy ~. -(`Year Released`), data = train.data)
# checking for multicolinearity
sort(sqrt(vif(lm3)))
# the multicollinearity issue has been solved
summary(lm3)
# R - squared value is 78.3% which is better than the lm1 model, but we will make another model consisting only of significant predictors
lm.final <- lm(energy ~ acousticness+danceability+loudness+speechiness+valence, data = train.data)
# checking for multicollinearity
sort(sqrt(vif(lm.final)))

summary(lm.final)
# R-squared and Residual standard error are both better for lm.final model in comparison to lm1 model
# R-squared value is 0.774, meaning 77,4% variability of response variable is explained  by model
# Residual standard error is 0.09024

# All predictor variables are significant predictors for response variable energy

# Based on the coefficient of the acousticness variable, with each unit increase in acousticness, energy value decreases by 0.137308 units
# Based on the coefficient of the danceability variable, with each unit increase in danceability, energy value decreases by 0.239884 units
# Based on the coefficient of the loudness variable, with each unit increase in loudness, energy value increases by 0.034714 units
# Based on the coefficient of the speechiness variable, with each unit increase in speechiness, energy value increases by 1.273658 units
# Based on the coefficient of the valence variable, with each unit increase in valence, energy value increases by 0.346245 units

# making predictions with lm.final model
lm.final.pred <- predict(lm.final, newdata = test.data)
head(lm.final.pred)

test.data.lmfinal <- cbind(test.data, predicted = lm.final.pred)
# plot actual (energy) vs. predicted values
library(ggplot2)
ggplot(data = test.data.lmfinal) +
  geom_density(mapping = aes(x=energy, color = 'real')) +
  geom_density(mapping = aes(x=predicted, color = 'predicted')) +
  scale_colour_discrete(name ="energy distribution") +
  theme_classic()

# calculating R - squared, RMSE values
lmfinal.test.RSS <- sum((lm.final.pred - test.data$energy)^2)
# TSS is the same
lmfinal.test.R2 <- 1 - lmfinal.test.RSS/lm1.test.TSS
lmfinal.test.R2
# R - squared value on test set is 75% and it is very similar to the one obtained on the training set, confirming the stability of model lm.final

# calculate RMSE
lmfinal.test.RMSE <- sqrt(lmfinal.test.RSS/nrow(test.data))
lmfinal.test.RMSE

lmfinal.test.RMSE/ mean(test.data$energy)
# The error we are making in predictions is around 12% of mean value