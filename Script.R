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


