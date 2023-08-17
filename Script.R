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
# The variable "danceability" will be used as outcome variable