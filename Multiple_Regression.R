library(ggplot2)
library(lindia)

#Read in the csv
data <- read.csv('/Users/janetscott/Documents/Articles/Store.csv')

#View the first few lines of data
head(data)

#Create a new data frame with only the fields we are interested in.
data2 <- data[, c('revenue', 'local_tv', 'online', 'instore')]

#Create a scatter plot matrix to visualize the relationships between each of the variables. 
plot(data2)

#Fit a regression model with all variables in our data frame.
model <- lm(revenue~., data=data2)

#Display the results of the model
summary(model)

#Return a list of the residuals 
res <- resid(model)

#Create residual vs. fitted plot for the model
plot(fitted(model), res)

#Create scatter plots with residuals on the y axis and each predictor on the x axis
gg_resX(model)

#Add a horizontal line at 0 
abline(0,0)


#Customize which residual plot is included
plots <- gg_resX(model, plot.all = FALSE)

#Return name of the plots
names(plots)  

#Exclude particular residual plots
exclude_plots <- plots[-1 ]  

#Include particular residual plots
include_plots <- plots[1]   

#Plot all excluded plots
plot_all(exclude_plots) 

#Plot all included plots
plot_all(include_plots)


#Create a normal probability plot (Q-Q plot) of the residuals
qqnorm(residuals(model), main="Q-Q plot")
qqline(residuals(model))

