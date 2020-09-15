#R SCRIPT - DIAMONDS PRICE PREDICTION REGRESSION MODEL 
#Upload diamonds Excel file ####
library(readxl)
diamonds <- read_excel("Desktop/MAT 252/Mini-projects/diamonds.xlsx")
View(diamonds)

#DATA ANALYSIS ####
#change character columns to factors ####
diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

#COR TEST - CARAT
#fits price extremely well (R is .91 and R^2 is .84)
#slightly larger spread once carat > 1.5
cor(diamonds$price, diamonds$carat)
summary(lm(diamonds$price~diamonds$carat))  
plot(diamonds$carat, diamonds$price)

#COR TEST - X, Y, Z (numerical data)
#based on pairs, all three have over .97 correlation and could lead to overfitting
#do not use in model
pairs(diamonds$carat~diamonds$depth+diamonds$table+diamonds$x+diamonds$y+diamonds$z)
cor(diamonds$carat,diamonds$x) 
cor(diamonds$carat,diamonds$y)
cor(diamonds$carat,diamonds$z)

cor(diamonds$price, diamonds$carat)

#COR TEST - DEPTH, TABLE
#low correlations - not including in model 
cor(diamonds$price, diamonds$table) #.14846
cor(diamonds$price, diamonds$depth) #.0259

#COR TEST - FACTOR COLUMNS (CUT, COLOR, CLARITY)
summary(lm(diamonds$price~diamonds$carat+diamonds$cut+diamonds$color+diamonds$clarity))
#R-squared is .9111!
#however - all three are variables with extreme biases and will not be included in model 

#most appropriate case is to use only carat for determining price ####

plot(lm(diamonds$price~diamonds$carat))

#After plotting - the residual curve is concave upward suggesting re-expressing! 

#plotting the logs of the two columns gives us a nice qq-plot and residuals
#summary gives us a .925 R^2
plot(lm(log(diamonds$price)~log(diamonds$carat)))
summary(lm(log(diamonds$price)~ log(diamonds$carat)))

#rename the log expressions for easier predictions, then save object
a  <- log(diamonds$price)
b <- log(diamonds$carat)
lm_final <- (lm(a~b))
plot(lm_final)

#log(price) = 8.434 + 1.651 log (Carat)

#predicting model based off the logs of the fivenum, then using exp to return x 
pred_model <- predict(lm_final, newdata = data.frame(b = fivenum(b)), interval='prediction', level=.95)
pred_model
exp(pred_model)

#compare exp(pred_model) to actual diamonds$price data
fivenum(diamonds$price)

boxplot(exp(pred_model), fivenum(diamonds$price))

#the median is slightly higher in our prediction, but median is pretty close! 


