#Author: Yichn Ren
#Date: 2020-10-28

data=read.csv("Data.CSV", sep=",", dec=".", header = T)
data[675,]
#risk free return
rf = mean(data[,8])
#we take the mean value of rf so the variety of rf will not influence our model assessment
rf 
#returns on 6 portfolios
r = data[,2:7]
#excess return of 6 portfolios
e_r = r - rf
#all candidate fatctors
fac = data[,9:14]
factor_names <- c("MKT", "SMB", "HML", "RMW", "CMA", "MOM")

#choose one portfolio to build the model
y = e_r[,3]

#First, check the correlation--the strength of the linear association between
#the excaess return of the portfolio(dependent variables) and the factors(independent variables)
round(cor(fac, y), 4)
#Since none of the correlation is close to 0, there are linear relationships between factors
# and the excaess return of the portfolio (some relationships are not strong though)

#According to previous analysis, we notice there are 6 factors that has linear relationship
#with the dependent variable, which is the excess return of the portfolio
#If two or more explanatory variables have a linear relationship with the dependent variable,
#the regression is called a multiple linear regression.
#Remember it is better to use adjusted R^2 to evaluate the performance of the model 
# because when any variable is added to the model, R^2 increases.
# However, we prefer simplest best model.
# If the added variable does not really provide any new information, adjusted R^2 will not increase

# Two predictor variables are said to be collinear when they are correlated,
#and this collinearity complicates model estimation
#Plot matrix
library('GGally')
ggpairs(fac[,factor_names], axisLabels = "internal")
#we notice HML and CMA has a high correlation 0.694
reg = lm(y ~ fac$HML+fac$CMA+fac$MKT+fac$RMW+fac$SMB+fac$MOM)
library(car)
vif(reg)
#A VIF measures the extent to which multicollinearity has increased the variance of an
#estimated coefficient
#All the values are smaller than 5, so the multicollinearity problem is not severe
#we do not need to drop the one of them

#We are trying to build the best model with the 6 factors, 
# so we will try all the combinations

#Two list to keep track of factors sets and the corresponding Adjusted R^2
factor_sets <- list()
adj_r2 <- list()

# One factor:
for (i in 1:6) {
  reg = lm(y~fac[,factor_names[i]])
  print(paste("Factor:",factor_names[i]))
  beta = reg$coefficients
  print(beta)
  print(paste("Residual standard error:",summary(reg)[[6]]))
  print(paste("Multiple R-squared:",summary(reg)[[8]]))
  print(paste("Adjusted R-squared:",summary(reg)[[9]]))
  cat("\n")
  factor_sets <- append(factor_sets, factor_names[i])
  adj_r2 <- append(adj_r2, summary(reg)[[9]])
}


#Two factors:
library('gtools')
coms_2 = combinations(6, 2, factor_names)
coms_2
for (i in 1:length(coms_2[,1])) {
  reg = lm(y~fac[,coms_2[i,][1]]+fac[,coms_2[i,][2]])
  cat("Factor:", coms_2[i,])
  cat("\n")
  beta = reg$coefficients
  print(beta)
  print(paste("Residual standard error:",summary(reg)[[6]]))
  print(paste("Multiple R-squared:",summary(reg)[[8]]))
  print(paste("Adjusted R-squared:",summary(reg)[[9]]))
  cat("\n")
  factor_sets <- append(factor_sets, list(coms_2[i,]))
  adj_r2 <- append(adj_r2, summary(reg)[[9]])
}

#three factors:
coms_3 = combinations(6, 3, factor_names)
coms_3
for (i in 1:length(coms_3[,1])) {
  reg = lm(y~fac[,coms_3[i,][1]]+fac[,coms_3[i,][2]]+fac[,coms_3[i,][3]])
  cat("Factor:", coms_3[i,])
  cat("\n")
  beta = reg$coefficients
  print(beta)
  print(paste("Residual standard error:",summary(reg)[[6]]))
  print(paste("Multiple R-squared:",summary(reg)[[8]]))
  print(paste("Adjusted R-squared:",summary(reg)[[9]]))
  cat("\n")
  factor_sets <- append(factor_sets, list(coms_3[i,]))
  adj_r2 <- append(adj_r2, summary(reg)[[9]])
}

#four factors:
coms_4 = combinations(6, 4, factor_names)
coms_4
for (i in 1:length(coms_4[,1])) {
  reg = lm(y~fac[,coms_4[i,][1]]+fac[,coms_4[i,][2]]+fac[,coms_4[i,][3]]+fac[,coms_4[i,][4]])
  cat("Factor:", coms_4[i,])
  cat("\n")
  beta = reg$coefficients
  print(beta)
  print(paste("Residual standard error:",summary(reg)[[6]]))
  print(paste("Multiple R-squared:",summary(reg)[[8]]))
  print(paste("Adjusted R-squared:",summary(reg)[[9]]))
  cat("\n")
  factor_sets <- append(factor_sets, list(coms_4[i,]))
  adj_r2 <- append(adj_r2, summary(reg)[[9]])
}

#five factors:
coms_5 = combinations(6, 5, factor_names)
coms_5
for (i in 1:length(coms_5[,1])) {
  reg = lm(y~fac[,coms_5[i,][1]]+fac[,coms_5[i,][2]]+fac[,coms_5[i,][3]]+fac[,coms_5[i,][4]]+fac[,coms_5[i,][5]])
  cat("Factor:", coms_5[i,])
  cat("\n")
  beta = reg$coefficients
  print(beta)
  print(paste("Residual standard error:",summary(reg)[[6]]))
  print(paste("Multiple R-squared:",summary(reg)[[8]]))
  print(paste("Adjusted R-squared:",summary(reg)[[9]]))
  cat("\n")
  factor_sets <- append(factor_sets, list(coms_5[i,]))
  adj_r2 <- append(adj_r2, summary(reg)[[9]])
}

#six factors:
reg = lm(y~fac[,1]+fac[,2]+fac[,3]+fac[,4]+fac[,5]+fac[,6])
cat("Factor:", factor_names)
beta = reg$coefficients
print(beta)
print(paste("Residual standard error:",summary(reg)[[6]]))
print(paste("Multiple R-squared:",summary(reg)[[8]]))
print(paste("Adjusted R-squared:",summary(reg)[[9]]))
factor_sets <- append(factor_sets, list(factor_names))
adj_r2 <- append(adj_r2, summary(reg)[[9]])


#Display all the combinations factors and their corresponding Adjusted_R^2
fac_df = do.call("rbind", factor_sets)
names(fac_df)[1] = "Factors"
r2_df = do.call(rbind.data.frame, adj_r2)
names(r2_df)[1] = "Adjusted_R^2"
num = c(rep(1, 6),rep(2, 15), rep(3, 20), rep(4, 15), rep(5, 6), 6)
num
comparison_df <- cbind(fac_df, r2_df, num)
comparison_df
#R will auto fill NA cells by repeating front cells
#Our true factors sets need remove duplications.
comparison_df[order(comparison_df['Adjusted_R^2']),]

#Model has the best performance when it use all the six factors as independent variables
#However, we prefer simplest best model
#Adjusted_R^2 does not improve much from using only three factors(HML, MKT, SMB) to six factors.
#In reality, to reduce the complexity of the model, we should choose MKT, SMB, HML
#to build the model and predict the excess return of the portfolio

#Extra model diagnostics
#fit a regrSession line and plot the fit
fit <- lm(y ~ fac$MKT+fac$SMB+fac$HML)
summary(fit)
#Plot the residuals plot
#Residuals are leftovers from the model fit: Data = Fit + Residual
#which mean residual is the difference between observed y and predicted y
#lm() performs least square regression
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")
plot(fit$residuals)
abline(0, 0, col="red")
#Since the variability of residuals around 0 line is roughly constant
# We can assume homoskedasticity
# uncomment below to see all diagnostic plots of the regression
# plot(fit)

# Points with high residual and high 
# leverage are influential points that need to be removed
# They will skew the model fit away from the rest of the data
#check if there are influencial points by calculating Cook’s Distance
#investigate any point over 4/n, where n is the number of observations
plot(fit, 4)
cook_df = cooks.distance(fit) > 4/675
cook_df[cook_df==TRUE]

#drop influencial points and build the same regression again
fit <- lm(y[cook_df==FALSE] ~ fac$MKT[cook_df==FALSE]+fac$SMB[cook_df==FALSE]+fac$HML[cook_df==FALSE])
summary(fit)
print(paste("Adjusted R-squared:",summary(fit)[[9]]))
#As we can see adjusted R^2 increase from 0.9818 to 0.9859
#Our model now outforms all the other models

#linearity
plot(fit, 1)
#the residual plot will show no fitted pattern. 
#That is, the red line should be approximately horizontal at zero

#homoscedasticity
#It’s good if you see a horizontal line with equally spread points. 
plot(fit, 3)

#There are still influential points, but we our dataset is not enough to keep dropping
(2*(6 + 1))/(675-45)
plot(fit, 5)

#independence of the errors
plot(fit$residuals)
abline(0, 0, col="red")


#test for normality
#If the data are normally distributed, the data points will be close to the diagonal line. 
#If the data points stray from the line in an obvious non-linear fashion, the data are not normally distributed.
qqnorm(fit$residuals)
qqline(fit$residuals, col="red")

#all the diagnostic plots seem really good
# uncomment below to see all diagnostic plots of the regression
# plot(fit)

#Shapiro-Wilk fails to reject for a low alpha
shapiro.test(resid(fit))








