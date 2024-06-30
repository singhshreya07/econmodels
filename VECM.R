#INSTALLING PACKAGES
install.packages("tsDyn")
library(tsDyn)
library(vars)

########################################
#JOHANSEN COINTEGRATION in R
########################################

#Calling the packages for use

library(urca)
library(forecast)
library(tidyverse)


#LOADING THE DATASET
data <- read.csv(file.choose())
head(data)

#DECLARE THE TIME SERIES OBJECT
GDP <- ts(data$lnGDP, start = c(2003,1,31), frequency = 4)
CPI <- ts(data$lnCPI, start = c(2003,1,31), frequency = 4)
M3 <- ts(data$lnM3, start = c(2003,1,31), frequency = 4)

#CREATING OUR SYSTEM
dset <- cbind(GDP,CPI,M3)

#SELECTING THE OPTIMAL NUMBER OF LAGS (Recall, this is p - 1)

lagselect <- VARselect(dset, lag.max = 7, type = "const")
lagselect$selection
lagselect$criteria
#Since 5 came up the most, we use (5-1) or 4 lags

ctest1t <- ca.jo(dset, type = "trace", ecdet = "const", K = 4)
summary(ctest1t)

ctest1e <- ca.jo(dset, type = "eigen", ecdet = "const", K = 4)
summary(ctest1e)

#Hence, we have one cointegrating relationship in this model

######################################################################


#BUILD THE VECM MODEL

Model1 <- VECM(dset, 4, r = 1, estim =("2OLS"))
summary(Model1)
#there exists a negative relationship with GDP and CPI
#there exists a positive relationship with GDP and Money Supply M3
#the results may seem counter-intuitive due to ordering of variables

#CPI does not have short run relationship with GDP as its coefficient is not significant
#But we can see that the ECT of CPI is significant which indicates it has a long-run relationship with GDP


####Diagnostic Tests#####

#NEED TO TRANSFORM VECM INTO VAR

Model1VAR <- vec2var(ctest1t, r = 1) #r stands for number of cointegrating relationships

#Serial Correlation
Serial1 <- serial.test(Model1VAR, lags.pt = 5, type = "PT.asymptotic")
Serial1
#we fail this test for autocorrelation


#ARCH Effects/Heteroscedacity
Arch1 <- arch.test(Model1VAR, lags.multi = 15, multivariate.only = TRUE)
Arch1
#there is no ARCH effects as p value is 1


#Normality of Residuals
Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1
#the residuals are not normally distributed


#IMPULSE RESPONSE FUNCTIONS
#we see how GDP affects M3 and CPI, vice versa

M3irf <- irf(Model1VAR, impulse = "GDP", response = "M3", n.ahead = 20, boot = TRUE)
plot(M3irf, ylab = "M3", main = "GDP's shock to M3")
#In general, it is a small impact on M3, if GDP increases M3 should increase

CPIirf <- irf(Model1VAR, impulse = "GDP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "GDP's shock to CPI")
#we see CPI is gradually picking up with time to a GDP shock

GDPirf <- irf(Model1VAR, impulse = "GDP", response = "GDP", n.ahead = 20, boot = TRUE)
plot(GDPirf, ylab = "GDP", main = "GDP's shock to GDP")
#oscillating impact and has some sort of seasonality/cyclicality


#VARIANCE DECOMPOSITION
FEVD1 <- fevd(Model1VAR, n.ahead = 10)
plot(FEVD1)
#the GDP is increasingly explained by CPI
#the effect of CPI on GDP is deterring little bit with time
#for M3, we see increasing and decreasing effects

