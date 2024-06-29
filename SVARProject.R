library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

#Loading the Dataset
macro <- read_csv(file.choose)
head(macro)
clear

#Creating thee Time Series Objectives
#converting each variable to time series objects
#all datapoints start at 2000
y <- ts(macro$`Output Gap`, start = c(2000,1,1), frequency = 4)
pi <- ts(macro$CPI, start = c(2000,1,1), frequency = 4)
r <- ts(macro$RRP, start = c(2000,1,1), frequency = 4)

#Time Series Plots
ts_plot(y, title = "Output Gap", Xtitle = "Time", Ytitle = "Output Gap")
ts_plot(pi, title = "Inflation Rate", Xtitle = "Time", Ytitle = "Inflation Rate")
ts_plot(r, title = "Overnight Reverse Repurchase Rate", Xtitle = "Time", Ytitle = "RRP")

#Setting the Restrictions
#create a value for matrix representing the values of contemporaenous correlations affecting the variables in the system
#create an identity matrix that contains three variables
amat <- diag(3)
amat 
#[,1] stands for output gap, [,2] stands for inflationary gap, [,3] stands for RRP is the policy rate 
amat[2,1] <- NA
amat[3,1] <- NA
amat[3,2] <- NA
amat
#the restrictions we impose are based on economic intuition
#the behaviour of the policy rate is attributable to shocks in inflation and output gap
#the policy rate only affects the inflation rate and the output gap with a lag (does not affect in same period)
#the elements in the lower triangle are an ace. In essence, the coefficients are left free to be estimated. 
#the elements in the upper triangle are restrictions, the output gap can affect both inflation rate and interest rate in the same period
#the inflation rate can only affect the interest rate and the interest rate cannot affect the inflation rate or the output rate in the same period


#Building the Model
sv <- cbind(y, pi, r)
colnames(sv) <- cbind("OutputGap", "Inflation", "RRP")

lagselect <- VARselect(sv, lag.max = 8, type = "both")
lagselect$selection
lagselect$criteria


Model1 <- VAR(sv, p = 5, season = NULL, exog = NULL, type = "const")
SVARMod1 <- SVAR(Model1, Amat = amat, Bmat = NULL, hessian = TRUE, estmethod =
                   c("scoring", "direct"))
SVARMod1

#Impulse Response Functions

SVARog <- irf(SVARMod1, impulse = "OutputGap", response = "OutputGap")
SVARog
plot(SVARog)
#at period 1, the output gap quite increase a bit, falls a bit, goes up etc. 
#initially, there is a huge spike in the output gap and that is expected because the initial reaction 
#to a positive shock would be an increase in the output gap which suggests an increase in inflationary gap

SVARinf <- irf(SVARMod1, impulse = "OutputGap", response = "Inflation")
SVARinf
plot(SVARinf)

#because of inflationary gap, the inflation increase but after a while it decrease after 6 quarters gap 

SVARrrp <- irf(SVARMod1, impulse = "Inflation", response = "RRP")
SVARrrp
plot(SVARrrp)

#we also see an increase in policy rate
#the IMR are quite intuitive in stroytelling!
#we increase the output gap, it is expected that the output gap will increase the inflationary gap
#this means that the economy is producing more than it was potentially expected to
#Which may cause the economy to overheating. This scenario of economy overheating will push inflation upward as productivity continues to increase
#Since inflation goes up, the CB sees that the output gap and that inflation increase, its mandate is to control inflation
#what CB will do is try to limit inflation, thereby stopping the economy from overheating 
#and that is why RRF goes up as well, as a monetary policy response by a CB


#Forecast Error Variance Decomposition
SVARfevd <- fevd(SVARMod1, n.ahead = 10)
SVARfevd
plot(SVARfevd)

#
