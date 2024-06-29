#loading packages to run VAR
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)

#load the dataset
datafile <- read.csv(file.choose())

#plotting simple graph
ggplot(data = datafile) + geom_point(mapping = aes(x = unem, y = real_gdp_growth))

#the graph is relatively scattered. 
#when the unemployment is low, the real gdp growth is relatively high generally
#when the unemployment is high, real gp growth tends to be lower
#there is a negative relationship between gdp and unemployment rate
#This is because there is productivity loss or productivity rhat is underutilized that bolsters GDP growth


#DECLARE OUR TIME SERIES VARIABLES
gdp <- ts(datafile$real_gdp_growth, start = c(1999,3), frequency = 4)
unem <- ts(datafile$unem, start = c(1999,3), frequency = 4)

#PLOTTING THE SERIES
autoplot(cbind(gdp,unem))

#OLS
ols1 <- lm(gdp ~ unem)
summary(ols1)
#we see that unemployment affects gdp growth but gdp cannot affect unemployment because gdp is a dependent variable
#the train of causality is from unemployment is to GDP
 

#DETERMINE THE PERSISTENCE OF THE MODEL VIA ACF/PACF
acf(gdp, main = 'ACF for Real GDP Growth')
#we see that the first few lags are statistically signficant but it disappears quickly
pacf(gdp, main = 'PACF for Real GDP Growth')
#we notice that it is not that necessarily significant
acf (unem, main = 'ACF for Unemployment')
#the lags are found to be relatively significant
pacf (unem, main = 'PACF for Unemployment')

#FINDING THE OPTIMAL LAGS
datafile.bv <- cbind(gdp, unem)
colnames(datafile.bv) <- cbind("GDP", "Unemployment")
lagselect<- VARselect(datafile.bv, lag.max = 10, type = "const")
lagselect$selection
#4 is the optimal lag 

Model1 <- VAR(datafile.bv, p = 4, type = "const", season = NULL, exog = NULL)
summary(Model1)
#we noticed that all of the roots are inside the unit circle
#so we have no strenous roots, the system is generally stable


#DIAGNOSING the VAR
#Test for Serial Correlation
Serial1 <-serial.test(Model1, lags.pt = 12, type = "PT.asymptotic")
Serial1
#if the p value of this portmanteau test is greater than 0.05 
#it suggests that there is no serial correlation


#Tests for Heteroscedacitiy
Arch1 <- arch.test(Model1, lags.multi = 12, multivariate.only = TRUE)
#the model does not suffer from heteroscedacity as p value is greater than 0.05


#Normal Distribution of residuals Test
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1
#In Jarque Bera Test, since the p value is less than 0.05, the residuals are not normally-distributed
#also failed the skewness and kurtosis test

#Testing for Structural breaks in the residuals
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

#we notice that there are no points in this graph that exceeds the two red lines, the upper and lower CI
#therefore, the system is stable

#GRANGER CAUSALITY TEST
#checking more of a causality than a correlation
#we can see if GDP causes unemployment or unemployment causes GDP or both directions

GrangerGDP <- causality(Model1, cause = "GDP")
GrangerGDP
#we cannot reject null hypothesis since p value is greater than 0.05
#thus GDP does not Granger Cause unemployment

GrangerUnemployment <- causality(Model1, cause = "Unemployment")
GrangerUnemployment
#since we fail to reject the null, Unemployment does  not Granger Cause GDP
#While Okun's Law applies to most developed countries
#It does not hold for the our sample

#IMPLUSE RESPONSE FUNCTIONS (IRF)
#it sees how our variables behave n periods from now
#if we shock unemployment, say unemployment experiences a positive shock, how variables respond to shock
GDPirf <- irf(Model1, impulse = "Unemployment", reponse = "GDP", n.ahead = 20, boot = TRUE)
plot(GDPirf, ylab = "GDP", main = "Shock from Unemployment")
#if we increase unemployment (a positive shock) will decrease GDP and eventually recover in some time
#note those dotted lines are CI, we have relatively big room for air
#we see that unemployment will negatively affect GDP


Unemploymentirf <- irf(Model1, impulse = "GDP", response = "Unemployment", n.ahead = 20, boot = TRUE)
plot(Unemploymentirf, ylab = "Unemployment", main = "Shock from GDP")
#we can see indecisive relationship as unemployment goes up and then down and then up
#this is kind of reference from Granger Causality that these two variables are as not related to each other


#VARIANCE DECOMPOSITION
#create a graph that explains how much these variables are influenced by shocks
FEVD1 <- fevd(Model1, n.ahead = 10)
plot(FEVD1)
#we can see that both GDP and unemployment are relatively influenced by their own shocks
#the entire change in GDP will due to change in gdp
#similar case with unemployment 
#shocks in unemployment are typically the ones that are gonna affect unemployment
#the future values  of unemployment will heavily be influenced by future shocks in GDP


#VAR FORECASTING

forecast123 <- predict(Model1, n.ahead = 4, ci = 0.95)
fanchart(forecast123, names = "GDP")
#we notice that roughly 6.5% GDP growth in the next four quarters
fanchart(forecast123, names = "Unemployment")
#we see that unemployment tend to go downward, it becomes more gradient
#we forecast that it will go down and project towards less than 4%
