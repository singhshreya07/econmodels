#LOADING THE REQUIRED PACKAGE
library(tidyverse)

#LOADING THE DATASET
data = read_csv(file.choose())
head(data)
tail(data)

#DROPPING THE  NAs
data <-data%>%
  drop_na()

#PLOTTING SCATTER PLOT
ggplot(data) +geom_point(mapping =aes(x=lnGDPPC, y=lnConsumption))
#there is a positive relationship between the two as expected
#this is because countries with higher GDP per capita usually have greater consumption levels 

ggplot(data) + geom_point(mapping = aes(x = lnGDPPC, y = lnConsumption, color =
                                          IncomeGroup))
ggplot(data) + geom_point(mapping = aes(x = lnGDPPC, y = lnConsumption, shape =
                                          IncomeGroup))

#ADDING A TREND LINE
ggplot(data) + geom_point(mapping = aes(x = lnGDPPC, y = lnConsumption, color =
                                          IncomeGroup)) + geom_abline()
#this graph shows a positive slope indicating a positive relationship between log of GDP per capita and log of consumption


#SEGMENTING FURTHER BY REGION
ggplot(data) + geom_point(mapping = aes(x = lnGDPPC, y = lnConsumption, color =
                                          IncomeGroup, shape = Region)) + geom_abline()


#MENTIONING LABELS
graph <- ggplot(data) + geom_point(mapping = aes(x = lnGDPPC, y = lnConsumption, color =
                                                   IncomeGroup, shape = Region)) + geom_abline()
print(graph + ggtitle("Income and Welfare") + labs(y="Log GDP per capita, 2017", x = "Log
Consumption per capita, 2017") + labs(colour = "Income Group"))


#EXTRAS (LABELLING EACH DATA POINT WITH COUNTRY CODE/NAME)

extra <- ggplot(data, aes(lnConsumption, lnGDPPC, label = CC, color = IncomeGroup)) +
  geom_text(size = 2, hjust = 0, nudge_x = 0.033) + geom_point() + geom_abline()
print(extra + ggtitle("Income and Welfare") + labs(y="Log Consumption per capita, 2017", x =
                                                     "Log GDP per capita, 2017") + labs(colour = "Income Group")) + theme(legend.position =
                                                                                                                            c(0.87,0.2))
