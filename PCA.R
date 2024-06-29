# PCA
# A form of dimension reduction. - Transforms our data set to one with less features
# Maximizes Variance while minimizing.
# PCA is a statistical procedure that uses Eigendecompositions (finding eigenvalues/eigenvectors)
# to convert a set of observations to a set of linearly uncorrelated variables. (can use SVD - a different method)
# Each of these linearly uncorrelated variables (features) are known as Principal components.


####Read in data from csv file.
# make the directory the one with the data 
library(readr)
PCA <- read_csv("Ecotrix Projects/PCA.csv")

#selecting data
datapc <- PCA[, c(2, 3, 4, 5)]

# Neat Package (princomp)
pc.data <- princomp(datapc, cor=TRUE)

#Information Output
names(pc.data)

# Quick Summary
summary(pc.data)

#Eigenvalues/Eigenvectors
eigenvectors <- pc.data$loadings#note: these values are scaled so the SS = 1
eigenvalues <- pc.data$sdev *pc.data$sdev

#creating a correlation matrix
cor(PCA[,2:5], pc.data$scores)
round( cor(PCA[,2:5], pc.data$scores) , 3) #make things a bit nicer


#SCREE PLOT, number of components we will be using 
#we will be looking at an elbow of the graph to identify the PC we shall be using
screeplot(pc.data, type ="lines" , main = "Screenplot for the Data")
abline(1,0, col = 'red', lty = 2)

#PLOTTING A SCATTER PLOT
#type=n turns off the default symbol

plot(pc.data$scores[,2:3], type='n',xlab="PC1", ylab="PC2" )
points(pc.data$scores[,2:3],cex=0.5) #puts a small point at the centre

# create a new plot window
#windows()
# type = n turns off the default symbol
#---------------------------------
#another way to code this analysis

pc.fit <- prcomp(~real_gdp_growth + psei + bsp_rrp +   unem,
                 data=PCA,   scale=TRUE)
#we do not have any dependent variable to reduce on and we are just trying to minimise the amount of features we have
eigenvalues <- pc.fit$sdev * pc.fit$sdev
eigenvectors <- pc.fit$rotation
eigenvalues
round(eigenvectors,3)
summary(pc.fit)

#-------------------------
#THIRD WAY TO CODE THIS ANALYSIS
#fit using factor analysis function
#two factor solution - useful for printing when p is large

library(psych)

#Factor Analysis - Principle Component Method#
#Types of rotation - varimaz, promax, quartimax, simplimax, oblimin, clusterm 
pc2 <- principal(datapc, nfactors = 2, rotate="none")
pc2

# Prints out the Correlation matrix with the corresponding factors.