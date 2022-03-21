
setwd("C:/Users/Giri/Desktop/Auxin_data") #sets the working directory

#tell where the data comes from
dir("C:/Users/Giri/Desktop/Auxin_data") #Read the mentioned directory
auxin <-read.csv(file="auxin_data.csv", header=TRUE, sep=",") # Read the specified file

summary(auxin) #statistical summary of the data by column
names(auxin) #gives the column information/list data categories


#some hands on (for creating strip chart)
stripchart(auxin$SAMPLE)
stripchart(auxin$CPM_3H)
stripchart(auxin$DPM, method="stack")

#specify the factors in the data file
POPULATION <- factor(auxin$POPULATION)
Individual <- factor(auxin$Individual)


#histogram distribution for data on a particular column
hist(auxin$DPM, main="Distributions of DPM (both population pooled)", xlab="Disintegrations per minute (DPM)")

#Scatter Plot
plot(DPM~SAMPLE, data=auxin, main="Scatter plot: DPM (MY+SP)")

#box plot for a particular column
boxplot(auxin$DPM, data=auxin, main="Histogram for DPM")
means <- tapply(auxin$DPM, auxin$POPULATION, mean)
points(means, col="red", pch=18)

boxplot(DPM~POPULATION, data=auxin, main="DPM by Population") # box plot for data by a particular factor (population)
means <- tapply(auxin$DPM, auxin$POPULATION, mean) #locates mean position in the distribution
points(means, col="red", pch=18) #plots the mean with red colour with size 18

means <-(DPM~POPULATION)

#calculates the mean DPM for each population category
aggregate(DPM~POPULATION, data=auxin, FUN=mean)

#calculates the mean DPM for each individual category
aggregate(DPM~Individual, data=auxin, FUN=mean)

#calculates the mean DPM for each population+individual category
aggregate(DPM~POPULATION+Individual, data=auxin, FUN=mean)


boxplot(auxin$DPM~Individual, main="DPM by Individual") # box plot for data by a particular factor (Individual)
means <- tapply(auxin$DPM, auxin$Individual, mean)
points(means, col="red", pch=18)

# x/y correlation plot for a particular category
plot(auxin$DPM,auxin$CPM_3H)

#Check for normal Distribution
qqnorm(auxin$DPM) # creates a quantile plot
qqline(auxin$DPM) # creates a line on the quantile plot


var.test(auxin$DPM~auxin$POPULATION, alternative = "two.sided", conf.level = 0.95)
#OR:
var.test(DPM~POPULATION, data=auxin, alternative = "two.sided", conf.level = 0.95)

#ANOVA test for population level difference for auxin transport or/ DPM
fit <- aov(DPM~POPULATION, data=auxin) 
summary(fit)

#ANOVA test for individual level difference for auxin transport or/ DPM
fitID <- aov(DPM~Individual, data=auxin) 
summary(fitID)

#ANOVA test for population level difference (with individuals nested within population)
fit2 <- aov(DPM~POPULATION/Individual, data=auxin)
summary(fit2)



#Linear Regression for population level effects:
fit_pop <- lm(auxin$DPM~auxin$POPULATION)
summary(fit_pop)

#Linear Regression for Individual level effects:
fit_ID <- lm(auxin$DPM~auxin$Individual)
#gives summary of fit and p-values with first variable (MY1) as intercept
#the significace of difference for other individuals are calculated with reference to intercept (MY1)
summary(fit_ID)


#Linear regression for Population (with individuals nested) level effects:
#******Please clarify the results***********#
fit_popID <- lm(DPM~POPULATION/Individual, data=auxin)
summary(fit_popID)


#linear regression with 0 as an intercept
fit_IDm1 <- lm(auxin$DPM~auxin$Individual-1) #gives summary fit and p-values with 0 as intercept
#the significane of difference for other individuals are caculate with reference to intercept=0
summary(fit_IDm1)

#ANOVA summary for linear regression modeling

#first put code for combining multiple plots, par(mfrow=c(nrow, ncol))
opar <- par(mfrow=c(2,2), oma=c(0,0,1.1,0))

anova(fit_pop)
plot(fit_pop, las=1)

plot(fit_ID, las=1)
anova(fit_ID)

plot(fit_popID, las=1)
anova(fit_popID)


###*****************************###
deviance(fit_ID, fit_popID)
step(fit_byID)
###*****************************###



#****************************************************************
#Since the observation (MY2 sample #7) has an unexpected outcome
#Further analysis will be done by removing this outlier

auxin_trunc <- auxin[c(1:6,8:36),] #only selects data on row: 1 thru 6, and 8 thru 36

summary(auxin_trunc) #statistical summary of the data by column
names(auxin_trunc) #gives the column information/list data categories


#some hands on (for creating strip chart)
stripchart(auxin_trunc$SAMPLE)
stripchart(auxin_trunc$CPM_3H)
stripchart(auxin_trunc$DPM, method="stack")

#specify the factors in the data file
POPULATION_T <- factor(auxin$POPULATION)
Individual_T <- factor(auxin$Individual)


#histogram distribution for data on a particular column
hist(auxin_trunc$DPM, main="DPM (both population pooled) - outlier removed", xlab="Disintegrations per minute (DPM)")

#Scatter Plot
plot(DPM~SAMPLE, data=auxin_trunc, main="Scatter plot: DPM (MY+SP)")

#box plot for a particular column
boxplot(auxin$DPM, data=auxin_trunc, main="BoxPlot for DPM")
means <- tapply(auxin$DPM, auxin$POPULATION, mean)
points(means, col="red", pch=18)

boxplot(DPM~POPULATION, data=auxin_trunc, main="DPM by Population") # box plot for data by a particular factor (population)
means <- tapply(auxin$DPM, auxin$POPULATION, mean) #locates mean position in the distribution
points(means, col="red", pch=18) #plots the mean with red colour with size 18

boxplot(DPM~Individual, data = auxin_trunc, main="DPM by Individual") # box plot for data by a particular factor (Individual)
means <- tapply(auxin$DPM, auxin$Individual, mean)
points(means, col="red", pch=18)


# x/y correlation plot for a particular category
plot(auxin$DPM,auxin$CPM_3H)

#Check for normal Distribution
qqnorm(auxin$DPM) # creates a quantile plot
qqline(auxin$DPM) # creates a line on the quantile plot


var.test(auxin$DPM~auxin$POPULATION, alternative = "two.sided", conf.level = 0.95)
#OR:
var.test(DPM~POPULATION, data=auxin_trunc, alternative = "two.sided", conf.level = 0.95)

#ANOVA test for population level difference for auxin transport or/ DPM
fitnew <- aov(DPM~POPULATION, data=auxin_trunc) 
summary(fitnew)

#ANOVA test for individual level difference for auxin transport or/ DPM
fitnewID <- aov(DPM~Individual, data=auxin_trunc) 
summary(fitnewID)

#ANOVA test for population level difference (with individuals nested within population)
fitnewPOPid <- aov(DPM~POPULATION/Individual, data=auxin_trunc)
summary(fitnewPOPid)



####Linear Regression for population level effects:
fit_popnew <- lm(auxin$DPM~auxin$POPULATION)
summary(fit_popnew)

#Linear Regression for Individual level effects:
fit_IDnew <- lm(auxin$DPM~auxin$Individual)
#gives summary of fit and p-values with first variable (MY1) as intercept
#the significace of difference for other individuals are calculated with reference to intercept (MY1)
summary(fit_IDnew)


#Linear regression for Population (with individuals nested) level effects:
#******Please clarify the results***********#
fit_popIDnew <- lm(DPM~POPULATION/Individual, data=auxin_trunc)
summary(fit_popIDnew)


#linear regression with 0 as an intercept
fit_IDm1new <- lm(auxin$DPM~auxin$Individual-1) #gives summary fit and p-values with 0 as intercept
#the significane of difference for other individuals are caculate with reference to intercept=0
summary(fit_IDm1new)

#ANOVA summary for linear regression modeling

#first put code for combining multiple plots, par(mfrow=c(nrow, ncol))
opar <- par(mfrow=c(2,2), oma=c(0,0,1.1,0))

anova(fit_popnew)
plot(fit_popnew, las=1)

plot(fit_IDnew, las=1)
anova(fit_IDnew)

plot(fit_popIDnew, las=1)
anova(fit_popIDnew)


#### *** Data may be typed and analyzed *** #### but always good to create and read a file
# data from Auxin transport (DPM values for a=mayodan popn & b=Spiterstulen population)

a<- c(9515.15, 13025.76, 7634.05, 18045.6, 14576.01, 8749.81, 10515.96, 7690.5, 8356.31, 6127.58,
      6823.86, 12670.45, 5326.98, 18805.09, 8336.29, 8145.97, 6029.39)

b<- c(3633.05, 4110.97, 11757.36, 5122.64, 6600.33, 4916.23, 10586.33, 4305.08, 11396.72, 5474.96, 4702.98,
      3312.76, 14455.48, 6786.02, 12196.6, 8023.17, 9993.54, 9494.12)

var.test(a,b, alternative = "two.sided", conf.level = 0.95)
plot(a)
plot(b)
hist(a)       # gives histogram distribution of a
hist(b)       # gives histogram distribution of b
var.test(a,b) # test for the equality of the variances

t.test(a,b, var.equal=FALSE, paired=FALSE) # t-test assuming unequal variances
t.test(a,b, var.equal=TRUE, paired=FALSE)  # t-test assuming equal variances


# data for individual samples
m1<-c(9515.15, 13025.76, 7634.05, 18045.6, 14576.01, 8749.81)
m2<-c(10515.96, 7690.5, 8356.31, 6127.58, 6823.86)
m3<-c(12670.45, 5326.98, 18805.09, 8336.29, 8145.97, 6029.39)
s1<-c(3633.05, 4110.97, 11757.36, 5122.64, 6600.33, 4916.23)
s2<-c(10586.33, 4305.08, 11396.72, 5474.96, 4702.98, 3312.76)
s3<-c(14455.48, 6786.02, 12196.6, 8023.17, 9993.54, 9494.12)

boxplot(m1, m2, m3, s1, s2, s3)

oneway.test(m1, m2, m3, s1, s2, s3) #script not working
