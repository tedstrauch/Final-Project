library(rcompanion)
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")

# In this R script we will be running tests for normality and simple linear regression
# I will not comment on everything like in the 2017 model for sake of time but will look at results at the end. 

plotNormalHistogram(Happy2015.2017$Happiness.Score.2015)

plotNormalHistogram(Happy2015.2017$Economy.GDP.per.Capita.2015)

# This looks fairly normally distributed as well. 

plotNormalHistogram(Happy2015.2017$Child_Mortality_2015)
Happy2015.2017$Child_Mortality_2015TUK <- transformTukey(Happy2015.2017$Child_Mortality_2015, plotit = FALSE)
plotNormalHistogram(Happy2015.2017$Child_Mortality_2015TUK)

# Now lets model with Linear regression
# The question is: Does the GDP per capita impact the child mortality rate? 
# Test Assumptions
# Testing for Linearity by looking at a scatterplot. 

scatter.smooth(x=Happy2015.2017$Economy.GDP.per.Capita.2015, y=Happy2015.2017$Child_Mortality_2015TUK, main="GDP impact on Child Mortality Rate 2015")
lmMod <- lm(Child_Mortality_2015TUK~Economy.GDP.per.Capita.2015, data=Happy2015.2017)
par(mfrow=c(2,2))
plot(lmMod)
lmtest::bptest(lmMod)
car::ncvTest(lmMod)
gvlma(lmMod)
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
# outliers in 15, 65 and 96
lev = hat(model.matrix(lmMod))
plot(lev)
Happy2017[lev>.2,]

car::outlierTest(lmMod)
summary(influence.measures(lmMod))
# good to go
summary(lmMod)
# All assumptions were met. 
# With this test, we see our slope is not zero. There is sufficient evidence to lead us to believe that in 2015 GDP/economy somehow influences child mortality rates in the countries represented in our data. 
# we can reject the null hypothesis and conclude there is a linear relationship between GDP and child mortality
# looking at the R-squared measure we can see that 47% of the variation in child mortality rates in 2017 can be explained by the GDP/economy. The other 53% is due to error or other vaiables not accounted for in this model.

# We will now look at GDP impact on happiness score. 

scatter.smooth(x=Happy2015.2017$Economy.GDP.per.Capita.2015, y=Happy2015.2017$Happiness.Score.2015, main="GDP impact on Happiness Score 2015")
lmMod <- lm(Happiness.Score.2015~Economy.GDP.per.Capita.2015, data=Happy2015.2017)
par(mfrow=c(2,2))
plot(lmMod)
lmtest::bptest(lmMod)
car::ncvTest(lmMod)
gvlma(lmMod)
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
# outliers in 95, 108 and 118
lev = hat(model.matrix(lmMod))
plot(lev)
Happy2017[lev>.2,]

car::outlierTest(lmMod)
summary(influence.measures(lmMod))
# good to go
summary(lmMod)
# All assumptions were met here too. 
# with this we can conclude that the slope is not zero. There is sufficient evidence to lead us to believe GDP somehow influences the happiness score in all the countries in our data in 2015. 
# here we have a t test that is significant at p < .001 meaning GDP is a significant predictor of Happiness score. 
# Our adjusted R-squared tells us that 64% of the happiness score can be explained by GDP per capita. The other 36% is due to error or other variables not accounted for in this model.

