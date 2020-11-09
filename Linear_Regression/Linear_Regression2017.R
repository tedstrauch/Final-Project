library(rcompanion)

# Visualizing Transformations

plotNormalHistogram(Happy2017$Happiness.Score)

# this looks approximately normally distributed, maybe just a bit negativaly skewed so well try squaring it just to see if we can make it better. 

Happy2017$Happiness.ScoreSQ <- Happy2017$Happiness.Score * Happy2017$Happiness.Score
plotNormalHistogram(Happy2017$Happiness.ScoreSQ)

# that actually brought it to far to the left, so lets stick with the original. 

plotNormalHistogram(Happy2017$Economy..GDP.per.Capita.)

# This looks fairly normally distributed as well. 

plotNormalHistogram(Happy2017$Child_Mortality_2017)

# Child Mortality is quite possitivally scewed. 

Happy2017$Child_Mortality_2017TUK <- transformTukey(Happy2017$Child_Mortality_2017, plotit = FALSE)
plotNormalHistogram(Happy2017$Child_Mortality_2017TUK)

# this now looks good. normally distributed. 

# Now lets model with Linear regression
# The question is: Does the GDP per capita impact the child mortality rate? 
# Test Assumptions
# Testing for Linearity by looking at a scatterplot. 

scatter.smooth(x=Happy2017$Economy..GDP.per.Capita., y=Happy2017$Child_Mortality_2017TUK, main="GDP impact on Child Mortality Rate")
# Although fairly spread out, there does seem to be a relationship between GDP per cap. and child mortality rates. 

# Testing for Homoscedasticity. 
# Create linear model. 
lmMod <- lm(Child_Mortality_2017TUK~Economy..GDP.per.Capita., data=Happy2017)
# Create some graphs allowing for testing of homoscedasticity. 
# First load in some more libraries. 
library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")

par(mfrow=c(2,2))
plot(lmMod)
# here we have a suspicious trend in the bottom left raising some red flags. this is showing to be heteroscedastic. 

# we'll run another test: the Breush-Pagan test. 
lmtest::bptest(lmMod)
# our p-value is greater than .05 meaning we are ok, we have homoscedasticity. 
# we can also check with the NCV test. 

car::ncvTest(lmMod)
# again we have passed the test!

# Test for Homogeneity of Variance by looking at the boxes printed above. 

# we'll also look at the GVLMA library for assumptions

gvlma(lmMod)

# This looks good! Assumptions acceptable across the board. 

# Test for outliers in X space. 
CookD(lmMod, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
# outliers in 3, 14, and 136
# test for leverage. 
lev = hat(model.matrix(lmMod))
plot(lev)
Happy2017[lev>.2,]
# looks like we have no outliers with a leverage over .2 so we can conclude that we have no outliers in x space. 

# Test for outliers in y Space. 
car::outlierTest(lmMod)
# the raw studentized deleted residual is 2.9, so it could be considered an outlier, but since our bonferroni p value is not significant, we'll leave it be. 

# Test for outliers in x and y space. 
summary(influence.measures(lmMod))
# for all the possible outliers, there are no values greater than 1, so we have no influential outliers in our data. 

# interpreting the output for simple linear regression 
summary(lmMod)
# With this we see our slope is not zero. There is sufficient evidence to lead us to believe that GDP/economy somehow influences child mortality rates in the countries represented in our data. 
# we can reject the null hypothesis and conclude there is a linear relationship between GDP and child mortality
# looking at the R-squared measure we can see that 46% of the variation in child mortality rates in 2017 can be explained by the GDP/economy. The other 54% is due to error or other vaiables not accounted for in this model. 

# We can move on to another simple linear regression with the question: How does GDP impact Happiness score. 

scatter.smooth(x=Happy2017$Economy..GDP.per.Capita., y=Happy2017$Happiness.Score, main="GDP impact on Happiness Score")
# Looks like there is a relationship between GDP and happiness score. 

# Testing for Homoscedasticity. 
# Create linear model. 
lmMod1 <- lm(Happiness.Score~Economy..GDP.per.Capita., data=Happy2017)
# Create some graphs allowing for testing of homoscedasticity. 

par(mfrow=c(2,2))
plot(lmMod1)
# Not bad, bottom left has slight upward trend. 

# we'll run another test: the Breush-Pagan test. 
lmtest::bptest(lmMod1)
# our p-value is greater than .05 meaning we are ok, we have homoscedasticity. 
# we can also check with the NCV test.

car::ncvTest(lmMod1)
# again we have passed the test!

# Test for Homogeneity of Variance by looking at the boxes printed above. 
# we'll also look at the GVLMA library for assumptions
gvlma(lmMod1)
# again, all assumptions acceptable. 

# Test for outliers in X space. 
CookD(lmMod1, group=NULL, plot=TRUE, idn=3, newwd=TRUE)
# outliers in 33, 82, and 125
# test for leverage.

lev = hat(model.matrix(lmMod1))
plot(lev)
Happy2017[lev>.2,]
# looks like we have no outliers with a leverage over .2 so we can conclude that we have no outliers in x space. 

# Test for outliers in y Space. 
car::outlierTest(lmMod1)
# again our rstudent shows pretty high- 3.08 so we could have a problem with outliers in y space, but since our bonferroni p value is in not significant we'll leave it be. 

# Test for outliers in x and y space. 
summary(influence.measures(lmMod1))
# again, for all the possible outliers, there are no values greater than 1, so we have no influential outliers in our data. 

# interpreting the output for simple linear regression 
summary(lmMod1)
# with this we can conclude that the slope is not zero. There is sufficient evidence to lead us to believe GDP somehow influences the happiness score in all the countries in our data in 2017. 
# here we have a t test that is significant at p < .001 meaning GDP is a significant predictor of Happiness score. 
# Our adjusted R-squared tells us that 68% of the happiness score can be explained by GDP per capita. The other 32% is due to error or other variables not accounted for in this model. 






