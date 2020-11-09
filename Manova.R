library("rcompanion")
library("car")
library("dplyr")
library("mvnormtest")

# Does the country inlfuence the happiness score and the child mortality rate from 2015 to 2017.

#Ensure variables are numeric
str(HappyMelt2015.2017$Happiness.Score)
str(HappyMelt2015.2017$Economy.GDP.per.Capita)

keeps <- c("Happiness.Score", "Child.Mortality.Rate")
Happy1 <- HappyMelt2015.2017[keeps]

Happy2 <- as.matrix(Happy1)

# Test Assumtions
## Sample Size = met
### Multivariate Normality

mshapiro.test(t(Happy2))
# We have violated the assumption of multivariate normality with a p value < .05
# We can try happiness score and GDP

Keeps1 <- c("Happiness.Score", "Economy.GDP.per.Capita")
Happy3 <- HappyMelt2015.2017[Keeps1]

Happy4 <- as.matrix(Happy3)
mshapiro.test(t(Happy4))
# This one passed with a p value>.05

# Homogeneity of Variance
leveneTest(Happiness.Score ~ Country, data=HappyMelt2015.2017)
leveneTest(Economy.GDP.per.Capita ~ Country, data=HappyMelt2015.2017)
# Both have met the assumption of homgeneity of variance

# Absence of Multicollinearity
cor.test(HappyMelt2015.2017$Happiness.Score, HappyMelt2015.2017$Economy.GDP.per.Capita, method="pearson", use="complete.obs")
# We are just above the line with a correlation of r=.80

# We'll continue with the analysis to see what happens

MANOVA <- manova(cbind(Happiness.Score, Economy.GDP.per.Capita) ~ Country, data = HappyMelt2015.2017)
summary(MANOVA)
# Looks like there is a significants in the GDP and Happiness score by country

# Post Hocs

summary.aov(MANOVA, test = "wilks")
# There is a significant difference in Happiness Score and GDP per country












