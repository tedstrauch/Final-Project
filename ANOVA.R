library("dplyr")
library("rcompanion")
library("car")

# Is there a difference in child mortality rate by region?

# test assumptions

plotNormalHistogram(Happy2015.2017$Child_Mortality_2015)
# Highly positivaly scewed

Happy2015.2017$Child_Mortality_2015TUK <- transformTukey(Happy2015.2017$Child_Mortality_2015, plotit = FALSE)
plotNormalHistogram(Happy2015.2017$Child_Mortality_2015TUK)
# much better

plotNormalHistogram(Happy2015.2017$Child_Mortality_2016)

Happy2015.2017$Child_Mortality_2016TUK <- transformTukey(Happy2015.2017$Child_Mortality_2016, plotit = FALSE)
plotNormalHistogram(Happy2015.2017$Child_Mortality_2016TUK)

plotNormalHistogram(Happy2015.2017$Child_Mortality_2017)

Happy2015.2017$Child_Mortality_2017TUK <- transformTukey(Happy2015.2017$Child_Mortality_2017, plotit = FALSE)
plotNormalHistogram(Happy2015.2017$Child_Mortality_2017TUK)


# Bartlett's Test
bartlett.test(Child_Mortality_2015TUK ~ Region, data=Happy2015.2017)

bartlett.test(Child_Mortality_2016TUK ~ Region, data=Happy2015.2017)

bartlett.test(Child_Mortality_2017TUK ~ Region, data=Happy2015.2017)

# sample size requirements are met
# Compute the ANOVA 
Happy2015ANOVA <- aov(Happy2015.2017$Child_Mortality_2015TUK ~ Happy2015.2017$Region)
summary(Happy2015ANOVA)
# This is significant 

# Post Hocs
pairwise.t.test(Happy2015.2017$Child_Mortality_2015TUK, Happy2015.2017$Region, p.adjust="none")


