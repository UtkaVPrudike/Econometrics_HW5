library(lmtest)
library(plm)
library(sandwich)
library(Metrics)
library(car)
library(xtable)
data <- read.csv("handguns.csv")
data <- pdata.frame(data, index = c("state", "year"))

# Task 3

plm.i <- plm(log(vio) ~ shall, data, model = "pooling")
plm.ii <- plm(log(vio) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029, data, model = 'pooling')

summary(plm.ii)
# coefficient on shall is around -0.37, meaning that if a state has a shall-issue law the rate of violent crimes decreases by 37% on average, other regressors held constant,
# This value is large in a real-world sense

summary(plm.i)
coeftest(plm.i, vcov. = vcovHC(plm.i, method = "arellano", type = "HC0", cluster = "group"))
coeftest(plm.ii, vcov. = vcovHC(plm.ii, method = "arellano", type = "HC0", cluster = "group"))
# coefficients are quite different in the real-world sense (in plm.i having a shall-issue law corresponds with a reduction in violent crimes by 44%, a difference of 7 percentage points)
# coefficient on shall is statistically significant for both regressions at the 5% significance level

# culture of gun ownership could cause omitted variable bias
# a proxy for this could be the availability of guns at different stores (i.e. gun shops, supermarkets, gas stations)
# this is a determinant of the dependent variable because culture around gun ownership influences how educated people are about the dangers of carrying a gun
# this is correlated with carry-issue laws because states which more culture around gun ownership will probably allow citizens to carry firearms
# culture is varied across states but not time because social norms are usually very reluctant to change



# Task 4

full_spec.1 <- log(vio) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029

plm.1.1 <- plm(log(vio) ~ shall, data, model = "pooling")
plm.1.2 <- plm(full_spec.1, data, model = "pooling")
plm.1.3 <- plm(full_spec.1, data, model = "within", effect = "individual")
plm.1.4 <- plm(full_spec.1, data, model = "within", effect = "twoways")
plm.1.5 <- plm(full_spec.1, data, model = "within", effect = "time")

rows <- c('shall', 'state control', 'state FE', 'year FE', 'F-stat for state FE', 'p value for state FE', 'F-stat for time FE', 'p value for time FE', 'n')
one <- c(plm.1.1$coefficients[2], 'no', 'no', 'no', '-', '-', '-', '-', nobs(plm.1.1))
two <- c(plm.1.2$coefficients[2], 'yes', 'no', 'no', '-', '-', '-', '-', nobs(plm.1.2))
three <- c(plm.1.3$coefficients[1], 'yes', 'yes', 'no', pFtest(plm.1.3, plm.1.2)$statistic, pFtest(plm.1.3, plm.1.2)$p.value, '-', '-', nobs(plm.1.3))
four <- c(plm.1.4$coefficients[1], 'yes', 'yes', 'yes', pFtest(plm.1.4, plm.1.5)$statistic, pFtest(plm.1.4, plm.1.5)$p.value, pFtest(plm.1.4, plm.1.3)$statistic, pFtest(plm.1.4, plm.1.3)$p.value, nobs(plm.1.4))
data.frame(one, two, three, four, row.names = rows)



full_spec.2 <- log(rob) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029

plm.2.1 <- plm(log(rob) ~ shall, data, model = "pooling")
plm.2.2 <- plm(full_spec.2, data, model = "pooling")
plm.2.3 <- plm(full_spec.2, data, model = "within", effect = "individual")
plm.2.4 <- plm(full_spec.2, data, model = "within", effect = "twoways")
plm.2.5 <- plm(full_spec.2, data, model = "within", effect = "time")

one <- c(plm.2.1$coefficients[2], 'no', 'no', 'no', '-', '-', '-', '-', nobs(plm.2.1))
two <- c(plm.2.2$coefficients[2], 'yes', 'no', 'no', '-', '-', '-', '-', nobs(plm.2.2))
three <- c(plm.2.3$coefficients[1], 'yes', 'yes', 'no', pFtest(plm.2.3, plm.2.2)$statistic, pFtest(plm.2.3, plm.2.2)$p.value, '-', '-', nobs(plm.2.3))
four <- c(plm.2.4$coefficients[1], 'yes', 'yes', 'yes', pFtest(plm.2.4, plm.2.5)$statistic, pFtest(plm.2.4, plm.2.5)$p.value, pFtest(plm.2.4, plm.2.3)$statistic, pFtest(plm.2.4, plm.2.3)$p.value, nobs(plm.2.4))
data.frame(one, two, three, four, row.names = rows)



full_spec.3 <- log(mur) ~ shall + incarc_rate + density + avginc + pop + pb1064 + pw1064 + pm1029

plm.3.1 <- plm(log(mur) ~ shall, data, model = "pooling")
plm.3.2 <- plm(full_spec.3, data, model = "pooling")
plm.3.3 <- plm(full_spec.3, data, model = "within", effect = "individual")
plm.3.4 <- plm(full_spec.3, data, model = "within", effect = "twoways")
plm.3.5 <- plm(full_spec.3, data, model = "within", effect = "time")

one <- c(plm.3.1$coefficients[2], 'no', 'no', 'no', '-', '-', '-', '-', nobs(plm.3.1))
two <- c(plm.3.2$coefficients[2], 'yes', 'no', 'no', '-', '-', '-', '-', nobs(plm.3.2))
three <- c(plm.3.3$coefficients[1], 'yes', 'yes', 'no', pFtest(plm.3.3, plm.3.2)$statistic, pFtest(plm.3.3, plm.3.2)$p.value, '-', '-', nobs(plm.3.3))
four <- c(plm.3.4$coefficients[1], 'yes', 'yes', 'yes', pFtest(plm.3.4, plm.3.5)$statistic, pFtest(plm.3.4, plm.3.5)$p.value, pFtest(plm.3.4, plm.3.3)$statistic, pFtest(plm.3.4, plm.3.3)$p.value, nobs(plm.3.4))
data.frame(one, two, three, four, row.names = rows)

