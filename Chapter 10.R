###### Chapter 10 Power Analysis
# Power analysis allows you to determine the sample size required to detect an
#     effect of a given size with a given degree of confidence. 
#  Conversely, it allows you to determine the probability of detecting an effect
#     of a given size with a given level of confidence, under sample size constraints. 
# In this chapter, you’ll learn how to conduct power analyses for a variety of statistical
#     tests, including tests of proportions, t-tests, chi-square tests, balanced one-way
#     ANOVA, tests of correlations, and linear models. 
####
# If the null hypothesis is true but you reject it, you’ve committed a Type I error.
# If the null hypothesis is false and you fail to reject it, you’ve committed a Type II error.
# sample size
# significant level, (also referred to asalpha) is defined as the probability of making a Type
#     I error. The significance level can also be thought of as the probability of finding an 
#     effect that is not there. 
# Power is defined as one minus the probability of making a Type II error. Power can be thought
#     of as the probability of finding an effect that is there. 
# Effect size is the magnitude of the effect under the alternate or research hypothesis. The
#     formula for effect size depends on the statistical methodology employed in the hypothesis
#     testing. it quantifies the magnitude of the difference between populations or the 
#     relationship between explanatory and response variables. 
#  The four quantities (sample size, significance level, power, and effect size) have an
#     intimate relationship. Given any three, you can determine the fourth. 
#  Your research goal is typically to maximize the power of your statistical tests while
#     maintaining an acceptable significance level and employing as small a sample size as
#     possible. 
####
pwr::pwr.t.test(d = 0.8, sig.level = 0.05, power = 0.9, 
                type = "two.sample", alternative = "two.sided") # d is the effect size
# Additionally, you want to be 90% sure to detect such a difference if it exists, and 95% 
#     sure that you won’t declare a difference to be significant when it’s actually due to
#     random variability
# The results suggest that you need 34 participants in each group (for a total of 68 
#     participants) in order to detect an effect size of 0.8 with 90% certainty and no more
#     than a 5% chance of erroneously concluding that a difference exists when, in fact, it
#     doesn’t.
####
#  Assume that in comparing the two conditions you want to be able to detect a 0.5 
#     standard deviation difference in population means. --- d.
# You want to limit the chances of falsely declaring the population means to be different to 
#     1 out of 100. --- sig.level.
# Additionally, you can only afford to include 40 participants in the study. --- n(sample size).
# What’s the probability that you’ll be able to detect a difference between the population
#     means that’s this large, given the constraints outlined? --- ask for power.
pwr::pwr.t.test(n = 20, d = 0.5, sig.level = 0.01, 
                type = "two.sample", alternative = "two.sided") 
# With 20 participants in each group, an a priori significance level of 0.01, and a 
#     dependent variable standard deviation of 1.25 seconds, you have less than a 14% chance
#     of declaring a difference of 0.625 seconds or less significant (d = 0.5 = 0.625/1.25).
####
pwr::pwr.t2n.test(n1 = 10, n2 = 30, d = 0.5, sig.level = 0.01, 
                  alternative = "two.sided") # unequal sample sizes
####
#  For a one-way ANOVA comparing five groups, calculate the sample size needed in each group
#     to obtain a power of 0.80, when the effect size is 0.25 and a significance level of 0.05
#     is employed.
pwr::pwr.anova.test(k = 5, f = 0.25, sig.level = 0.05, power = 0.8) # provides power analysis
#     options for a balanced oneway analysis of variance
# k = the numer of groups
# n = sample size
# f = effect size
####
# You’ve set your significance level to 0.05, and you want to be 90% confident that you’ll 
#     reject H0 if it’s false. How many observations will you need?
pwr::pwr.r.test(r = 0.25, sig.level = 0.05, power = 0.9, 
                alternative = "two.sided") # function provides a power analysis for tests of correlation coefficients
# r = effect size
####
#  Let’s say you’re interested in whether a boss’s leadership style impacts workers’ 
#     satisfaction above and beyond the salary and perks associated with the job. Leadership
#     style is assessed by four variables, and salary and perks are associated with three 
#     variables. Past experience suggests that salary and perks account for roughly 30% of the
#     variance in worker satisfaction. From a practical standpoint, it would be interesting if
#     leadership style accounted for at least 5% above this figure. Assuming a significance
#     level of 0.05, how many subjects would be needed to identify such a contribution with
#     90% confidence?
f2 <- 0.05 / (1 - 0.35)
pwr::pwr.f2.test(u = 3, f2 = f2, sig.level = 0.05, power = 0.9) # prepare power analysis for linear models
# f2 = effect size
# u = numerator degrees of freedom
# v = denominator degrees of freedom
# In multiple regression, the denominator degrees of freedom equals N – k – 1, where
#     N is the number of observations and k is the number of predictors. In this case,
#     N – 7 – 1 = 185, which means the required sample size is N = 185 + 7 + 1 = 193.
####
#  Let’s say that you suspect that a popular medication relieves symptoms in 60% of
#     users. 
# A new (and more expensive) medication will be marketed if it improves symptoms in 65% 
#     of users. 
# How many participants will you need to include in a study comparing these two medications
#     if you want to detect a difference this large? 
# Assume that you want to be 90% confident in a conclusion that the new drug is
#     better and 95% confident that you won’t reach this conclusion erroneously. 
# You’ll use a one-tailed test because you’re only interested in assessing whether the new
#     drug is better than the standard. 
h <- pwr::ES.h(0.65, 0.6) # compute effect size for two populations
pwr::pwr.2p.test(h = h, sig.level = 0.05, power = 0.9,
                 alternative = "greater") # perform a power analysis when comparing two proportions.
# h = effect size
# n = common sample size in each group
# you’ll need to conduct a study with 1,605 individuals receiving the new drug and 1,605 
#     receiving the existing drug in order to meet the criteria
#### 
pwr::pwr.2p2n.test(n1 = 1000, n2 = 2000, h = h, sig.level = 0.05,
                   alternative = "greater") # for unequal sample sizes in each group
# power = 0.8469023
####
# Chi-square tests are often used to assess the relationship between two categorical
#     variables. The null hypothesis is typically that the variables are independent 
#     versus a research hypothesis that they aren’t. 
prob <- matrix(c(0.42, 0.28, 0.03, 0.07, 0.1, 0.1), byrow = TRUE, nrow = 3)
w <- pwr::ES.w2(prob) # find the effect size for this contigency table
pwr::pwr.chisq.test(w = w, df = 2, sig.level = 0.05, power = 0.9) # evaluate the power, 
#     effect size, or requisite sample size when employing a chi-square test. 
# w = effect size
# N = total smaple size
# df = degree of freedom, The degrees of freedom in a two-way contingency table are 
#     (r–1)×(c–1), where r is the number of rows and c is the number of columns. 
####
# For example, the data from past studies can be used to calculate effect sizes, which can
#     then be used to plan future studies.
# In the area of behavioral sciences, Cohen (1988) attempted to provide benchmarks for 
#     “small,” “medium,” and “large” effect sizes for various statistical tests
# t-test d 0.20 0.50 0.80
# ANOVA f 0.10 0.25 0.40
# Linear models f2 0.02 0.15 0.35
# Test of proportions h 0.20 0.50 0.80
# Chi-square w 0.10 0.30 0.50
####
# For example, what’s the probability of rejecting a false null hypothesis (that is, 
#     finding a real effect) if you’re using a one-way ANOVA with 5 groups, 25 subjects per
#     group, and a significance level of 0.05? 
pwr::pwr.anova.test(k = 5, n = 25, f = c(0.1, 0.25, 0.4), sig.level = 0.05)
####
# An alternative is to vary the study parameters and note the impact on such things as 
#     sample size and power. 
# For example, again assume that you want to compare five groups using a one-way ANOVA 
#     and a 0.05 significance level. 
es <- seq(0.1, 0.5, 0.01)
len <- length(es)
# 
samsize <- NULL
for (i in 1:len) {
  result <- pwr::pwr.anova.test(k = 5, f = es[i], sig.level = 0.05, power = 0.9)
  samsize[i] <- ceiling(result$n)
}
#
plot(samsize, es, type = "l", lwd = 2, col = col[1],
     ylab = "Effect Size", xlab = "Sample Size", 
     main = "One Way ANOVA with Power = 0.9 and Alpha = 0.05")
####
# Suppose you’d like to see the sample size necessary to declare a correlation coefficient 
#     statistically significant for a range of effect sizes and power levels. 
es <- seq(0.1, 0.5, 0.01)
nes <- length(es)
#
power <- seq(0.4, 0.9, 0.1)
npower <- length(power)
#
samsize <- array(numeric(nes * npower), dim = c(nes, npower))
for (i in 1:npower) {
  for (j in 1:nes) {
    result <- pwr::pwr.r.test(r = es[j], sig.level = 0.05, power = power[i], 
                              alternative = "two.sided")
    samsize[j, i] <- ceiling(result$n)
  }
}
#
col <- RColorBrewer::brewer.pal(npower, "RdYlBu")
xrange <- range(es)
yrange <- round(range(samsize))
#
plot(xrange, yrange, type = "n",
     xlab = "correlation coefficient or effect size (r)",
     ylab = "sample size (n)")
#
for (i in 1:npower) {
  lines(es, samsize[, i], type = "l", lwd = 2, col = col[i])
}
abline(v = 0, h = seq(0, yrange[2], 50), lty = 2, col = "grey89")
abline(v = seq(xrange[1], xrange[2], 0.02), h = 0, lty = 2, col = "grey89")
#
title("Sample Size Estimatiodn for Correlation Studiess\n Sig.level = 0.05")
legend("topright", title = "Power", as.character(power), fill = col)
####





