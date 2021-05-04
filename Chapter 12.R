###### Chapter 12 Resampling Statistics and Bootstrapping
####
# Statistical approaches based on randomization and resampling can be used in cases where the data is
#     sampled from unknown or mixed distributions, where sample sizes are small, where outliers are a
#     problem, or where devising an appropriate test based on a theoretical distribution is too 
#     complex and mathematically intractable.
####
# Permutation tests, also called randomization or re-randomization tests
# A permutation test takes a different approach. If the two treatments are truly equivalent, 
#     the label (Treatment A or Treatment B) assigned to an observed score is arbitrary.
## To test for differences between the two treatments, you could follow these steps:
# 1 Calculate the observed t-statistic, as in the parametric approach; call this t0.
# 2 Place all 10 scores in a single group.
# 3 Randomly assign five scores to Treatment A and five scores to Treatment B.
# 4 Calculate and record the new observed t-statistic.
# 5 Repeat steps 3–4 for every possible way of assigning five scores to Treatment A
#     and five scores to Treatment B. There are 252 such possible arrangements.
# 6 Arrange the 252 t-statistics in ascending order. This is the empirical distribution, 
#     based on (or conditioned on) the sample data.
# 7 If t0 falls outside the middle 95% of the empirical distribution, reject the null
#     hypothesis that the population means for the two treatment groups are equal at
#     the 0.05 level of significance.
# the empirical distribution was based on all possible permutations of the data. In such cases, 
#     the permutation test is called an exact test. As the sample sizes increase, the time required
#     to form all possible permutations can become prohibitive. 
# In such cases, you can use Monte Carlo simulation to sample from all possible permutations. Doing so 
#     provides an approximate test. 
# The coin package provides a comprehensive framework for permutation tests applied to independence 
#     problems, whereas the lmPerm package provides permutation tests for ANOVA and regression designs. 
install.packages(file.choose(), repos = NULL, type = "source")
####
# If distribution="exact", the distribution under the null hypothesis is computed
#     exactly (that is, from all possible permutations). 
# The distribution can also be approximated by its asymptotic distribution 
#     (distribution="asymptotic") or via Monte Carlo resampling 
#     (distribution="approximate(B=#)"), where # indicates the number of 
#     replications used to approximate the exact distribution. 
# At present, distribution="exact" is only available for two-sample problems.
# In the coin package, categorical variables and ordinal variables must
#     be coded as factors and ordered factors, respectively. Additionally, the
#     data must be stored in a data frame.
####
t <- read.csv("permutation.txt", sep = "",header = FALSE)
p <- NULL
for (i in 1:length(t)) {
  p[i] <- t[i]
}
score<- unlist(p)
#
treatment <- factor(c(rep("A", 5), rep("B", 5)))
my.data <- data.frame(treatment, score)
#
t.test(score ~ treatment, data = my.data, var.equal = TRUE) # independent 
#     samples t-test 
coin::oneway_test(score ~ treatment, data = my.data, distribution = "exact")
# one-way exact test 
####
require("MASS")
t <- transform(UScrime, So = factor(UScrime$So)) # coin pkg need all the categorial 
#     number be coded into factors
#
wilcox.test(Prob ~ So, data = t)
# the Wilcoxon–Mann–Whitney U test. the difference in the probability of 
#     imprisonment in Southern versus non-Southern US states
#
coin::wilcox_test(Prob ~ So, data = t, distribution = "exact") # exact Wilcoxon
#     rank-sum test, two results  are the same, taht is beacuse, wilcox.test 
#     function also calculate the exact distribution by default.
####
require("multcomp")
require("coin")
#
fit <- aov(response ~ trt, data = cholesterol)
summary(fit)
# In chapter 9, you used a one-way ANOVA to evaluate the impact of five 
#     drug regimens on cholesterol reduction in a sample of 50 patients.
#
set.seed(1234)
oneway_test(response ~ trt, data = cholesterol,
            distribution = approximate(nresample = 9999)) # k-sample
#     test. the reference distribution is based on 9,999 permutations 
#     of the data. There’s clearly a difference in response among 
#     patients in the various groups. 
####
# You can use permutation tests to assess the independence of two 
#     categorical variables using either the chisq_test() or cmh_test() 
#     function. 
# The latter function is used when data is stratified on a third 
#     categorical variable. 
# If both variables are ordinal, you can use the lbl_test() function 
#     to test for a linear trend.
t <- Arthritis
sapply(t, class)
tbl <- table(t$Treatment, t$Improved)
chisq.test(tbl)
#
t <- transform(t, Improved = as.factor(as.numeric(Improved)))
chisq_test(Treatment ~ Improved, data = t, 
           distribution = approximate(nresample = 9999))
# if you’d left it an ordered factor, coin() would have generated a 
#     linear × linear trend test instead of a chi-square test. 
#     Although a trend test would be a good choice in this situation.
####
tbl <- table(t$Treatment, t$Sex, t$Improved)
mantelhaen.test(tbl)
#
cmh_test(tbl, distribution = approximate(nresample = 9999))
####
t <- as.data.frame(state.x77)
#
cor.test(t$Illiteracy,t$Murder, method = "spearman")
#
set.seed(1234)
spearman_test(Illiteracy ~ Murder, data = t,
              distribution = approximate(nresample = 9999))
####
# The perm= option can take the value Exact, Prob, or SPR. 
# Exact produces an exact test, based on all possible permutations. 
# Prob samples from all possible permutations. Sampling continues until
#     the estimated standard deviation falls below 0.1 of the estimated
#     p-value. The stopping rule is controlled by an optional Ca parameter.
# Finally, SPR uses a sequential probability ratio test to decide when 
#     to stop sampling. 
# Note that if the number of observations is greater than 10, 
#     perm="Exact" will automatically default to perm="Prob"; 
#     exact tests are only available for small problems.
####
require("lmPerm")
#
t <- women
fit <- lm(weight ~ height, t)
summary(fit)
#
set.seed(1234)
fitp <- lmp(weight ~ height, t, perm = "Prob")
summary(fitp)
####
fit <- lm(weight ~ height + I(height^2), t)
summary(fit)
#
set.seed(1234)
fitp <- lmp(weight ~ height + I(height^2), t, perm = "Prob")
summary(fitp)
####
fit <- lm(weight ~ poly(height, 3, raw = TRUE), t)
summary(fit)
#
set.seed(1234)
fitp <- lmp(weight ~ poly(height, 3, raw = TRUE), t, perm = "Prob")
summary(fitp)
####
t <- as.data.frame(state.x77)
fit <- lm(Murder ~ Population + Income + Frost + Illiteracy, t)
summary(fit)
#
set.seed(1234)
fitp <- lmp(Murder ~ Population + Frost + Income + Illiteracy, 
            t, perm = "Prob")
#
summary(fitp) # When the two approaches don’t agree, you should look 
#     at your data more carefully. It may be that the assumption of 
#     normality is untenable or that outliers are present.
####
t <- cholesterol
fit <- aov(response ~ trt, t)
summary(fit)
#
set.seed(1234)
fitp <- aovp(response ~ trt, t, perm = "Prob")
summary(fitp)
####
t <- litter
fit <- aov(weight ~ gesttime + dose, t)
summary(fit)
#
set.seed(1234)
fitp <- aovp(weight ~ gesttime + dose, t, perm = "Prob")
summary(fitp)
####
t <- ToothGrowth
fit <- aov(len ~ supp*dose, t)
summary(fit)
#
set.seed(1234)
fitp <- aovp(len ~ supp*dose, t)
summary(fitp)
# it defaults to unique sums of squares (also called SAS Type III 
#     sums of squares). Each effect is adjusted for every other effect.
# The default for parametric ANOVA designs in R is sequential sums of 
#     squares (SAS Type I sums of squares). Each effect is adjusted for
#     those that appear earlier in the model. For balanced designs, the
#     two approaches will agree, but for unbalanced designs with unequal
#     numbers of observations per cell, they won’t.
# The greater the imbalance, the greater the disagreement. If desired, 
#     specifying seqs=TRUE in the aovp() function will produce 
#     sequential sums of squares. 
####
# In each of the permutation tests described, you were able to test 
#     statistical hypotheses without recourse to the normal, t, F, or 
#     chi-square distributions.
# You may have noticed how closely the results of the tests based on 
#     normal theory agreed with the results of the permutation approach 
#     in previous sections. 
# Permutation tests are primarily useful for generating p-values that
#     can be used to test null hypotheses. They can help answer the 
#     question, “Does an effect exist?” 
# It’s more difficult to use permutation methods to obtain confidence 
#     intervals and estimates of measurement precision. Fortunately, 
#     this is an area in which bootstrapping excels.
####
# Bootstrapping generates an empirical distribution of a test statistic
#     or set of test statistics by repeated random sampling with 
#     replacement from the original sample. It allow you to generate
#     confidence intervals and test statistical hypotheses without 
#     having to assume a specific underlying theoretical distribution.
#  But what if you aren’t willing to assume that the sampling 
#     distribution of the mean is normally distributed? You can use a 
#     bootstrapping approach instead:
# 1 Randomly select 10 observations from the sample, with replacement 
#     after each selection. Some observations may be selected more than 
#     once, and some may not be selected at all. 
# 2 Calculate and record the sample mean.
# 3 Repeat the first two steps 1,000 times.
# 4 Order the 1,000 sample means from smallest to largest.
# 5 Find the sample means representing the 2.5th and 97.5th percentiles. 
#     In this case, it’s the 25th number from the bottom and top. These
#     are your 95% confidence limits.
# If the underlying distributions are unknown, if outliers are a 
#     problem, if sample sizes are small, or if parametric approaches 
#     don’t exist, bootstrapping can often provide a useful method
#     of generating confidence intervals and testing hypotheses.
####
# Bootstrapping is a statistical method that uses data resampling with 
#     replacement to estimate the robust properties of nearly any 
#     statistic. Most commonly, these include standard errors and 
#     confidence intervals of a population parameter like a mean, median, 
#     correlation coefficient or regression coefficient. 
# Bootstrapping statistics has two attractive attributes:
#     It is particularly useful when dealing with small sample sizes.
#     It makes no apriori assumption about the distribution of the 
#     sample data.
####
# The permutation test is best for testing hypotheses and bootstrapping
#     is best for estimating confidence intervals.
# Permutation tests test a specific null hypothesis of exchangeability,
#     i.e. that only the random sampling/randomization explains the 
#     difference seen. This is the common case for things like 
#     t-tests and ANOVA. It can also be expanded to things like time
#     series (null hypothesis that there is no serial correlation) or 
#     regression (null hypothesis of no relationship). Permutation 
#     tests can be used to create confidence intervals, but it 
#     requires many more assumptions, that may or may not be 
#     reasonable (so other methods are preferred). The Mann-Whitney/
#     Wilcoxon test is actually a special case of a permutation test, 
#     so they are much more popular than some realize.
# The bootstrap estimates the variability of the sampling process and 
#     works well for estimating confidence intervals. You can do a 
#     test of hypothesis this way but it tends to be less powerful
#     than the permutation test for cases that the permutation test
#     assumptions hold.
####
# You can bootstrap a single statistic (for example, a median) or a vector
#     of statistics (for example, a set of regression coefficients).
# In general, bootstrapping involves three main steps:
# 1 Write a function that returns the statistic or statistics of 
#     interest. If there is a single statistic (for example, a median),
#     the function should return a number. If there is a set of 
#     statistics (for example, a set of regression coefficients), the 
#     function should return a vector.
# 2 Process this function through the boot() function in order to 
#     generate R bootstrap replications of the statistic(s).
# 3 Use the boot.ci() function to obtain confidence intervals for the
#     statistic(s) generated in step 2
# The type parameter specifies the method for obtaining the confidence 
#     limits. The perc method (percentile) was demonstrated in the 
#     sample mean example. bca provides an interval that makes simple 
#     adjustments for bias. I find bca preferable in most circumstances. 
####
rsq <- function(formula, data, indices) {
  d <- data[indices,]
  # this is required for boot() to be able to select samples.
  fit <- lm(formula, data = d)
  return(summary(fit)$r.square)
}
#
require("boot")
set.seed(1234)
result <- boot(data = mtcars, statistic = rsq,
               R = 1000, formula = mpg ~ wt + disp)
print(result)
plot(result) # the distribution of bootstrapped R-squared values isn’t
#     normally distributed.
boot.ci(result)
# boot.ci(result, type = c("perc", "bca"))
####
bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}
result <- boot(data = mtcars, statistic = bs,R = 1000, 
               formula =  mpg ~ wt + disp)
print(result)
plot(result, index = 1)
plot(result, index = 2)
#
boot.ci(result, type = "bca", index = 2)
####
# There’s no simple answer to the first question. Some say that an 
#     original sample size of 20–30 is sufficient for good results, as 
#     long as the sample is representative of the population. Random 
#     sampling from the population of interest is the most trusted 
#     method for assuring the original sample’s representativeness.
# 1,000 replications are more than adequate in most cases. Computer
#     power is cheap, and you can always increase the number of 
#     replications if desired.
####











