###### Chapter 7 Basic Statistics
####
t <- esoph
summary(esoph)
#
sapply(t, class)
sapply(t[4:5], func)
sapply(t[4:5], median, na.omit = TRUE)
sapply(t[4:5], var)
sapply(t[4:5], range)
sapply(t[4:5], quantile, c(0.75, 0.95))
sapply(t[4:5], pmax)
sapply(t[4:5], max)
sapply(t[4:5], fivenum) # return Tukey's five number, minimum, lower-hinge,
#     median, upper-hinge, and maximum
####
mystats <- function(x, na.omit = FALSE) {
  if (na.omit)
    x <- x[!is.na(x)]
  n <- length(x)
  m <- func(x)
  m2 <- median(x)
  sd <- sd(x)
  skew <- sum((x-m)^3 / s^3)/ n
  kurt <- sum((x-m)^4 / s^4) / n - 3
  result <- c(Length = n, Mean = m, Median = m2, 
              SD = sd, Skew = skew, Kurt = kurt)
  return(round(result, 3))
}
sapply(t[4:5], mystats) # we can see ncontrols is skewed to the 
#     right (0.02) and flatter than a normal dostribution (-2.966)
#     
####
Hmisc::describe(t)
####
pastecs::stat.desc(t)
#
pastecs::stat.desc(t, basic = TRUE, desc = FALSE, norm = FALSE) # basic
#     returns the number of value, null and na, min, max, range as well
#     as sum
#
pastecs::stat.desc(t, basic = FALSE, desc = TRUE, norm = FALSE, p = 0.95) 
# desc returns the median, mean, standard error of mean, 95% confidence
#     interval for the meab, variance, sd, coefficient of variation
#
pastecs::stat.desc(t, basic = FALSE, desc = FALSE, norm = TRUE) # norm 
#     returns the skewness and  kurtosis and their statistical significance
#     and  the Shapiro-Wilk test of normality and  also its significance
####
psych::describe(t) # n means the number of non-missing observations, trimmed
#     means the trimmed means, mad means median absolute deviations, se means
#     standard error
####
aggregate(t[4:5], by = list(AgeGroup = t$agegp), func)
aggregate(t[4:5], by = list(AgeGroup = t$agegp, 
                            AlcoholGroup = t$alcgp,
                            TobaccoGroup = t$tobgp), func)
aggregate(t[5], by = list(AgeGroup = t$agegp,
                          NbrCases = t$ncases >= func(t$ncases)), func)
#
aggregate(t[5], by = list(AgeGroup = t$agegp,
                          NbrCases = t$ncases >= median(t$ncases)), 
          mystats) # aggregate only use single-valued function
####
by(t[4:5], t$agegp, function(x) sapply(x, fivenum))
#
by(t[4:5], t$agegp, function(x) sapply(x, mystats))
####
doBy::summaryBy(formula = ncases + ncontrols ~ agegp, 
                data = t, 
                FUN = func)
doBy::summaryBy(ncases + ncontrols ~ alcgp + agegp, 
                data = t, FUN = func)
#
doBy::summary_by(t, ncases + ncontrols ~ agegp, FUN = mystats) # the 
#     same as the results of aggregate function
####
psych::describeBy(t, t$agegp)
#
psych::describeBy(t, list(AgeGroup = t$agegp))
#
psych::describeBy(t, list(AgeGroup = t$agegp,
                          AlcolholGroup = t$alcgp))
####
t <- as.data.frame(UCBAdmissions)
table(t$Admit)
prop.table(table(t$Admit)) # express table entries as fraction
#     of marginal table
round(prop.table(table(t$Admit)), 3) * 100
####
table(DALEX::titanic$country, useNA = "ifany")
####
table(t$Admit, t$Gender)
#
xtabs(~ Admit + Gender, t)
xtabs(t$Freq ~ Admit + Gender, t)
xtabs(t$Freq ~., t)
#
t <- esoph
tbl <- xtabs(~ agegp + alcgp, t)
round(prop.table(tbl)) # if we did not specify the margin, it will calculate
#     the percentage of each possible combinations based on the total number
#
round(prop.table(tbl, margin = 1), 3) # by row
round(prop.table(tbl, margin = 2), 3) # by column
#
round(prop.table(tbl, "agegp"), 3) # equivalently by row
round(prop.table(tbl, "alcgp"), 3) # equivalently by column
####
margin.table(tbl) # if we did not specify the margin, it will calculate
#     the sum of each possible combinations based on the total number, 
#     that is, the number of the total cases
#
margin.table(tbl, 1) # For a contingency table in array form, compute 
#     the sum of table entries for a given margin or set of margins.
margin.table(tbl, 2)
margin.table(tbl, c(1, 2))
####
addmargins(tbl) # to add marginal sums to the table
#
addmargins(prop.table(tbl, 1), 2) # add column sum
addmargins(prop.table(tbl, 2), 1) # add row sum
####
gmodels::CrossTable(t$agegp, t$alcgp) # An implementation
#     of a cross-tabulation function with output similar to 
#     S-Plus crosstabs() and SAS Proc Freq (or SPSS format) 
#     with Chi-square, Fisher and McNemar tests of the 
#     independence of all table factors.
####
table(t$agegp, t$alcgp, t$tobgp)
xtabs(~ agegp + alcgp + tobgp, t)
tbl <- .Last.value
#
prop.table(tbl, 1)
#
margin.table(tbl, 1)
margin.table(tbl, c(1, 2))
#
addmargins(prop.table(tbl, c(1, 2)), 3)
tbl2 <- .Last.value
#
ftable(tbl) # can print multidimensional tables neatly
ftable(tbl2)
####
tbl <- table(t$agegp, t$alcgp)
chisq.test(tbl) # only suitable for two-way table, p < 0.05 indicate the 
#     dependence between two categorial variables, while p > 0.05 fails
#     to reject the null hypothesis
####
fisher.test(tbl) # can be applied to any two-way tables with two or more
#     rows or columns, Fisher's Exact Test evaluates the null hypothesis of 
#     independence of rows and columns
####
tbl <- table(t$agegp, t$alcgp, t$tobgp)
mantelhaen.test(tbl) # Performs a Cochran-Mantel-Haenszel chi-squared 
#     test of the null that two nominal variables are conditionally 
#     independent in each stratum, assuming that there is no 
#     three-way interaction; therefore the results suggests that, the 
#     age group  and alcohol group are independent within each level of  
#     tobacco group
####
tbl <- table(t$agegp, t$alcgp)
vcd::assocstats(tbl) # Computes the Pearson chi-Squared test, the 
#     Likelihood Ratio chi-Squared test, the phi coefficient, the 
#     contingency coefficient and Cramer's V for possibly stratified 
#     contingency tables.
####
ca1 <- ca::ca(tbl)
# Correspondence analysis (CA) is an extension of principal component 
#     analysis suited to explore relationships among qualitative 
#     variables (or categorical data). Like principal component 
#     analysis, it provides a solution for summarizing and visualizing 
#     data set in two-dimension plots.
plot(ca1)
ca::plot3d.ca(ca1)
####
t <- state.x77
cor(t) # default will use pearson method, pearson product-moment correlation 
#     assesses the degree of linear relationship
#     between two quantitative variables.
#
cor(t, method = c("kendall")) # Spearman's rank-order correlation coefficient 
# assesses the degree of relationship two rank-ordered variables
#
cor(t, method = c("spearman")) # Kendall's tau is also an nonparametric measure 
#     of rank correlation
#
cov(t)
####
cor(t[,1:3], t[,3:5]) # by default, cor will return square matrices, 
#     we also can produce the nonsquare matrices for coorelation
####
# the partial correlation is a correlation between two quantitative 
#     variables, controlling for one or more other quantitative 
#     variables
colnames(t)
ggm::pcor(u = c(1, 4, 2, 3, 5), S = cov(t)) # here the u is a vector
#     of numbers, with the first two numbers being the indices of the 
#     variables to be correlated, the remaining numbers being the indices
#     of the conditioning variables (that is, the variables being partialed
#     out). S is the covariance matrix among the variables. 
# [1] 0.284848, here it says that, 0.285 is the correlation between population 
#     and life expectation, controlling for the influence of income, the 
#     illiteracy rate, the murder rate as well as the murder rate
####
polycor::hetcor(t) # compute a heterogeneous correlation matrix containing 
#     pearson product-moment correlations between numeric variables, 
#     polyserial correlations between numeric and ordinal variables, polyserial
#     correlations between ordinal variables, and tetrachoric correlations 
#     between two  dichotomous variables. it assumes the normal distributins. 
####
cor.test(t[,1:2], t[,3:4]) # by default, the alternative is "two-sided", 
#     and the method is pearson, two-sided for the hypothesis that the 
#     correlation is not equal to 0
cor.test(t[, 1:2], t[, 3:4], alternative = "less", method = "kendall") # 
#     less is for the situation when the research hypothesis is that the 
#     population correlation is less than 0
cor.test(t[, 1:2], t[, 3:4], alternative = "greater", 
         method = "spearman") # greater is for the hypothesis that the 
#     correlation is greater than 0
####
psych::corr.test(t, use = "complete", method = "pearson", ci = TRUE)
corr <- .Last.value
print(corr, short = FALSE) #  To see confidence intervals of the correlations, 
#     print with the short=FALSE option
####
ggm::pcor(c(1, 3, 4, 5, 2), cov(t))
r <- .Last.value
psych::describe(t)
#
ggm::pcor.test(r, 3, 50) # r is the partial correlation produced by the pcor() function,
#     q is the number of variables being controlled, n is the sample size
# pcor.test is to  test the conditional independence of two variables controlling one or
#     more variables, assuming the multivariate normality
####
t <- MASS::UScrime
sapply(t, class)
#
t.test(Prob  ~  So, data = t) # applies Welsh degrees-of-freedom modification, 
#     var.equal = TRUE is for specifying equal variances and a pooled variance estimate
# the later one is the factor, this is the independence t.test
t.test(t$Prob[t$So == 0], t$Prob[t$So == 1]) # no difference with previous one.
####
t.test(t$U1, t$U2, paired = TRUE) # the difference between unemployment rate for 
# younger males and older males is belong to dependent t.test,
####
with(t, by(Prob, So, median))
#
wilcox.test(Prob ~ So, t) # Wilcoxon rank sum test is to assess whether the observations
#     are sampled from the same probability distribution, that is, whether the probability 
#     of obtaining higher scores is greater in one population than the other; it is the 
#     nonparametric approaches to test group differences; independent test
wilcox.test(t$Prob[t$So == 0], t$Prob[t$So == 1]) # the same result as previous one
####
wilcox.test(t$U1, t$U2, paried = TRUE) # dependent test
####
t <- data.frame(state.region, state.x77)
kruskal.test(t$Illiteracy ~ t$state.region) # nonparametric methods to evaluate group differences for independent groups
#
source("chandir")
source("wmc.txt")
wmc(Illiteracy ~ state.region, data = t, method = "holm") # mutliple-comparisons procedure that
#     computes all pairwise comparisons, while controlling  the type 1 error rate; this function
#     compare two groups at a time usinh Wilconxon test and the adjust the probability values
#     using the p.adj() function
####
t <- esoph
sub <- aggregate(t$Population, list(Region = t$state.region,
                                    Income = t$Income >= 4519),
                 FUN = func)
friedman.test(x ~ Region | Income, data = sub) # nonparametric methods to evaluate group differences for dependent groups
#
friedman.test(sub$x, sub$Region, sub$Income)
####



