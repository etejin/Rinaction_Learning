###### Chapter 13 Generalized linear models
####
# In chapters 8 (regression) and 9 (ANOVA), we explored linear models that can be
#     used to predict a normally distributed response variable from a set of 
#     continuous and/or categorical predictor variables.
# But there are many situations in which it’s unreasonable to assume that the 
#     dependent variable is normally distributed (or even continuous)
# The outcome variable may be categorical. Binary variables (for example, yes/
#     no, passed/failed, lived/died) and polytomous variables (for example, poor
#     /good/excellent, republican/democrat/independent) clearly aren’t normally 
#     distributed.
# logistic regression (where the dependent variable is categorical).
# The outcome variable may be a count (for example, number of traffic 
#     accidents in a week, number of drinks per day). Such variables take on
#     a limited number of values and are never negative. Additionally, their 
#     mean and variance are often related (which isn’t true for normally 
#     distributed variables).
# Poisson regression (where the dependent variable is a count variable).
####
# Instead, you assume that Y follows a distribution that’s a member of the 
#     exponential family. You specify the link function and the probability 
#     distribution, and the parameters are derived through an iterative
#     maximum-likelihood-estimation procedure.
####
# To summarize, generalized linear models extend the standard linear model by 
#     fitting a function of the conditional mean response (rather than the 
#     conditional mean response) and assuming that the response variable follows
#     a member of the exponential family of distributions (rather than being 
#     limited to the normal distribution). The parameter estimates are derived 
#     via maximum likelihood rather than least squares.
####
t <- transform(UScrime, So = as.factor(UScrime$So))
fit <- glm(So ~ y, family = binomial, data = t)
#
plot(predict(fit, type = "response"), 
     residuals(fit, type = "deviance"))
# The deviance residuals can be used to check the model fit at each observation 
#     for generalized linear models. 
# 
plot(hatvalues(fit))
#
plot(rstudent(fit))
#
plot(cooks.distance(fit))
#
car::influencePlot(fit)
# Diagnostic plots tend to be most helpful when the response variable takes on
#     many values. When the response variable can only take on a limited number
#     of values (for example, logistic regression), the utility of these plots
#     is decreased.
####
require("AER")
t <- Affairs
#
table(t$affairs)
# 
t <- transform(Affairs, Faffairs = 
                 as.factor(doBy::recodeVar(Affairs$affairs, src = "0", tgt = "0", default = "1")))
#
table(t$Faffairs)
#
fit <- glm(Faffairs ~ gender + age + yearsmarried + children + religiousness + 
             education + occupation + rating, data = t, family = binomial)
summary(fit)
#
fit.improved <- glm(Faffairs ~ age + yearsmarried + religiousness + rating, 
                    data  = t, family = binomial())
summary(fit.improved)
#
anova(fit.improved, fit, test = "Chisq") # For generalized linear models, you’ll
#     want a chi-square version of this test
# reinforcing your belief that gender, children, education, and occupation 
#     don’t add significantly to the prediction above and beyond the other 
#     variables in the equation. Therefore, you can base your interpretations 
#     on the simpler model.
####
coef(fit.improved) # In a logistic regression, the response being modeled is the
#     log(odds) that Y = 1. The regression coefficients give the change in 
#     log(odds) in the response for a unit change in the predictor variable, 
#     holding all other predictor variables constant. 
exp(coef(fit.improved)) # Because log(odds) are difficult to interpret, you can 
#     exponentiate them to put the results on an odds scale
# Now you can see that the odds of an extramarital encounter are increased by a factor
#     of 1.106 for a one-year increase in years married (holding age, religiousness, 
#     and marital rating constant). 
exp(confint(fit.improved)) # print 95% confidence intervals for each of the 
#     coefficients on an odds scal
# For binary logistic regression, the change in the odds of the higher value on 
#     the response variable for an n unit change in a predictor variable is 
#     exp(βj)^n. 
# If a one-year increase in years married multiplies the odds of 
#     an affair by 1.106, a 10-year increase would increase the odds by a factor
#     of 1.106^10, or 2.7, holding the other predictor variables constant
####
testdata <- data.frame(rating = c(1, 2, 3, 4, 5),
                       age = func(t$age),
                       yearsmarried = func(t$yearsmarried),
                       religiousness = func(t$religiousness))
#
testdata$Prob <- predict(fit.improved, newdata = testdata, type = "response")
# observe the impact of varying the levels of a predictor variable
#     on the probability of the outcome. 
####
q <- quantile(t$age, c(0, 0.25, 0.5, 0.75, 1))
testdata <- data.frame(age = c(q[1], q[2], q[3], q[4], q[5]),
                       rating = func(t$rating),
                       yearsmarried = func(t$yearsmarried),
                       religiousness = func(t$religiousness))
#
testdata$Prob <- predict(fit.improved, newdata = testdata, type = "response")
####
# Overdispersion occurs when the observed variance of the response variable
#     is larger than what would be expected from a binomial distribution. 
# Overdispersion can lead to distorted test standard errors and inaccurate 
#     tests of significance. 
# When overdispersion is present, you can still fit a logistic regression using
#     the glm() function, but in this case, you should use the quasibinomial 
#     distribution rather than the binomial distribution
#  One way to detect overdispersion is to compare the residual deviance with 
#     the residual degrees of freedom in your binomial model. If the ratio is
#     considerably larger than 1, you have evidence of overdispersion.
deviance(fit.improved) / df.residual(fit.improved) # [1] 1.03248, which is close
#     to 1, suggesting no overdispersion.
fit.od <- glm(Faffairs ~ age + yearsmarried + rating + religiousness, data = t,
              family = quasibinomial())
pchisq(summary(fit.od)$dispersion * fit.improved$df.residual,
       fit$df.residual, lower = FALSE) # [1] 0.2988425, support the hypothesis that
#     the φ = 1, which means no overdispersion. this is overdispersion test
####
# Poisson regression is useful when you’re predicting an outcome variable representing
#     counts from a set of continuous and/or categorical predictor variables
data(breslow.dat, package = "robust")
t <- breslow.dat
#
names(t)
summary(t[c(6, 7, 8, 10)])
# 
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
hist(t$sumY, breaks = 20, xlab = "seizure count",
     main = "distribution of seizures") # You can clearly see the skewed nature
#     of the dependent variable and the possible presence of outliers. 
boxplot(t$sumY ~ t$Trt, xlab = "treatment", 
        main = "group comparison")
#
par(opar)
####
fit <- glm(sumY ~ Base + Age + Trt, data = t, family = poisson())
summary(fit)
# 
coef(fit)
# In a Poisson regression, the dependent variable being modeled is the log of 
#     the conditional mean loge(λ). The regression parameter 0.0227 for Age 
#     indicates that a one-year increase in age is associated with a 0.03 
#     increase in the log mean number of seizures, holding baseline seizures 
#     and treatment condition constant. 
exp(coef(fit))
# It’s usually much easier to interpret the regression coefficients in the 
#     original scale of the dependent variable (number of seizures, rather than
#     log number of seizures).
# It’s important to remember that, like the exponentiated parameters in logistic
#     regression, the exponentiated parameters in the Poisson model have a 
#     multiplicative rather than an additive effect on the response variable.
####
# In a Poisson distribution, the variance and mean are equal. Overdispersion 
#     occurs in Poisson regression when the observed variance of the 
#     response variable is larger than would be predicted by the Poisson 
#     distribution.
# REASONS FOR OVERDISPERSION
# The omission of an important predictor variable can lead to overdispersion. 
# Overdispersion can also be caused by a phenomenon known as state dependence.
#     Within observations, each event in a count is assumed to be independent.
#     For the seizure data, this would imply that for any patient, the 
#     probability of a seizure is independent of each other seizure. But this
#     assumption is often untenable. For a given individual, the probability of 
#     having a first seizure is unlikely to be the same as the probability of 
#     having a 40th seizure, given that they’ve already had 39.
# In longitudinal studies, overdispersion can be caused by the clustering 
#     inherent in repeated measures data. We won’t discuss longitudinal Poisson
#     models here.
####
deviance(fit) / df.residual(fit) # [1] 10.1717, has overdispersion
# overdispersion is suggested if the ratio of the residual deviance to the 
#     residual degrees of freedom is much larger than 1
qcc::qcc.overdispersion.test(t$sumY, type = "poisson") # Not surprisingly, 
#     the significance test has a p-value less than 0.05, strongly suggesting
#     the presence of overdispersion.
fit.od <- glm(sumY ~ Base + Age + Trt, data = t, family = quasipoisson())
summary(fit.od)
# Notice that the parameter estimates in the quasi-Poisson approach are
#     identical to those produced by the Poisson approach.
####
# Poisson regression model, including models that allow varying time periods, 
#     models that correct for too many zeros, and robust models that are useful
#     when data includes outliers and influential observations.
####
# But you can fit Poisson regression models that allow the time period to vary 
#     for each observation. In this case, the outcome variable is a rate
# To analyze rates, you must include a variable (for example, time) that records
#     the length of time over which the count occurs for each observation.
# To fit this new model, you use the offset option in the glm() function. For 
#     example, assume that the length of time that patients participated 
#     post-randomization in the Breslow study varied from 14 days to 60 days. 
#     You could use the rate of seizures as the dependent variable (assuming 
#     you had recorded time for each patient in days) and fit the model
t$time <- 60-41
fit.time <- glm(sumY ~ Age + Trt + Base, data = t, offset = log(time), 
                family = poisson)
summary(fit.time)
# where sumY is the number of seizures that occurred post-randomization for a 
#     patient during the time the patient was studied. In this case, you’re 
#     assuming that rate doesn’t vary over time (for example, 2 seizures in 4 
#     days is equivalent to 10 seizures in 20 days)
####
# There are times when the number of zero counts in a dataset is larger than 
#     would be predicted by the Poisson model. This can occur when there’s a 
#     subgroup of the population that would never engage in the behavior being 
#     counted. 
# variable (affairs) counted the number of extramarital sexual 
#     intercourse experiences participants had in the past year. It’s likely 
#     that there’s a subgroup of faithful marital partners who would never have
#     an affair, no matter how long the period of time studied. These are 
#     called structural zeros (primarily by the swingers in the group).
# In such cases, you can analyze the data using an approach called 
#     zero-inflated Poisson regression. 
# The approach fits two models simultaneously—one that predicts who would
#     or would not have an affair, and the second that predicts how many affairs a participant
#     would have if you excluded the permanently faithful. Think of this as a
#     model that combines a logistic regression (for predicting structural 
#     zeros) and a Poisson regression model (that predicts counts for 
#     observations that aren’t structural zeros). 
#     Zero-inflated Poisson regression can be fit using the zeroinfl() function
#     in the pscl package. 
####






