###### Chapter 8
####
## linear and curvilinear relationships
# simple linear
# polynomial
# mutiple linear
# multilevel: predicting a response variables from data that hvae a hierarchical
#     struture (e.g., students within different classrooms in a school),
#     it also called hierachial, nested or mixed model 
# multivariate: one or more than one responses
# logistic
# possion
# cox proportional harzard: predicting time to an event (death, failure,
#     relapse) from one or more explanatory variables
# time-series: modeling time-series data with correlated errors
# nonlinear
# nonparametric
# robust
####
## OLS model assumptions:
# Normality: for fixed value of the independent variables, the dependent 
#     variables is normally distributed
# Independence: the Yi (response) values are dependent to each other 
# Linearity: the dependent varible is linearly related to the independent
#     variables
# Homoscedasticity: the variance of the dependent varaible does not vary
#     with the levels of the independent variables.(constant variance)
####
t <- women
fit <- lm(weight  ~ height, t)
#
summary(fit)
# the regression coefficient is significantly different from zero
# the model accounts fpr 99.1% of the variance in weights, R Squared
# F statistics, test whether the predictor variables, taken together to 
#     predict the response variable  above the chace levels
#
confint(fit)
resid(fit)
fitted(fit)
#
plot(t$weight ~ t$height)
abline(fit) # we can see we might need some adjustments to the model, for
# instance, log() or polynomial 
####
fit2 <- lm(weight ~  height + I(height^2), t)
summary(fit2) # 0.99995
# 
lines(t$height, fitted(fit2), lty = 2, lwd = 3, col = col[2]) # it is not
#     a stright line anymore, so we will use line function
# 
fit3 <- lm(weight  ~  height + I(height^2) + I(height^3), t)
summary(fit3) # 0.9998, its plot cannot use lines to plot it.
####
car::scatterplot(weight ~  height, t, ellipse = TRUE)
#
car::scatterplot(weight ~ height, data = t, 
                 smooth = list(lty = 2),
                 pch = 19)
#
car::scatterplot(weight ~ height, data = t, 
                 regLine = list(lty = 2, lwd = 3, col = col[3]),
                 pch = 19)
####
require("car")
scatterplot(prestige ~ income, data=Prestige, ellipse=TRUE)
#
scatterplot(prestige ~ income | type, data=Prestige,
            smooth=list(var=TRUE, 
                        span=1, lwd=4, lwd.var=2))
#
scatterplot(prestige ~ income | type, data=Prestige, 
            legend=list(coords="topleft"))
# 
scatterplot(vocabulary ~ education, jitter=list(x=1, y=1),
            data=Vocab, smooth=FALSE, lwd=3)
#
scatterplot(infantMortality ~ ppgdp, log="xy", data=UN, id=list(n=5))
####
colnames(state.x77)
t <- data.frame(state.x77[,c("Income", "Murder", "Frost", "Population")])
cor(t)
car::scatterplotMatrix(t, smooth = list(lty = 2), col = col[2])
#
fit <- lm(Murder ~ Income + Frost + Population, t)
summary(fit)
# the coefficients indicate the increase in the dependent variables for a unit change
#     constant in a predictor variable, holding all other predictors unchanged
####
t <- mtcars
fit <- lm(mpg ~ hp + wt + hp:wt, t)
summary(fit)
# a significant interaction between two predictor variables tell you that the 
#     relationship between one predictor and the response variable depends on the
#     level of the other, here means the relationship between mile per gallon and 
#     horsepower varies by car weight
####
require("effects")
plot(effect("hp:wt", fit,, list(wt = c(2.2, 3.2, 4.2))), multiline = TRUE)
####
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
t <- women
fit <- lm(weight ~  height, t)
plot(fit)
# Normality: If the dependent variable is normally distributed for a fixed set of
#     predictor values, then the residual values should be normally distributed with 
#     a mean of 0. The Normal Q-Q plot (upper right) is a probability plot of the 
#     standardized residuals against the values that would be expected under normality. 
#     If you’ve met the normality assumption, the points on this graph should fall on
#     the straight 45-degree line. Because they don’t, you’ve clearly violated the 
#     normality assumption.
# Independence: You can’t tell if the dependent variable values are independent
#     from these plots. You have to use your understanding of how the data was collected. 
#     There’s no a priori reason to believe that one woman’s weight influences
#     another woman’s weight. If you found out that the data were sampled from families, 
#     you might have to adjust your assumption of independence.
# Linearity: If the dependent variable is linearly related to the independent variables, 
#     there should be no systematic relationship between the residuals and the
#     predicted (that is, fitted) values. In other words, the model should capture all
#     the systematic variance present in the data, leaving nothing but random noise.
#     In the Residuals vs. Fitted graph (upper left), you see clear evidence of a curved
#     relationship, which suggests that you may want to add a quadratic term to the
#     regression.
# Homoscedasticity: If you’ve met the constant variance assumption, the points in
#     the Scale-Location graph (bottom left) should be a random band around a 
#     horizontal line. You seem to meet this assumption.
# Residuals vs. Leverage graph (bottom right) provides information about
#     individual observations that you may wish to attend to. The graph identifies 
#     outliers, high-leverage points, and influential observations. 
#
fit2 <- lm(weight ~ height + I(height ^ 2), t)
plot(fit2)
####
states <- as.data.frame(state.x77[, c("Frost", "Murder", "Population", "Income")])
fit <- lm(Murder ~ Frost + Population + Income, data = states)
plot(fit)
####
par(opar)
qqPlot(fit, labels = row.names(t), id.method = "indentify",
            simulate = TRUE)
# id.method = "indentify" makes the plot interactive, after the graph is drawn, mouse
#     click on points in the graph will label them with values specified in the lables
#     option functions
# When simulate=TRUE, a 95% confidence envelope is produced using a parametric 
#     bootstrap.
# Alaska and  Nevada has large positive residuals, indicating  that the model underestimates
#     the murder rate in these two states.
#
states["Alaska",] # 11.3
fitted(fit)["Alaska"] # 3.07903 
####
car::residualPlot(fit)
####
residualplot <- function(fit, nbreaks = 10) {
  z <- rstudent(fit)
  hist(z, breaks = nbreaks, prob = TRUE, 
       xlab = "Studentized Residual",
       main = "Distribution of Errors")
  rug(jitter(z), col = "brown")
  curve(dnorm(x, mean = func(z), sd = sd(z)), add = TRUE, col = "blue", lwd = 2)
  lines(density(z)$x, density(z)$y, col = "red", lwd = 2, lty = 2)
  legend("topright", 
          legend = c("Normal Curve", "Kernel Density Curve"),
          lty = 1:2, col = c("blue", "red"), cex = 0.7)
}
residualplot(fit)
## Normality
####
car::durbinWatsonTest(fit) # to detect such correlated errors, large p value suggest
#     the lack of autocorrelation and teh independence of errors
## Independence
####
car::crPlots(fit) # using component plus residual plots, also known as partial residaul
#     plots to find the evidence of nonlinearity
####
car::ncvTest(fit) # produces a score test of the hypothesis of constant error variance 
#     against the alternative that the error variance changes with the level of the fitted
#     values; p = 0.012212, does not have constant variance, which is heteroscedasticity
#
car::spreadLevelPlot(fit) # creates a scatter plot of the absolute standardized residuals vs 
#     the fitted  values and superimposes a line of best fit
# the suggested power p (Yp) that would stabilize the nonconstant error variance.
# If the suggested power was 0, you’d use a log transformation. In the current example,
#     there’s no evidence of heteroscedasticity, and the suggested power is close to 1 (
####
gvlma::gvlma(fit) # performs the global validation of linear model assumptions as well as 
#     sepearte evaluations of skewness, kurtosis and heteroscedasiticity
####
car::vif(fit)
# mutlicollinearity, can be detected by using a statistic called the variance inflation
#     factor (VIF), the square root of VIF indicates the degree to which teh confidence
#     interval for that varaibles's regression paramteter is expanded relative to a model 
#     with uncorrelated predictors
# As a general rule, sqrt(vif) > 2 indicates a multicollinearity problem. 
# This amounts to looking at the relationship of grip strength and age, holding age constant.
#     The problem is called multicollinearity. It leads to large confidence intervals for model
#     parameters and makes the interpretation of individual coefficients difficult.
####
car::outlierTest(fit) # outliers are the observations that are not predicted well by the model
# they have positive or negative residuals, positive residuals means that the model underestimate 
#     the response value, while the negative is for overestimating
# this function test the single largest residual for significant outlier, No Studentized residuals
#     with Bonferroni p < 0.05
####
# high leverage points are outliers with unusual combination of predictors values, obeservation
#     with high leverage points named hat statistic
# For a given dataset, the average hat value is p/n, where p is the number of parameters estimated
#     in the model (including the intercept) and n is the sample size. Roughly speaking, an
#     observation with a hat value greater than 2 or 3 times the average hat value should be
#     examined. 
#
hat.plot <- function(fit) {
  p <- length(coef(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2, lwd = 3)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
# identify reads the position of the graphics pointer when the (first) mouse button is 
#     pressed. It then searches the coordinates given in x and y for the point closest 
#     to the pointer. If this point is close enough to the pointer, its index will be
#     returned as part of the value of the call.
hat.plot(fit) #  two lines are two and three times of the average hat values
# higher-leverage values may or may not be influential points, that will depend on whether
#     they are outliers at the same time.
####
# influential points have a disproportionate impact on the values of the model parameters.
# Roughly speaking, Cook’s D values greater than 4/(n – k – 1), where n is the sample size 
#     and k is the number of predictor variables, indicate influential observations. 
cutoff <- 4 / (length(fitted(fit)) - length(coef(fit)) - 1)
cutoff2 <- 4 / (length(fitted(fit)) - length(coef(fit)) - 2) #  Note that although it’s useful
#     to cast a wide net when searchin for influential observations, I tend to find a cutoff of 1 
#     more generally useful than 4/(n – k – 1). Given a criterion of D=1, none of the observations
#     in the dataset would appear to be influential.
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, lwd = 3, col  = "red")
abline(h = 0)
#
plot(fit, which = 4)
par(mfrow = c(2, 2))
plot(fit)
par(opar)
####
# For one response variable and k predictor variables, you’d create k added-variable plots as 
#     follows
# For each predictor Xk, plot the residuals from regressing the response variable on the other 
#     k – 1 predictors versus the residuals from regressing Xk on the other k – 1 predictors. 
# comparing with cook distance plot, added-variable plots can also provide how the influential
#     points influence the model
car::avPlots(fit)
####
car::influencePlot(fit)
# Influence plot. States above +2 or below –2 on the vertical axis are considered outliers. 
# States above 0.2 or 0.3 on the horizontal axis have high leverage (unusual combinations 
#     of predictor values). 
# Circle size is proportional to influence. Observations depicted by large circles may have 
#     disproportionate influence on the parameter estimates of the model.
####
# after identifying the problems, solutions may be 
# deleting observations
# transforming variables
# adding or deleting variables
# using another regression approach
####
# In other cases, the unusual observation may be the most interesting thing about
#     the data you’ve collected. Uncovering why an observation differs from the rest can
#     contribute great insight to the topic at hand and to other topics you might not have
#     thought of. Some of our greatest advances have come from the serendipity of noticing
#     that something doesn’t fit our preconceptions (pardon the hyperbole)
# Deleting outliers can often improve a dataset’s fit to the normality assumption. 
####
# When models don’t meet the normality, linearity, or homoscedasticity assumptions,
#     transforming one or more variables can often improve or correct the situation. 
t <- car::powerTransform(states$Murder) # generate a maximum-likelihood estimation of the 
#     power λ most likely, to normalize the variable Xλ. Box-Cox Transformation to Normality
summary(t) # But in this case, the hypothesis that λ=1 can’t be rejected (p = 0.145), so 
#     there’s no strong evidence that a transformation is needed in this case. 
## NORMALITY
####
states <- as.data.frame(state.x77)
car::boxTidwell(Murder ~ Population + Illiteracy, data = states) # generate maximum-likelihood 
#     estimates of predictor powers that can improve linearity. 
# applying the Box–Tidwell transformations
# Pr(>|z|) 0.7468 and 0.5357, suggests that neither of these two need to be transformed
## LINEARLITY
car::boxTidwell(Murder ~ Income + Frost + Population, data = states) # variables to be transformed 
#     must have only positive values, here the postive means the postive coefs
summary(fit)
#
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
summary(fit2) # all of the coefs should be positive
####
car::spreadLevelPlot(fit) # package offers a power transformation for improving
#     homoscedasticity.
# HOMOSCEDASTICITY
####
#  Deleting variables is a particularly important approach for dealing with multicollinearity.
#     If your only goal is to make predictions, then multicollinearity isn’t a problem. But if
#     you want to make interpretations about individual predictor variables, then you must deal
#     with it. The most common approach is to delete one of the variables involved in the
#     multicollinearity (that is, one of the variables with a sqrt(vif) > 2). An alternative is 
#     to use ridge regression, a variant of multiple regression designed to deal with
#     multicollinearity situations.
####
# The selection of a final regression model always involves a compromise between predictive accuracy 
#     (a model that fits the data as well as possible) and parsimony (a simple and replicable model).
fit1 <- lm(Murder ~ Population + Income + Frost, states) 
fit2 <- lm(Murder ~ Population + Frost, states)
# A nested model is one whose terms are completely included in the other model. here, model 1 is nested
#     within model2 
anova(fit1, fit2) #  p-value is 0.1514, no significance, therefore, choose model2 might be good choice
####
AIC(fit1, fit2) #  The Akaike Information Criterion (AIC) provides another method for comparing
#     models. The index takes into account a model’s statistical fit and the number of
#     parameters needed to achieve this fit. Models with smaller AIC values indicating adequate 
#     fit with fewer parameters are preferred. 
# The smaller the AIC, the better. 
# Note that although the ANOVA approach requires nested models, the AIC approach doesn’t.
####
# In stepwise selection, variables are added to or deleted from a model one at a time, until some stopping
#     criterion is reached. 
fit <- lm(Murder ~ Frost + Population + Income + Illiteracy, data = states)
MASS::stepAIC(fit, direction = "backward") #  it performs stepwise model selection (forward, backward, or 
#     stepwise) using an exact AIC criterion. 
# The AIC value for <none> is the model AIC if no variables are removed. 
#  Deleting any more variables would increase the AIC, so the process stops.
## stepwise regression
####
# In all subsets regression, every possible model is inspected. 
# The analyst can choose to have all possible results displayed or ask for the nbest models of each subset size 
#     (one predictor, two predictors, and so on).
leaps <- leaps::regsubsets(Murder ~ Population + Illiteracy + Income + Frost, data = states, 
                  nbest = 4) # For example, if nbest=2, the two best one-predictor models are displayed, followed by 
#     the two best two-predictor models, followed by the two best three-predictor models, up to a model with all predictors.
#
# R-squared is the amount of variance accounted for in the response variable by the predictors variables. R-squared 
#     always increases with the addition of predictors.  
# Adjusted R-squared is similar but takes into account the number of parameters in the model. 
# The Mallows Cp statistic is also used as a stopping rule in stepwise regression. It has been widely suggested that 
#     a good model is one in which the Cp statistic is close to the number of model parameter (including the intercept). 
#
plot(leaps, scale = "adjr2", 
     main = "Best four models for each subset size \nbased on Adjusted R-square")
# Looking at the first row (starting at the bottom), you can see that a model with the intercept and Income has an adjusted 
#     R-square of 0.33. A model with the intercept and Population has an adjusted R-square of 0.1. Jumping to the 12th row, a 
#     model with the intercept, Population, Illiteracy, and Income has an adjusted R-square of 0.54, whereas one with the intercept, 
#     Population, and Illiteracy alone has an adjusted R-square of 0.55. Here you see that a model with fewer predictors has a larger 
#     adjusted R-square (something that can’t happen with an unadjusted R-square). The graph suggests that the two-predictor model 
#     (Population and Illiteracy) is the best.
#
car::subsets(leaps, statistic = "cp", 
             main = "Cp Ploy for All Subsets Regression")
abline(1, 1, lty = 2, col = "red") # best four models for each subset size based on the Mallows Cp statistic. Better models will fall 
#     close to a line with intercept 1 and slope 1. 
#  The plot suggests that you consider a two-predictor model with Population and Illiteracy; a three-predictor model with Population, 
#     Illiteracy, and Frost, or Population, Illiteracy, and Income (they overlap on the graph and are hard to read); or a four-predictor
#     model with Population, Illiteracy, Income, and Frost. You can reject the other possible models.
####
# Cross-validation is a useful method for evaluating the generalizability of a regression equation. 
# In k-fold cross-validation, the sample is divided into k subsamples. Each of the k subsamples serves as a hold-out group, and the combined 
#     observations from the remaining k – 1 subsamples serve as the training group. The performance for the k prediction equations applied to
#     the k hold-out samples is recorded and then averaged. (When k equals n, the total number of observations, this approach is called jackknifing.) 
shrinkage <- function(fit, k = 10) {
  
  theta.fit <- function(x, y) {lsfit(x, y)}
  theta.predict <- function(fit, x) {cbind(1, x) %*% fit$coef}
  
  x <- fit$model[, 2:ncol(fit$model)]
  y <- fit$mode[,1]
  
  results <- bootstrap::crossval(x, y, theta.fit, theta.predict, ngroup = k)
  r2 <- cor(y, fit$fitted.values) ^2
  r2cv <- cor(y, results$cv.fit) ^2
  
  cat("Oringinal R-Square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}
shrinkage(fit)
# You could use cross-validation in variable selection by choosing a model that demonstrates better generalizability.
#  You’ll get less R-squared shrinkage and make more accurate predictions.
####
zstates <- as.data.frame(scale(state.x77)) # Standardized regression coefficients describe the expected change in the
#     response variable (expressed in standard deviation units) for a standard deviation change in a predictor variable, 
#     holding the other predictor variables constant. 
zfit <- lm(Murder ~ Population + Income + Illiteracy + Frost, zstates)
rank(coef(zfit))
####
# A new method called relative weights shows significant promise. The method closely approximates the average increase
#     in R-square obtained by adding a predictor variable across all possible submodels 
relweights <- function(fit, ...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta) ^ 2
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$mode[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import), 1, drop = FALSE]
  dotchart(import$Weights, labels = row.names(import),
           xlab = "% of R-Square = ", pch = 19,
           main = "Relative Importance of Predicted Variables",
           sub = paste("Total R-square =", round(rsquare, 3)),
           ...)
  return(import)
}
relweights(fit)
####


mean(c(1,2,3,4))


