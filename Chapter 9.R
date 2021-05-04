###### Chapter 9 Analysis of Variance
####
# When factors are included as explanatory variables, our focus usually shifts 
#     from prediction to understanding group differences, and the methodology is 
#     referred to as analysis of variance (ANOVA). 
####
# In this design, Treatment is a between-groups factor with two levels (CBT, EMDR).
#     It’s called a between-groups factor because patients are assigned to one and 
#     only one group. No patient receives both CBT and EMDR. These characters represent
#     the subjects (patients). 
# STAI is the dependent variable, and Treatment is the independent variable.
# Because there is an equal number of observations in each treatment condition, you have 
#     a balanced design. When the sample sizes are unequal across the cells of a design, 
#     you have an unbalanced design.
####
# The statistical design with a single classification variable is called a one-way ANOVA.
#     Specifically, it’s a one-way between-groups ANOVA. Effects in ANOVA designs are primarily
#     evaluated through F tests. If the F test for Treatment is significant, you can conclude 
#     that the mean STAI scores for two therapies differed after five weeks of treatment.
# Time is a within-groups factor with two levels (five weeks, six months). It’s called a
#     within-groups factor because each patient is measured under both levels. The statistical
#     design is a one-way within-groups ANOVA. Because each subject is measured more than once, 
#     the design is also called a repeated measures ANOVA. If the F test for Time is significant,
#     you can conclude that patients’ mean STAI scores changed between five weeks and six months.
# By including both Therapy and Time as factors, you’re able to examine the impact of Therapy 
#     (averaged across time), Time (averaged across therapy type), and the interaction of Therapy 
#     and Time. The first two are called the main effects, whereas the interaction is (not surprisingly) 
#     called an interaction effect. 
# When you cross two or more factors, as is done here, you have a factorial ANOVA design. Crossing two 
#     factors produces a two-way ANOVA, crossing three factors produces a three-way ANOVA, and so forth.
#     When a factorial design includes both between-groups and within-groups factors, it’s also called a 
#     mixed-model ANOVA. The current design is a two-way mixed-model factorial ANOVA (phew!). 
# In this case, you’ll have three F tests: one for Therapy, one for Time, and one for the Therapy × Time 
#     interaction.
# In this case, BDI would be called a covariate, and the design would be called an analysis of covariance 
#     (ANCOVA). 
#  When there’s more than one dependent variable, the design is called a multivariate analysis of variance 
#     (MANOVA). 
# If there are covariates present, it’s called a multivariate analysis of covariance (MANCOVA). 
####
# Although ANOVA and regression methodologies developed separately, functionally they’re both special cases 
#     of the general linear model. You could analyze ANOVA models using the same lm() function used for regression
#     in chapter 7. But you’ll primarily use the aov() function in this chapter. The results of lm() and aov() are 
#     equivalent, but the aov() function presents these results in a format that’s more familiar to ANOVA 
#     methodologists. For completeness, I’ll provide an example using lm() at the end of this chapter. 
####
# One-way ANOVA:  y ~ A
# One-way ANCOVA with 1 covariate:  y ~ x + A
# Two-way factorial ANOVA:  y ~ A * B 
# Two-way factorial ANCOVA with 2 covariates:  y ~ x1 + x2 + A * B 
# Randomized block:  y ~ B + A (where B is a blocking factor)
# One-way within-groups ANOVA: y ~ A + Error(Subject/A)
# Repeated measures ANOVA with 1 within-groups factor (W) and 1 between-groups factor (B):
#     y ~ B * W + Error(Subject/W)
####
# The order in which the effects appear in a formula matters when (a) there’s more than one 
#     factor and the design is unbalanced, or (b) covariates are present. 
# When either of these two conditions is present, the variables on the right side of the 
#     equation will be correlated with each other.
# The greater the imbalance in sample sizes, the greater the impact that the order of
#     the terms will have on the results.
# In general, more fundamental effects should be listed earlier in the formula. In particular, 
#     covariates should be listed first, followed by main effects, followed by two-way interactions,
#     followed by three-way interactions, and so on. 
# For main effects, more fundamental variables should be listed first. Thus gender would be 
#     listed before treatment. 
# Here’s the bottom line: when the research design isn’t orthogonal (that is, when the factors 
#     and/or covariates are correlated), be careful when specifying the order of effects
#  Before moving on to specific examples, note that the Anova() function in the car package (not to be 
#     confused with the standard anova() function) provides the option of using the Type II or Type III 
#     approach, rather than the Type I approach used by the aov() function. You may want to use the Anova() 
#     function if you’re concerned about matching your results to those provided by other packages such as SAS
#     and SPSS.
####
t <- multcomp::cholesterol
sapply(t, class)
summary(t)
#
tapply(t$response, t$trt, func)
tapply(t$response, t$trt, sd) # Standard deviations were relatively constant
#     across the five groups, ranging from 2.88 to 3.48
#
fit <- aov(response ~ trt, t)
summary(fit) # The ANOVA F test for treatment (trt) is significant (p < .0001), 
#     providing evidence that the five treatments aren’t all equally effective. 
#
gplots::plotmeans(t$response ~ t$trt, 
                  xlab = "Treatment", ylab = "Response", 
                  main = "Mean Plot with 95% CI")
#
gplots::plotmeans(t$response ~ t$trt, 
                  p = 0.99,
                  ci.label = TRUE, mean.labels = TRUE, digits = 3,
                  connect = FALSE, 
                  main = "Mean plot with 99% CI")
#
col <- RColorBrewer::brewer.pal(4, "PiYG")
gplots::plotmeans(t$response ~ t$trt, 
                  mean.labels = TRUE, digits = 3,
                  pch = 19,
                  col = col[1],
                  ccol = col[2])
#
opar <- par(no.readonly = TRUE)
par(las=2,                        # use perpendicular axis labels
    mar=c(10.1,4.1,4.1,2.1),      # create enough space for long x labels
    mgp=c(8,1,0)                  # move x axis legend down to avoid overlap
)
gplots::plotmeans(ncases/ncontrols ~ interaction(agegp , alcgp, sep =""),
          connect=list(1:6,7:12,13:18,19:24),
          barwidth=2,
          col="dark green",
          data=esoph,
          xlab="Age Group and Alcohol Consumption",
          ylab="# Cases / # Controls",
          ylim = c(-.9,1.4),
          main=c("Fraction of Cases for by Age and Alcohol Consumption",
                 "Ile-et-Vilaine Esophageal Cancer Study")
)
abline(v=c(6.5, 12.5, 18.5), lty=2)
par(opar)
####
TukeyHSD(fit) # provides a test of all pairwise differences between group means
# 
par(las = 2, # rotates the axis labels
    mar = c(5, 8, 4, 2))
plot(TukeyHSD(fit)) # pairwise comparisons, CI includes 0 indicate...
par(opar)
####
par(mar = c(5, 6, 6, 2))
tuk <- multcomp::glht(fit, linfct = mcp(trt = "Tukey")) # General linear 
#     hypotheses and multiple comparisons for parametric models, including 
#     generalized linear models, linear mixed effects models, and survival 
#     models.
# linft: a specification of the linear hypotheses to be tested. Linear 
#     functions can be specified by either the matrix of coefficients or 
#     by symbolic descriptions of one or more linear hypotheses. Multiple 
#     comparisons in AN(C)OVA models are specified by objects returned from 
#     function mcp.
plot(multcomp::cld(tuk, level = 0.05, decreasing = FALSE), 
     col = col[2]) # Extract information from glht, summary.glht or 
#     confint.glht objects which is required to create and plot compact letter
#     displays of all pair-wise comparisons.
# Groups (represented by box plots) that have the same letter don’t have 
#     significantly different means. 
par(opar)
####
# In a one-way ANOVA, the dependent variable is assumed to be normally distributed
#     and have equal variance in each group. You can use a Q-Q plot to assess the 
#     normality assumption
car::qqPlot(lm(response ~ trt, cholesterol), simulate = TRUE) 
# qqPlot require lm fit
# The data falls within the 95% confidence envelope, suggesting that the normality 
#     assumption has been met fairly well.
## NORMALITY
####
bartlett.test(response ~ trt, t) # perform Bartlett test in the null that each groups 
#     have the same variance, Bartlett test of homogeneity of variances,
#     p-value = 0.9653
## HOMOGENEITY
####
fligner.test(response ~ trt, t) # Fligner-Killeen test of homogeneity of variances,
#     p-value = 0.946
## HOMOGENEITY
####
HH::hov(response ~ trt, t) # Oneway analysis of variance makes the assumption that 
#     the variances of the groups are equal, p-value = 0.9893
## HOMOGENEITY
####
car::outlierTest(fit) # No Studentized residuals with Bonferroni p < 0.05,
#     p-value = 0.029422, there is no outliers here.
## Outliers
#  Taking the Q-Q plot, Bartlett’s test, and outlier test together, the data appear 
#     to fit the ANOVA model quite well. This, in turn, adds to your confidence in 
#     the results.
####
# A one-way analysis of covariance (ANCOVA) extends the one-way ANOVA to include
#     one or more quantitative covariates.
t <- multcomp::litter
sapply(t, class)
summary(t)
# 
tapply(t$weight, t$dose, func)
tapply(t$weight, t$dose, sd)
#
fit <- aov(weight ~ gesttime + dose, t)
summary(fit) # The ANCOVA F tests indicate that (a) gestation time was 
#     related to birth weight, and (b) drug dosage was related to birth weight
#     after controlling for gestation time. The mean birth weight isn’t the same
#     for each of the drug dosages, after controlling for gestation time.
#
effects::effect("dose", fit) # calculate adjusted means.
# Because you’re using a covariate, you may want to obtain adjusted group means, 
#     that is, the group means obtained after partialing out the effects of 
#     the covariate.
# The effects package provides a powerful method of obtaining adjusted means for 
#     complex research designs and presenting them visually. 
#
contrast <- rbind("no drug vs. drug" = c(3, -1, -1, -1)) # the multcomp package 
#     can be used to test specific user-defined hypotheses about the means.
# The contrast c(3, -1, -1, -1) specifies a comparison of the first group with 
#     the average of the other three. 
summary(multcomp::glht(fit, linfct = mcp(dose = contrast)))
# The hypothesis is tested with a t statistic (2.581 in this case), which is 
#     significant at the p < .05 level. Therefore, you can conclude that the no-drug
#     group has a higher birth weight than drug conditions. 
# For each factor, which is included in model as independent variable, a contrast 
#     matrix or a symbolic description of the contrasts can be specified as 
#     arguments to mcp.
# The mcp function must be used with care when defining parameters of 
#     interest in two-way ANOVA or ANCOVA models. Here, the definition of 
#     treatment differences (such as Tukey's all-pair comparisons or 
#     Dunnett's comparison with a control) might be problem specific.
#     Because it is impossible to determine the parameters of interest 
#     automatically in this case, mcp in multcomp version 1.0-0 and 
#     higher generates comparisons for the main effects only, ignoring 
#     covariates and interactions (older versions automatically averaged 
#     over interaction terms). A warning is given.
## Other contrasts can be added to the rbind() function.
amod <- aov(breaks ~ tension, data = warpbreaks)
# alternatively, describe differences symbolically
glht(amod, linfct = mcp(tension = c("M - L = 0", 
                                    "H - L = 0",
                                    "H - M = 0")))

# alternatively, define contrast matrix directly
contr <- rbind("M - L" = c(-1, 1, 0),
               "H - L" = c(-1, 0, 1), 
               "H - M" = c(0, -1, 1))
glht(amod, linfct = mcp(tension = contr))
####
# ANCOVA designs make the same normality and homogeneity of variance assumptions
#     described for ANOVA designs.
car::qqPlot(lm(weight ~ gesttime + dose, litter), simulate = TRUE)
# NOMALITY
####
# standard ANCOVA designs assume homogeneity of regression slopes.
# In this case, it’s assumed that the regression slope for predicting birth weight 
#     from gestation time is the same in each of the four treatment groups. 
fit2 <- aov(weight ~ gesttime * dose, t)
# A test for the homogeneity of regression slopes can be obtained by including a 
#     gestation × dose interaction term in your ANCOVA model. A significant
#     interaction would imply that the relationship between gestation and birth
#     weight depends on the level of the dose variable.
summary(fit2)
# The interaction is nonsignificant, supporting the assumption of equality of slopes. 
# If the assumption is untenable, you could try transforming the covariate or dependent
#     variable, using a model that accounts for separate slopes, or employing a 
#     nonparametric ANCOVA method that doesn’t require homogeneity of regression slopes.
#     See the sm.ancova() function in the sm package for an example of the latter. 
## HOMOGENEITY
####
require("HH")
ancova(weight ~ gesttime + dose, data = litter)
# Here you can see that the regression lines for predicting birth weight from gestation
#     time are parallel in each group but have different intercepts. As gestation time
#     increases, birth weight increases. 
# Additionally, you can see that the zero-dose group has the largest intercept and the 
#     five-dose group has the lowest intercept. The lines are parallel because they’ve
#     been specified to be. 
####
ancova(weight ~ gesttime * dose, data = litter)
# you’d generate a plot that allows both the slopes and intercepts to vary by group. 
# This approach is useful for visualizing the case where the homogeneity of regression 
#     slopes doesn’t hold.
## HOMOGENEITY
####
# In a two-way factorial ANOVA, subjects are assigned to groups that are formed from the
#     cross-classification of two factors. 
t <- ToothGrowth
sapply(t, class)
summary(t)
#
table(t$supp, t$dose) # balanced design
#
aggregate(t$len, by = list(Way = t$supp, Dose = t$dose), func)
aggregate(t$len, by = list(Way = t$supp, Dose = t$dose), sd)
#
t$dose <- factor(t$dose)
#
fit <- aov(len ~ dose * supp, t) # for two-way factorial ANOVA, we also need to consider
#   its interactions
summary(fit) # significance not only for the main effects, but also for the interactions
#     between these two factors
####
# visualization 1
interaction.plot(t$dose, t$supp, t$len, type = "b", 
                 legend = TRUE, col = col[1:2], pch = c(18, 19),
                 main = "Interaction Between Dose and Supplement Type")
# this function can display the interaction in a two-way ANOVA, while the ultimate plot shows 
#     the mean tooth length for each supplement at each dose. 
interaction.plot(t$dose, t$supp, t$len, fun = median, type = "b", 
                 trace.label = "Supplemenet",
                 legend = TRUE, col = col[1:2], pch = c(13, 15),
                 cex.axis = 0.8, cex = 0.6)
# try median
####
# visualization 2
gplots::plotmeans(t$len ~ interaction(t$supp, t$dose, sep = " "),
                  connect = list(c(1, 3, 5), c(2, 4, 6)),
                  col = col[1:2],
                  main = "Interaction Plot with 95% CIs",
                  xlab = "Treatment and Dose Combination") # the graph also shows
#     the means, error bars (95% CI) as well as sample sizes
####
# visualization 3
HH::interaction2wt(t$len ~ t$supp * t$dose) # to produce a plot of both main effects
#     and two-way interactions for any factorial design of any order
####
car::qqPlot(lm(len ~ supp * dose, ToothGrowth))
## Normality
bartlett.test(len ~  interaction(supp, dose, sep = " ", lex.order = TRUE), 
              ToothGrowth)
# interaction computes a factor which represents the interaction of the given factors. 
#     The result of interaction is always unordered.
# lex.order = TRUE, logical indicating if the order of factor concatenation should be 
#     lexically ordered.
bartlett.test(len ~ supp, t)
bartlett.test(len ~ dose, t)
#
fligner.test(len ~ interaction(supp, dose, sep = " "), ToothGrowth)
HH::hov(len ~ interaction(supp, dose, sep = " "), ToothGrowth)
## HOMOGENITY
####
contrast <- rbind("0.5 vs more" = c(2, -1, -1))
tuk <- multcomp::glht(fit, 
                      linfct = mcp(dose = "Tukey", interaction_average = TRUE))
# 
multcomp::glht(fit, 
               linfct = mcp(dose = contrast, interaction_average = TRUE))
one <- .Last.value
summary(one)
#
plot(multcomp::cld(tuk, level = 0.05, decreasing = TRUE), 
     col = col[2])
####
t <- subset(CO2, Treatment == "chilled", 
            select = c(Type, conc, uptake, Plant))
#
sapply(t, class)
summary(t)
#
t$conc <- factor(t$conc)
#
table(t$Type, t$conc) # balanced
#
aggregate(t$uptake, by = list(Type = t$Type, Conc = t$conc), func)
aggregate(t$uptake, by = list(Type = t$Type, Conc = t$conc), sd)
#
fit <- aov(uptake ~ conc * Type + Error(Plant/ (conc)), t)
summary(fit) 
# Between-subjects (or between-groups) study design: different people test each condition, 
#     so that each person is only exposed to a single user interface. Within-subjects (or repeated-measures) 
#     study design: the same person tests all the conditions (i.e., all the user interfaces).
####
# visualization
par(las = 2)
par(mar = c(10, 4, 4, 2))
interaction.plot(t$conc, t$Type,t$uptake, fun = func,
                 type = "b", col = col[1:2], pch = c(18, 19),
                 xlab = "",
                 trace.label = "Type",
                 main = "interaction plot  for plant type and  concentration")
# 
boxplot(uptake ~ Type * conc, data = t,
        col = col[1:2], xlab = "")
# When dealing with repeated measures designs, you typically need the data in long 
#     format before fitting models. In long format, each measurement of the dependent
#     variable is placed in its own row. 
par(opar)
####
t <- UScereal
t$shelf <- factor(t$shelf)
#
dependent <- cbind(t$calories, t$fat, t$sugars)
colnames(dependent) <- c("Calories", "Fat", "Sugars")
aggregate(dependent, by = list(Shelf = t$shelf), func)
# 
cov(dependent)
#
fit <- manova(dependent ~ t$shelf)
summary(fit)
#
summary.aov(fit) # print univariate results
####
# The two assumptions underlying a one-way MANOVA are multivariate normality and
#     homogeneity of variance-covariance matrices. 
# The first assumption states that the vector of dependent variables jointly follows
#     a multivariate normal distribution. 
center <- colMeans(dependent)
n <- nrow(dependent)
p <- ncol(dependent)
d <- mahalanobis(dependent, center, cov(dependent))
coord <- qqplot(qchisq(ppoints(n), df = p), 
                d,
                main = "Q-Q Plot Assessing Mutlivariate Normality",
                ylab = "Mahalanobis D2")
abline(a = 0, b = 1) # If the data follow a multivariate normal distribution, 
#     then points will fall on the line.
identify(coord$x, coord$y, labels = row.names(t))
## NORMALITY
####
# The homogeneity of variance-covariance matrices assumption requires that the 
#     covariance matrix for each group is equal. 
## HOMOGENITY
####
mvoutlier::aq.plot(dependent)
## OUTLIERS
####
# If the assumptions of multivariate normality or homogeneity of variance-covariance
#     matrices are untenable, or if you’re concerned about multivariate outliers, you may
#     want to consider using a robust or nonparametric version of the MANOVA test instead.
#     A robust version of the one-way MANOVA is provided by the Wilks.test() function in
#     the rrcov package. 
# The adonis() function in the vegan package can provide the equivalent of a nonparametric MANOVA. 
#
rrcov::Wilks.test(dependent, t$shelf, method = "mcd")
# you can see that using a robust test that’s insensitive to both outlier and violations of MANOVA 
#     assumptions still indicates that the cereals on the top, middle, and bottom store shelves 
#     differ in their nutritional profiles.
####
t <- cholesterol
levels(t$trt)
#
fit.aov <- aov(response ~ trt, t)
summary(fit.aov)
#
fit.lm <- lm(response ~ trt, t)
summary(fit.lm)
# If the factor has k levels, k – 1 contrast variables are created. 
# By default, treatment contrasts are used for unordered factors, and orthogonal polynomials
#     are used for ordered factors.
# contr.helmert: Contrasts the second level with the first, the third level with the average of the 
#     first two, the fourth level with the average of the first three, and so on.
# contr.poly: Contrasts are used for trend analysis (linear, quadratic, cubic, and so on) based 
#     on orthogonal polynomials. Use for ordered factors with equally spaced levels.
# contr.sum: Contrasts are constrained to sum to zero. Also called deviation contrasts, they 
#     compare the mean of each level to the overall mean across levels.
# contr.treatment: Contrasts each level with the baseline level (first level by default). Also called 
#     dummy coding.
# contr.SAS: Similar to contr.treatment, but the baseline level is the last level. This produces 
#     coefficients similar to contrasts used in most SAS procedures.
#
fit <- lm(response ~ trt, t, contrasts = "contr.helmert")
summary(fit)
####
























