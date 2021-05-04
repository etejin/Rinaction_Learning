###### Chapter 18 Advanced methods for missing data
####
# You can eliminate missing data by (1) removing cases with missing data or 
#     (2) replacing miss-ing data with reasonable substitute values. 
# A comprehensive approach usually includes the following steps: 
# 1 Identify the missing data.
# 2 Examine the causes of the missing data.
# 3 Delete the cases containing missing data, or replace (impute) the missing 
#     values with reasonable alternative data values. 
# Deciding how to treat missing values will depend on your estimation of
#     which procedures will produce the most reliable and accurate results. 
####
# Statisticians typically classify missing data into one of three types. These types 
#     are usually described in probabilistic terms, but the underlying ideas
#     are straightforward.
# 1. Missing complete at random: If the presence of missing data on a
#     variable is unrelated to any other observed or unobserved variable, 
#     then the data are missing completely at random (MCAR). if every variable
#     with missing data is MCAR, you can consider the complete cases to be a 
#     simple random sample from the larger dataset. 
# 2. Missing at random: If the presence of missing data on a variable is
#     related to other observed variables but not to its own unobserved value,
#     the data are missing at random (MAR).In this case, the presence
#     or absence of dream sleep data is random, once you control for body
#     weight.
# 3. Not missing at random: If the missing data for a variable are neither
#     MCAR nor MAR, the data are not missing at random (NMAR). 
####
# Most approaches to missing data assume that the data are either MCAR or MAR.
#     In this case, you can ignore the mechanism producing the missing data
#     and (after re placing or deleting the missing data) model the 
#     relationships of interest directly. 
# Data that are NMAR can be difficult to analyze properly. When data are NMAR,
#     you have to model the mechanisms that produced the missing values, as
#     well as the relationships of interest. (Current approaches to analyzing
#     NMAR data include the use of selection models and pattern mixtures. The
#     analysis of NMAR data can be complex and is beyond the scope of this
#     book.)
####
# the three most popular methods for dealing with incomplete data (a 
#     rational approach, listwise deletion, and multiple imputation)
####
# missing values using the symbol NA (not available) 
# impossible values using the symbol NaN (not a number).
# the symbols Inf and -Inf represent poitive infinity and negative infinity
# The functions is.na(), is.nan(), and is.infinite() can be used to identify
#     missing, impossible, and infinite values, respectively. Each returns 
#     either TRUE or FALSE. 
data(sleep, package = "VIM")
#
sleep[complete.cases(sleep), ]
sleep[!complete.cases(sleep),]
##  Because the logical values TRUE and FALSE are equivalent to the numeric
#     values 1 and 0, the sum() and mean() functions can be used to obtain
#     useful information about missing data. 
sum(is.na(sleep$Dream))
func(is.na(sleep$Dream))
func(!complete.cases(sleep))
# There are two things to keep in mind when identifying missing values. 
# First, the complete.cases() function only identifies NA and NaN as missing.
#     Infinite values (Inf and –Inf) are treated as valid values. 
# Second, you must use missing-values functions, like those in this section,
#     to identify the missing values in R data objects. Logical comparisons
#     such as myvar == NA are never true. 
####
# we’ll review tabular, graphical, and correlational methods for exploring
#     missing value patterns.
# understand why the data are missing. The answer will have implications 
#     for how you proceed with further analyses.
####
require("mice")
t <- VIM::sleep
md.pattern(t) # produces a tabulation of the missing data patterns in a 
#     matrix or data frame
# The 1s and 0s in the body of the table indicate the missing-values patterns,
#     with a 0 indicating a missing value for a given column variable and a 1
#     indicating a non-missing value.
# The first row describes the pattern of “no missing values” (all elements 
#     are 1). 
# The second row describes the pattern “no missing values except for Dream and
#     NonD.” 
# The first column indicates the number of cases in each missing data pattern,
#     the last column indicates the number of variables with missing values 
#     present in each pattern.
# Here you can see that there are 42 cases without missing data and 2 cases 
#     that are missing Span alone. 
# Nine cases are missing both NonD and Dream values. The dataset
#     has a total of (42 × 0) + (2 × 1) + … + (1 × 3) = 38 missing values. 
# The last row gives the total number of missing values on each variable
####
require("VIM")
aggr(sleep, prop = FALSE, numbers = TRUE) # plots the number of missing 
#     values for each variable alone and for each combination of variables
# You can see that the variable NonD has the largest number of missing
#     values (14), and that two mammals are missing NonD, Dream, and Sleep 
#     scores. Forty-two mammals have no missing data.
aggr(t, prop = TRUE, numbers = TRUE)
####
matrixplot(t, interactive = TRUE) # produces a plot displaying the data 
#     for each case. 
# Here, the numeric data are rescaled to the interval [0, 1] and represented
#     by grayscale colors, with lighter colors representing lower values and
#     darker colors representing larger values. 
# By default, missing values are represented in red. 
####
marginplot(t[c("Gest", "Dream")], pch = 20,
           col = col[1:3])
#  The body of the graph displays the scatter plot between Gest and Exp 
#     (based on complete cases for the two variables). In the left margin,
#     box plots display the distribution of Dream for mammals with (red) and
#     without (green) Gest values. 
# (Note that in grayscale, red is the darker shade.) 
# Four green dots represent the values of Exp for mammals missing Gest 
#     scores. 
# In the bottom margin, the roles of Gest and Dream are reversed. You can see 
#     that a negative relationship exists between length of gestation and
#     dream sleep and that dream sleep tends to be higher for mammals that 
#     are missing a gestation score. 
# The number of observations missing values on both variables at the
#     same time is printed in blue at the intersection of both margins
#     (bottom left).
####
# You can replace the data in a dataset with indicator variables, coded 1 for
#     missing and 0 for present. The resulting matrix is sometimes called a 
#     shadow matrix. 
# Correlating these indicator variables with each other and with the original
#     (observed) variables can help you to see which variables tend to be 
#     missing together, as well as relationships between a variable’s
#     “missingness” and the values of the other variables.
x <- as.data.frame(abs(is.na(t)))
head(x, 2)
head(t, 2)
#
y <- x[which(apply(x, 2, sum) > 0)] # extracts the variables that have some
#     (but not all) missing value
cor(y) # Here, you can see that Dream and NonD tend to be missing 
#     together (r = 0.91). To a lesser extent, Sleep and NonD tend to be 
#     missing together (r = 0.49) and Sleep and Dream tend to be missing 
#     together (r = 0.20).
cor(t, y, use = "pairwise.complete.obs") # relationship between missing 
#     values in a variable and the observed values on other variables
# In this correlation matrix, the rows are observed variables, and the 
#     columns are indicator variables representing missingness. 
# From the first column of the correlation matrix, you can see that 
#     nondreaming sleep scores are more likely to be missing for mammals with
#     higher body weight (r = 0.227), gestation period (r = 0.202), and 
#     sleeping exposure (r = 0.245). 
# Other columns are read in a similar fashion. None of the correlations in 
#     this table are particularly large or striking, which suggests that the
#     data deviate minimally from MCAR and may be MAR. 
####
# Note that you can never rule out the possibility that the data are NMAR, 
#     because you don’t know what the values would have been for data that
#     are missing. For example, you don’t know if there’s a relationship 
#     between the amount of dreaming a mammal engages in and the probability
#     of a value being missing on this variable. In the absence of strong 
#     external evidence to the contrary, we typically assume that data are 
#     either MCAR or MAR. 
####
# You can identify the amount, distribution, and pattern of missing data in 
#     order to evaluate the potential mechanisms leading to the missing data
#     and the impact of the missing data on your ability to answer 
#     substantive questions. 
# Answers to these questions help determine which statistical methods are 
#     most appropriate for analyzing your data. 
####
# If you can assume that the data are either MCAR or MAR, you may be able to
#     apply multiple imputation methods to arrive at valid conclusions. 
# If the data are NMAR, you can turn to specialized methods, collect new data,
#     or go into an easier and more rewarding profession.
####
# In a recent survey employing paper questionnaires, I found that several 
#     items tended to be missing together. It became apparent that these 
#     items clustered together because participants didn’t realize that the
#     third page of the questionnaire had a reverse side—which contained the 
#     items. In this case, the data could be considered MCAR
# In another study, an education variable was frequently missing in a global
#     survey of leadership styles. Investigation revealed that European 
#     participants were more likely to leave this item blank. It turned out 
#     that the categories didn’t make sense for participants in certain 
#     countries. In this case, the data were most likely MAR.
# Finally, I was involved in a study of depression in which older patients 
#     were more likely to omit items describing depressed mood when 
#     compared with younger patients. Interviews revealed that older 
#     patients were loath to admit to such symptoms because doing so violated
#     their values about keeping a “stiff upper lip.” Unfortunately, it was 
#     also determined that severely depressed patients were more likely to 
#     omit these items due to a sense of hopelessness and difficulties with 
#     concentration. In this case, the data had to be considered NMAR.
####
# In a rational approach, you use mathematical or logical relationships 
#     among variables to attempt to fill in or recover missing values. 
# I used this assumption to create a gender-lookup table for gender-specific 
#     first names. Using this lookup table for participants with missing 
#     gender values, I was able to recover 7,000 cases (63% of the missing
#     responses)
# A rational approach typically requires creativity and thoughtfulness, 
#     along with a degree of data-management skill. Data recovery may be 
#     exact (as in the sleep example) or approximate 
#     (as in the gender example)
####
# In complete-case analysis, only observations containing valid data values
#     on every variable are retained for further analysis. Practically, this
#     involves deleting any row with one or more missing values, and is also
#     known as listwise, or case-wise, deletion. 
newdata <- t[complete.cases(t), ]
newdata <- na.omit(t) # these two are the same
#
cor(na.omit(t))
cor(t, use = "complete.obs") # these two are the same
#
fit <- lm(Dream ~ Span + Gest, data = na.omit(t))
summary(fit)
####
#  Listwise deletion assumes that the data are MCAR (that is, the complete
#     observations are a random subsample of the full dataset). 
#  To the degree that the MCAR assumption is violated, the resulting 
#     regression parameters will be biased. Deleting all observations with
#     missing data can also reduce statistical power by reducing the 
#     available sample size. 
####
# Multiple imputation (MI) provides an approach to missing values that’s 
#     based on repeated simulations. MI is frequently the method of choice 
#     for complex missing-values problems. 
#  In MI, a set of complete datasets is generated from an existing dataset
#     that’s missing values. 
# Monte Carlo methods are used to fill in the missing data in each of the 
#     simulated datasets. Standard statistical methods are applied to
#     each of the simulated datasets, and the outcomes are combined to 
#     provide estimated results and confidence intervals that take into 
#     account the uncertainty introduced by the missing values. 
# Good implementations are available in R through the Amelia,
#     mice, and mi packages. 
####
# mice (multivariate imputation by chained equations) package. 
# The function mice() starts with a data frame that’s missing data and 
#     returns an object containing several complete datasets (the default is 
#     five). Each complete dataset is created by imputing values for the 
#     missing data in the original data frame. There’s a random component
#     to the imputations, so each complete dataset is slightly different. 
# The with() function is then used to apply a statistical model (for example, 
#     a linear or generalized linear model) to each complete dataset in turn.
# Finally, the pool() function combines the results of these separate
#     analyses into a single set of results. The standard errors and
#     p-values in this final model correctly reflect the uncertainty 
#     produced by both the missing values and the multiple imputations
####
# Missing values are imputed by Gibbs sampling. By default, each variable 
#     with missing values is predicted from all other variables in the 
#     dataset. 
# These prediction equations are used to impute plausible values for the
#     missing data. The process iterates until convergence over the missing 
#     values is achieved. For each variable, you can choose the form of the
#     prediction model (called an elementary imputation method) and the
#     variables entered into it. 
# By default, predictive mean matching is used to replace missing data on
#     continuous variables, whereas logistic or polytomous logistic regression 
#     is used for target variables that are dichotomous (factors with two 
#     levels) or polytomous (factors with more than two levels), respectively.
# Other elementary imputation methods include Bayesian linear regression,
#     discriminant function analysis, two-level normal imputation, and 
#     random sampling from observed values. 
####
require("mice")
imp <- mice(t, seed = 1234)
fit <- with(imp, lm(Dream ~ Span + Gest))
pooled <- pool(fit)
summary(pooled)
pooled
# Here, you see that the regression coefficient for Span isn’t significant 
#     (p ≅ 0.103), and the coefficient for Gest is significant at the p < 0.01
#     level
# the current analysis is based on information gathered from the full set of
#     62 mammals. 
# By the way, the fmi column reports the fraction of missing information
#     (that is, the proportion of variability that is attributable to the 
#     uncertainty introduced by the missing data).
####
imp 
# "pmm" represents the predictive mean method
# predictor matrix indicates that each variable with missing data was 
#     imputed using all the other variables in the dataset. 
# In this matrix, the rows represent the variables being imputed, the columns
#     represent the variables used for the imputation, and 1s/0s indicate 
#     used/not used
imp$imp$Dream # get the imputation of one variable for all five imputations
# 
complete(imp, action = 3) # get the third (third out of five) imputation for
#     all variables
####
# In pairwise deletion, observations are deleted only if they’re missing
#     data for the variables involved in a specific analysis. 
cor(t, use = "complete.obs")
cor(t, use = "pairwise.complete.obs") # correlations between any two 
#     variables use all available observations for those two variables
#     (ignoring the other variables). 
# The correlation between Kaplan-Meier multiple is based on all 62 mammals
#     (the number of mammals with data on both variables). The correlation 
#     between Kaplan-Meier multiple is based on 42 mammals, and the 
#     correlation between Kaplan-Meier multiple is based on 46 mammals. 
# Although pairwise deletion appears to use all available data, in fact each
#     calculation is based on a different subset of the data. This can lead 
#     to distorted and difficult-to-interpret results.
####
# In simple imputation, the missing values in a variable are replaced with
#     a single value (for example, mean, median, or mode). 
# Using mean substitution, you could replace missing values on Kaplan-Meier
#     multiple with the value 1.97 and missing values on Kaplan-Meier 
#     multiple with the value 8.67 (the means on Kaplan-Meier multiple, 
#     respectively).
# Note that the substitution is nonstochastic, meaning that random error 
#     isn’t introduced (unlike with multiple imputation)
#  An advantage of simple imputation is that it solves the missing-values 
#       problem without reducing the sample size available for analyses. 
# it produces biased results for data that isn’t MCAR. 
####






