###### Chapter 14 Principal components and factor analysis
####
# two related but distinct ways for exploring and simplifing complex multivariate
#     data are principal components and exploratory factor analysis
# principal components analysis is a data-reduction technique that transforms a 
#     a larger number of correlated variables into a much smaller set of
#     uncorrelated variables called principal components
# For example, you might use PCA to transform 30 correlated (and possibly 
#     redundant) environmental variables into 5 uncorrelated composite variables
#     that retain as much information from the original set of variables as 
#     possible.
# exploratory factor analysis (EFA) is a collection of methods designed to
#     uncover the latent structure in a given set of variables. It looks for a 
#     smaller set of underlying or latent constructs that can explain the 
#     relationships among the observe or manifest variables.
# For example, the dataset Harman74.cor contains the correlations among 24
#     psychological tests given to 145 seventh- and eighth-grade children. If
#     you apply EFA to this data, the results suggest that the 276 test 
#     intercorrelations can be explained by the children’s abilities on 4 
#     underlying factors (verbal ability, processing speed, deduction, and 
#     memory).
####
# Principal components (PC1 and PC2) are linear combinations of the observed 
#     variables (X1 to X5). The weights used to form the linear composites are 
#     chosen to maximize the variance each principal component accounts for, 
#     while keeping the components uncorrelated. 
# In contrast, factors (F1 and F2) are assumed to underlie or “cause” the 
#     observed variables, rather than being linear combinations of them. 
#     The errors (e1 to e5) represent the variance in the observed variables 
#     unexplained by the factors. The circles indicate that the factors and 
#     errors aren’t directly observable but are inferred from 
#     the correlations among the variables. In this example, the curved arrow 
#     between the factors indicates that they’re correlated. Correlated factors
#     are common, but not required, in the EFA model. 
####
# analysts used rules of thumb like “factor analysis requires 5–10 times as many
#     subjects as variables.”
# Recent studies suggest that the required sample size depends on the number of 
#     factors, the number of variables associated with each factor, and 
#     how well the set of factors explains the variance in the variables 
#     (Bandalos and BoehmKaufman, 2009). 
####
# The most common steps are as follows:
# 1 Prepare the data. Both PCA and EFA derive their solutions from the 
#     correlations among the observed variables. You can input either the raw
#     data matrix or the correlation matrix to the principal() and fa() 
#     functions. If raw data is input, the correlation matrix is automatically 
#     calculated. Be sure to screen the data for missing values before 
#     proceeding.
# 2 Select a factor model. Decide whether PCA (data reduction) or EFA 
#     (uncovering latent structure) is a better fit for your research goals. 
#     If you select an EFA approach, you’ll also need to choose a specific 
#     factoring method (for example, maximum likelihood).
# 3 Decide how many components/factors to extract. 
# 4 Extract the components/factors.
# 5 Rotate the components/factors. 
# 6 Interpret the results. 
# 7 Compute component or factor scores
####
# The goal of PCA is to replace a large number of correlated variables with a 
#     smaller number of uncorrelated variables while capturing as much 
#     information in the original variables as possible. 
# These derived variables, called principal components, are linear combinations
#     of the observed variables. Specifically, the first principal component
#     PC1 = a1X1 + a2X2 + ... + akXk is the weighted combination of the k 
#     observed variables that accounts for the most variance in the original 
#     set of variables. 
# The second principal component is the linear combination that accounts for 
#     the most variance in the original variables, under the constraint that 
#     it’s orthogonal (uncorrelated) to the first principal component. 
# Each subsequent component maximizes the variance accounted for, while at the
#     same time remaining uncorrelated with all previous components.
# Theoretically, you can extract as many principal components as there are
#     variables. But from a practical viewpoint, you hope that you can 
#     approximate the full set of variables with a much smaller set of
#     components. 
####
t <- USJudgeRatings
sapply(t, class)
# Because the goal is to simplify the data, you’ll approach this problem using
#     PCA. 
# Several criteria are available for deciding how many components to retain in a PCA.
# They include
# ■ Basing the number of components on prior experience and theory
# ■ Selecting the number of components needed to account for some threshold
#     cumulative amount of variance in the variables (for example, 80%)
# ■ Selecting the number of components to retain by examining the eigenvalues 
#     of the k × k correlation matrix among the variables -- the most common 
#     way.
#  Each component is associated with an eigenvalue of the correlation matrix.
#     The first PC is associated with the largest eigenvalue, the second PC
#     with the second-largest eigenvalue, and so on. 
# The Kaiser–Harris criterion suggests retaining components with eigenvalues 
#     greater than 1. 
# Components with eigenvalues less than 1 explain less variance than contained 
#     in a single variable. In the Cattell Scree test, the eigenvalues are 
#     plotted against their component numbers. Such plots typically demonstrate
#    a bend or elbow, and the components above this sharp break are retained. 
# Finally, you can run simulations, extracting eigenvalues from random data 
#     matrices of the same size as the original matrix. If an eigenvalue based 
#     on real data is larger than the average corresponding eigenvalues
#     from a set of random data matrices, that component is retained. 
# The approach is called parallel analysis (see Hayton, Allen, and Scarpello, 
#     2004, for more details)
require("psych")
fa.parallel(t[, -1], fa = "pc", n.iter = 100,
             show.legend = FALSE, main = "scree plot with parellel analysis")
abline(h = 1)
# Assessing the number of principal components to retain for the USJudgeRatings
#     example. A scree plot (the line with x’s), eigenvalues greater than
#     1 criteria (horizontal line), and parallel analysis with 100 simulations
#     (dashed line) suggest retaining a single component
####
pc <- principal(t[, -1], nfactors = 1)
pc
# Here, you’re inputting the raw data without the CONT variable and specifying 
#     that one unrotated component should be extracted. (Rotation is explained 
#     in section 14.3.3.) Because PCA is performed on a correlation matrix, 
#     the raw data is automatically converted to a correlation matrix before 
#     the components are extracted. 
# The column labeled PC1 contains the component loadings, which are the 
#     correlations of the observed variables with the principal component(s). 
#     If you extracted more than one principal component, there would be 
#     columns for PC2, PC3, and so on.
# Component loadings are used to interpret the meaning of components. You can
#     see that each variable correlates highly with the first component (PC1). 
#     It therefore appears to be a general evaluative dimension.
# The column labeled h2 contains the component communalities—the amount of
#     variance in each variable explained by the components. 
# The u2 column contains the component uniquenesses—the amount of variance 
#     not accounted for by the components (or 1 – h2). For example, 80% of 
#     the variance in physical ability (PHYS) ratings is accounted for by 
#     the first PC, and 20% isn’t. PHYS is the variable least well 
#     represented by a one-component solution.
# The row labeled SS Loadings contains the eigenvalues associated with the 
#     components. The eigenvalues are the standardized variance associated 
#     with a particular component (in this case, the value for the first
#     component is 10). Finally, the row labeled Proportion Var represents the 
#     amount of variance accounted for by each component.
# Here you see that the first principal component accounts for 92% of the 
#     variance in the 11 variables.
####
t2 <- Harman23.cor
fa.parallel(t2$cov, n.obs = t2$n.obs, fa = "pc", n.iter = 100, 
            show.legend = FALSE, se.bars = TRUE, 
            main = "scree plot with parallel analysis")
abline(h = 1)
# Sharp breaks in the plot suggest the appropriate number of components or 
#     factors to extract.
# “Parallel" analyis is an alternative technique that compares the scree of 
#     factors of the observed data with that of a random data matrix of the 
#     same size as the original. This may be done for continuous, dichotomous, 
#     or polytomous data using Pearson, tetrachoric or polychoric correlations.
#  This won’t always be the case, and you may need to extract different numbers 
#     of components and select the solution that appears most useful. 
pc <- principal(t2$cov, nfactors = 2, rotate = "none")
pc
# you see that the first component accounts for 58% of the variance in the 
#     physical measurements, whereas the second component accounts for 22%. 
#     Together, the two components account for 81% of the variance. The two 
#     components together account for 88% of the variance in the height 
#     variable.
# Components and factors are interpreted by examining their loadings. The first
#     component correlates positively with each physical measure and appears to 
#     a general size factor. The second component contrasts the first four 
#     variables (height, arm. span, forearm, and lower leg), with the second 
#     four variables (weight, bitro diameter, chest girth, and chest width). 
#     It therefore appears to be a length-versus-volume factor.
# Conceptually, this isn’t an easy construct to work with. Whenever two or more 
#   components have been extracted, you can rotate the solution to make it 
#   more interpretable.
####
# Rotations are a set of mathematical techniques for transforming the component 
#     loading matrix into one that’s more interpretable. 
# They do this by “purifying” the components as much as possible. Rotation 
#     methods differ with regard to whether the resulting components remain 
#     uncorrelated (orthogonal rotation) or are allowed to correlate (oblique 
#     rotation). 
# They also differ in their definition of purifying. The most popular 
#     orthogonal rotation is the varimax rotation, which attempts to purify the
#     columns of the loading matrix, so that each component is defined by a
#     limited set of variables (that is, each column has a few large loadings
#     and many very small loadings). 
# Applying a varimax rotation to the body measurement data, you get the results 
#     provided in the next listing. 
rc <- principal(t2$cov, nfactors = 2, rotate = "Varimax")
rc
pc
# The column names change from PC to RC to denote rotated components. Looking at
#     the loadings in column RC1, you see that the first component is primarily 
#     defined by the first four variables (length variables). The loadings in 
#     the column RC2 indicate that the second component is primarily defined 
#     by variables 5 through 8 (volume variables). 
# Note that the two components are still uncorrelated and that together, they
#     still explain the variables equally well. You can see that the rotated 
#     solution explains the variables equally well because the variable 
#     communalities haven’t changed. Additionally, the cumulative variance 
#     accounted for by the two-component rotated solution (81%) hasn’t changed. 
# But the proportion of variance accounted for by each individual component has 
#     changed (from 58% to 44% for component 1 and from 22% to 37% for component
#     2). 
# This spreading out of the variance across components is common, and 
#     technically you should now call them components rather than 
#     principal components (because the variance-maximizing properties of 
#     individual components haven’t been retained).
# The ultimate goal is to replace a larger set of correlated variables with a 
#     smaller set of derived variables. To do this, you need to obtain scores 
#     for each observation on the components.
####
pc <- principal(t[, -1], nfactors = 1, scores = TRUE)
head(pc$scores)
# The principal component scores are saved in the scores element of the object
#     returned by the principal() function when the option scores=TRUE.
cor(t$CONT, pc$scores) # you could now get the correlation between the number 
#     of contacts occurring between a lawyer and a judge and their evaluation 
#     of the judge using
####
# When the principal components analysis is based on a correlation matrix and 
#     the raw data aren’t available, getting principal component scores for 
#     each observation is clearly not possible. But you can get the coefficients
#     used to calculate the principal components. 
rc <- principal(t2$cov, nfactors = 2, rotate = "varimax")
round(unclass(rc$weights), 2)
# The component scores are obtained using the formulas 
# PC1 = 0.28*height + 0.30*arm.span + 0.30*forearm + 0.29*lower.leg - 
#   0.06*weight - 0.08*bitro.diameter - 0.10*chest.girth - 
#   0.04*chest.width 
# and
# PC2 = -0.05*height - 0.08*arm.span - 0.09*forearm - 0.06*lower.leg + 
#   0.33*weight + 0.32*bitro.diameter + 0.34*chest.girth + 
#   0.27*chest.width
# These equations assume that the physical measurements have been standardized
#     (mean = 0, sd = 1). Note that the weights for PC1 tend to be around 0.3 
#     or 0. The same is true for PC2. 
# As a practical matter, you could simplify your approach further by taking
#     the first composite variable as the mean of the standardized scores for
#     the first four variables. 
# Similarly, you could define the second composite variable as the mean of the
#     standardized scores for the second four variables. This is typically 
#     what I’d do in practice.
####
# If your goal is to look for latent underlying variables that explain your 
#     observed variables, you can turn to factor analysis. 
# The goal of EFA is to explain the correlations among a set of observed 
#     variables by uncovering a smaller set of more fundamental unobserved 
#     variables underlying the data. These hypothetical, unobserved variables 
#     are called factors. (Each factor is assumed to explain the variance 
#     shared among two or more observed variables, so technically, they’re 
#     called common factors.)
# The model can be represented as Xi = a1F1 + a2F2 + ... + apFp + Ui
# where Xi is the ith observed variable (i = 1…k) 
# Fj are the common factors (j = 1…p), and p < k. 
# Ui is the portion of variable Xi unique to that variable (not explained by 
#     the common factors). 
# The ai can be thought of as the degree to which each factor contributes to 
#     the composition of an observed variable. 
####
# you’ll apply EFA to the correlations among six psychological tests. One 
#     hundred twelve individuals were given six tests, including a nonverbal
#     measure of general intelligence (general), a picture-completion test 
#     (picture), a block design test (blocks), a maze test (maze), a reading
#     comprehension test (reading), and a vocabulary test (vocab). 
# Can you explain the participants’ scores on these tests with a smaller
#     number of underlying or latent psychological constructs?
covariance <- ability.cov$cov
correlation <- cov2cor(covariance)
# Because you’re looking for hypothetical constructs that explain the data, 
#     you’ll use an EFA approach. As in PCA, the next task is to decide how 
#     many factors to extract. 
fa.parallel(correlation, n.obs = 112, fa = "both", n.iter = 100,
            main = "scree plots with parallel analysis")
# When in doubt, it’s usually a better idea to overfactor than to underfactor. 
#     Overfactoring tends to lead to less distortion of the “true” solution. 
# For EFA, the Kaiser–Harris criterion is number of eigenvalues above 0, rather
#     than 1. (Most people don’t realize this, so it’s a good way to win bets
#     at parties.) In the present case the Kaiser–Harris criteria
#     also suggest two factors.
####
# Unlike PCA, there are many methods of extracting common factors. They include
#     maximum likelihood (ml), iterated principal axis (pa), weighted least 
#     square (wls), generalized weighted least squares (gls), and minimum 
#     residual (minres). 
# Statisticians tend to prefer the maximum likelihood approach because of its
#     well-defined statistical model.
# Sometimes, this approach fails to converge, in which case the iterated 
#     principal axis option often works well. 
fa <- fa(correlation, nfactors = 2, rotate = "none", fm = "pa")
fa
ra <- fa(correlation, nfactors = 2, rotate = "varimax", fm = "pa")
ra # Looking at the factor loadings, the factors are certainly easier to 
#     interpret. Reading and vocabulary load on the first factor; and picture
#     completion, block design, and mazes load on the second factor. The general 
#     nonverbal intelligence measure loads on both factors. This may indicate
#     a verbal intelligence factor and a nonverbal intelligence factor.
ra.promax <- fa(correlation, nfactors = 2, rotate = "promax", fm = "pa")
ra.promax # By using an orthogonal rotation, you artificially force the two 
#     factors to be uncorrelated. What would you find if you allowed the two
#     factors to correlate? You can try an oblique rotation such as promax.
####
# In an orthogonal solution, attention focuses on the factor structure matrix 
#     (the correlations of the variables with the factors). 
# In an oblique solution, there are three matrices to consider: the factor 
#     structure matrix, the factor pattern matrix, and the factor 
#     intercorrelation matrix. 
# The factor pattern matrix is a matrix of standardized regression coefficients.
#     They give the weights for predicting the variables from the factors. 
# The factor intercorrelation matrix gives the correlations among the factors. 
# the values in the PA1 and PA2 columns constitute the factor pattern matrix. 
#     They’re standardized regression coefficients rather than correlations. 
# Examination of the columns of this matrix is still used to name the factors 
#     (although there’s some controversy here). Again, you’d find a verbal and
#     nonverbal factor. 
# The factor intercorrelation matrix indicates that the correlation between the
#     two factors is 0.57. This is a hefty correlation. If the factor 
#     intercorrelations had been low, you might have gone back to an orthogonal
#     solution to keep things simple.
# The factor structure matrix (or factor loading matrix) isn’t provided. 
#     But you can easily calculate it using the formula F = P*Phi, where F is 
#     the factor loading
fsm <- function(oblique) {
  if (class(oblique)[2] == "fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    p <- unclass(oblique$loading)
    F <- p %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  }
}
fsm2 <- fsm(ra.promax) # Now you can review the correlations between the variables and
#     the factors. Comparing them to the factor loading matrix in the 
#     orthogonal solution, you see that these columns aren’t as pure. This is 
#     because you’ve allowed the underlying factors to be correlated. Although 
#     the oblique approach is more complicated, it’s often a more realistic 
#     model of the data.
####
factor.plot(ra.promax, labels = rownames(ra.promax$loadings))
####
fa.diagram(ra.promax, simple = FALSE) # if you let simple = TRUE, only the 
#     largest loading per item is displayed. It shows the largest loadings 
#     for each factor, as well as the produces correlations between the factors. 
#     This type of diagram is helpful when there are several factors.
fa.diagram(ra.promax, simple = TRUE)
####
t3 <- Harman74.cor
fa.parallel(t3$cov, n.obs = t3$n.obs, fa = "both", n.iter = 100) # facotr is 4
ra <- fa(t3$cov, nfactors = 4, rotate = "promax", fm = "pa")
#
fsm3 <- function(oblique) {
  if (class(oblique)[2] == "fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    p <- unclass(oblique$loading)
    F <- p %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2", "PA3", "PA4")
    return(F)
  }
}
fsm3(ra)
#
factor.plot(ra, labels = rownames(ra$loadings))
#
fa.diagram(ra, simple = TRUE)
####
fa$weights #  Additionally, the scoring coefficients (standardized regression 
#     weights) are available in the weights element of the object returned.
# Unlike component scores, which are calculated exactly, factor scores can only
#     be estimated. Several methods exist. 
####
#  In EFA, you allow the data to determine the number of factors to be extracted
#     and their meaning. But you could start with a theory about how many 
#     factors underlie a set of variables, how the variables load on those 
#     factors, and how the factors correlatewith one another. You could then
#     test this theory against a set of collected data. The approach is called 
#     confirmatory factor analysis (CFA)
# CFA is a subset of a methodology called structural equation modeling (SEM). 
#     SEM allows you to posit not only the number and composition of underlying
#     factors but also how these factors impact one another. You can think of 
#     SEM as a combination of confirmatory factor analyses (for the variables)
#     and regression analyses (for the factors). The resulting output includes
#     statistical tests and fit indices. There are several excellent packages 
#     for CFA and SEM in R. They include sem, OpenMx, and lavaan. 
# Finally, R contains numerous methods for multidimensional scaling (MDS). 
#     MDS is designed to detect underlying dimensions that explain the 
#     similarities and distances between a set of measured objects 
#     (for example, countries).
####
#  PCA is a useful data-reduction method that can replace a large number of 
#     correlated variables with a smaller number of uncorrelated variables, 
#     simplifying the analyses. 
# EFA contains a broad range of methods for identifying latent or unobserved 
#     constructs (factors) that may underlie a set of observed or manifest 
#     variables.
#  Whereas the goal of PCA is typically to summarize the data and reduce its 
#     dimensionality, EFA can be used as a hypothesis-generating tool, 
#     useful when you’re trying to understand the relationships between a large
#     number of variables. It’s often used in the social sciences for theory 
#     development. 
####










































































































