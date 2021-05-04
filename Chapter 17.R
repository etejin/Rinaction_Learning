###### Chapter 17 Classification
# Each of these cases involves the prediction of a binary categorical outcome 
#     (good credit risk/bad credit risk, heart attack/no heart attack, 
#     spam/not spam) from a set of predictors (also called features). 
# The goal is to find an accurate method of classifying new cases into one of 
#     the two groups. 
#  The field of supervised machine learning offers numerous classification 
#     methods that can be used to predict categorical outcomes, including 
#     logistic regression, decision trees, random forests, support vector 
#     machines, and neural networks. 
####
#  Supervised learning starts with a set of observations containing values 
#     for both the predictor variables and the outcome. The dataset is then
#     divided into a training sample and a validation sample. 
# A predictive model is developed using the data in the training sample and 
#     tested for accuracy using the data in the validation sample. Both 
#     samples are needed because classification techniques maximize 
#     prediction for a given set of data.
# Estimates of their effectiveness will be overly optimistic if they’re 
#     evaluated using the same data that generated the model. 
# By applying the classification rules developed on a training sample to a 
#     separate validation sample, you can obtain a more realistic accuracy 
#     estimate. Once you’ve created an effective predictive model, you can
#     use it to predict outcomes in situations where only the predictor 
#     variables are known.
####
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep = "")
breast <- read.table(url, sep = ",", header = FALSE, na.strings = "?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity", 
                   "shapeUniformity", "maginalAdhesion", 
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")
#
df <- breast[-1]
df$class <- factor(df$class, levels = c(2, 4),
                   labels = c("benign", "malignant"))
#
set.seed(1234)
train <- sample(nrow(df), 0.7 * nrow(df))
df.train <- df[train, ]
df.test <- df[-train,]
#
table(df.train$class)
table(df.test$class)
####
# Logistic regression is a type of generalized linear model that is often
#     used to predict a binary outcome from a set of numeric variables 
fit.logit <- glm(class ~., data = df.train, family = binomial())
summary(fit.logit)
# 
prob <- predict(fit.logit, df.test, type = "response")
logit.pred <- factor(prob > 0.5, levels = c(FALSE, TRUE),
                     labels = c("benign", "malignant"))
logit.pref <- table(df.test$class, logit.pred, 
                    dnn = c("Actual", "Predicted"))
#  a cross-tabulation of actual status and predicted status 
#     (called a confusion matrix) is printed. It shows that 118 cases that 
#     were benign were classified as benign, and 76 cases that were malignant
#     were classified as malignant. Ten cases in the df.validate data frame
#     had missing predictor data and could not be included in the evaluation. 
accuracy <- (118 + 76) / (118 + 76 + 4 + 2) # [1] 0.97
####
#  In a prediction context, it’s often useful to remove such variables from
#     the final model. This is especially important in situations where a 
#     large number of non-informative predictor variables are adding what 
#     is essentially noise to the system.
fit.logit.reduced <- step(fit.logit, trace = 0) 
# Predictor variables are added or removed in order to obtain a model with 
#     a smaller AIC value. 
prob.reduced <- predict(fit.logit.reduced, df.test, type = "response")
prob.reduced <- factor(prob.reduced > 0.5, levels = c(FALSE, TRUE),
                            labels = c("benign", "malignant"))
table(df.test$class, prob.reduced, dnn = c("Actual", "Predicte)d"))
#
accuracy <- (118 + 77) / (118 + 77 + 3 +2) # 0.975
####
# decision trees involve creating a set of binary splits on the predictor 
#     variables in order to create a tree that can be used to classify new
#     observations into one of two groups
# classical trees and conditional trees
####
# process of building a classical decision trees:
# 1. choose the predictor variable that best splits the data into two groups such
#     that the purity (homogeneity of the outcome in the two groups is 
#     maximized), that is as many benign cases in one group and malignant cases
#     in the other possible.IF the predictor is continuous, choose a cut-point
#     that maximizes purity for the two groups created. IF categorical, combine
#     the categories to two groups within the maximum purity
# 2. separate teh data into these two groups, and continue the process for each 
#     each group
# 3. repeat 1 and 2 until a subgroup contains fewer than a minimum number of
#     observations or no splits decrease the impurity beyond a specified
#     threshold. the subgroup in the final set are called terminal nodes. each
#     terminal node is classified as one category of teh outcome or the other 
#     based on teh most frequent value of teh outcome for the sample in taht
#     node.
# 4. to classify a case, run it down the tree to a terminal node, and assign it 
#     the modal outcome value assigned in step 3.
# to compensate, you can prune back teh tree by choosing teh tree with the lowest
#     10-fold cross-validated prediction error.
# then this pruned tree is then used for future predictions
####
require("rpart")
set.seed(1234)
dtree <- rpart(class ~ ., data = df.train, method = "class", 
               parms = list(split = "information"))
# 
dtree$cptable # to choose the final size
# complexity parameter (cp) is used to penalize large trees
# tree size is defined by the number of branch splits (nsplit), a tree with
#     n splits, has n + 1 terminal nodes
# real error : the error rate for a tree of a given size in teh training sample
# xerror: cross-validated error is based on 10-fold cross validation, using 
#     training sample
# xstd: teh standard error of teh cross-validation errpr
plotcp(dtree)
# A good choice for the final tree size is the smallest tree whose 
#     cross-validated error is within one standard error of the minimum 
#     cross-validated error value.
# The minimum cross-validated error is 0.18 with a standard error of 0.0326.
#     In this case, the smallest tree with a cross-validated error within 
#     0.18 ± 0.0326 (that is, between 0.15 and 0.21) is selected. 
# Complexity parameter vs. cross-validated error. The dotted line is 
#     the upper limit of the one standard deviation rule (0.18 + 1 * 0.0326
#     = .21). The plot suggests selecting the tree with the leftmost cp value
#     below the line. in other words, choosing tree size associated with the 
#     largest complexity parameter below the line 
dtree.pruned <- prune(dtree, cp = 0.0125) # uses the complexity parameter to 
#     cut back a tree to the desired size. It takes the full tree and snips
#     off the least important splits based on the desired complexity parameter. 
#
require("rpart.plot")
prp(dtree.pruned, type = 1, extra = 104, 
    fallen.leaves = TRUE, main = "descision tree")
# Each node contains the probability of the classes in that node, along with 
#     the percentage of the sample.
#
dtree.pred <- predict(dtree.pruned, df.test, type = "class")
dtree.perf <- table(df.test$class, dtree.pred,
                    dnn = c("Actual", "Predicted"))
dtree.perf
#
accuracy <- (122 + 79) / (122 + 79 + 7 + 2) # [1] 0.9571429
# Unlike the logistic regression example, all 210 cases in the validation 
#     sample could be classified by the final tree. 
# Note that decision trees can be biased toward selecting predictors that 
#     have many levels or many missing values.
####
# let’s look at an important variant of the traditional decision tree called
#     a conditional inference tree. Conditional inference trees are similar 
#     to traditional trees, but variables and splits are selected based on 
#     significance tests rather than purity/homogeneity measures. 
# The significance tests are permutation tests. 
# In this case, the algorithm is as follows:
# 1 Calculate p-values for the relationship between each predictor and the 
#     outcome variable.
# 2 Select the predictor with the lowest p-value.
# 3 Explore all possible binary splits on the chosen predictor and dependent 
#     variable (using permutation tests), and pick the most significant split.
# 4 Separate the data into these two groups, and continue the process for 
#     each subgroup.
# 5 Continue until splits are no longer significant or the minimum node size
#     is reached.
####
require("party")
fit.ctree <- ctree(class ~ ., data = df.train)
plot(fit.ctree, main = "conditional inference tree")
#  The shaded area of each node represents the proportion of malignant cases
#     in that node.
#
ctree.pred <- predict(fit.ctree, df.test, type = "response")
ctree.perf <- table(df.test$class, ctree.pred, dnn = c("Actual", "Predicted"))
# 
accuracy <- (122 + 78) / (122 + 78 + 7 + 3) # [1] 0.952381
####
require("partykit")
as.party(dtree.pruned)
p <- .Last.value
plot(p)
####
# A random forest is an ensemble learning approach to supervised learning. 
#     Multiple predictive models are developed, and the results are 
#     aggregated to improve classification rates. 
#  The algorithm for a random forest involves sampling cases and variables to
#     create a large number of decision trees. Each case is classified by 
#     each decision tree. 
# The most common classification for that case is then used as the outcome.
####
# Assume that N is the number of cases in the training sample and M is the
#     number of variables. Then the algorithm is as follows:
# 1 Grow a large number of decision trees by sampling N cases with 
#    replacement from the training set.
# 2 Sample m < M variables at each node. These variables are considered 
#     candidates for splitting in that node. The value m is the same for 
#     each node.
# 3 Grow each tree fully without pruning (the minimum node size is set to 1).
# 4 Terminal nodes are assigned to a class based on the mode of cases in that
#     node.
# 5 Classify new cases by sending them down all the trees and taking a 
#     vote—majority rules.
####
# An out-of-bag (OOB) error estimate is obtained by classifying the cases 
#     that aren’t selected when building a tree, using that tree. This is an
#     advantage when a validation sample is unavailable. 
# Random forests also provide a natural measure of variable importance
# The default number of trees is 500, the default number of variables
#     sampled at each node is sqrt(M), and the minimum node size is 1
####
require("randomForest")
set.seed(1234)
fit.forest <- randomForest(class ~ ., data = df.train,
                           na.action = na.roughfix,
                           importance = TRUE)
fit.forest
importance(fit.forest, type = 2) # The relative importance measure specified 
#     by the type=2 option is the total decrease in node impurities 
#     (heterogeneity) from splitting on that variable, averaged over all
#     trees
# Node impurity is measured with the Gini coefficient. sizeUniformity is the 
#     most important variable and mitosis is the least important. 
#
forest.pred <- predict(fit.forest, df.test, type = "class")
forest.perf <- table(df.test$class, forest.pred, dnn = c("Actual", "Predicted"))
accuracy <- (117 + 79) / (117 + 79 + 1 + 4) # [1] 0.9751244
####
# If predictor variables are highly correlated, a random forest using 
#     conditional inference trees may provide better predictions
#  Random forests tend to be very accurate compared with other classification
#     methods. 
# Additionally, they can handle large problems (many observations and 
#     variables), can handle large amounts of missing data in the training 
#     set, and can handle cases in which the number of variables is much
#     greater than the number of observations. 
# The provision of OOB error rates and measures of variable importance are
#     also significant advantages.
# A significant disadvantage is that it’s difficult to understand the
#     classification rules (there are 500 trees!) and communicate them to
#     others. Additionally, you need to store the entire forest in order to 
#     classify new cases
####
# Support vector machines (SVMs) are a group of supervised machine-learning 
#     models that can be used for classification and regression. 
# SVMs seek an optimal hyperplane for separating two classes in a 
#     multidimensional space. The hyperplane is chosen to maximize the margin 
#     between the two classes’ closest points. The points on the boundary of
#     the margin are called support vectors (they help define the margin), 
#     and the middle of the margin is the separating hyperplane.
# For an N-dimensional space (that is, with N predictor variables), the 
#     optimal hyperplane (also called a linear decision surface) has 
#     N – 1 dimensions. If there are two variables, the surface is a line. 
#     For three variables, the surface is a plane. For 10 variables,
#     the surface is a 9-dimensional hyperplane. 
####
# The optimal hyperplane is identified using quadratic programming to 
#     optimize the margin under the constraint that the data points on one 
#     side have an outcome value of +1 and the data on the other side has an
#     outcome value of -1.
# If the data points are “almost” separable (not all the points are on one 
#     side or the other), a penalizing term is added to the optimization in 
#     order to account for errors, and “soft” margins are produced.
####
# SVMs use kernel functions to transform the data into higher dimensions, in 
#     the hope that they will become more linearly separable. 
# The mathematics of SVMs is complex and well beyond the scope of this book.
####
require("e1071")
set.seed(1234)
fit.svm <- svm(class ~ ., data = df.train)
# Because predictor variables with larger variances typically have a greater
#     influence on the development of SVMs, the svm() function scales each 
#     variable to a mean of 0 and standard deviation of 1 before fitting the
#     model by default. 
fit.svm
# 
svm.pred <- predict(fit.svm, na.omit(df.test))
# Unlike the random forest approach, the SVM is also unable to accommodate 
#     missing predictor values when classifying new cases. 
svm.pref <- table(na.omit(df.test)$class, svm.pred, 
                  dnn = c("Actual", "Predicted"))
#
accuracy <- (116 + 77) / (116 + 77 + 3 + 4) # [1] 0.965
####
# By default, the svm() function uses a radial basis function (RBF) to map
#     samples into a higher-dimensional space (the kernel trick). The RBF
#     kernel is often a good choice because it’s a nonlinear mapping that 
#     can handle relations between class labels and predictors that are
#     nonlinear. 
# When fitting an SVM with the RBF kernel, two parameters can affect the 
#     results: gamma and cost. 
# Gamma is a kernel parameter that controls the shape of the separating 
#     hyperplane. Larger values of gamma typically result in a larger number
#     of support vectors. Gamma can also be thought of as a parameter that 
#     controls how widely a training sample “reaches,” with larger values 
#     meaning far and smaller values meaning close. Gamma must be greater
#     than zero.
# The cost parameter represents the cost of making errors. A large value 
#     severely penalizes errors and leads to a more complex classification
#     boundary. There will be less misclassifications in the training sample,
#     but over-fitting may result in poor predictive ability in new samples.
#     Smaller values lead to a flatter classification boundary but may result
#     in under-fitting. Like gamma, cost is always positive
####
# But a different combination of gamma and cost may lead to a more effective
#     model. 
set.seed(1234)
tuned <- tune.svm(class ~ ., data = df.train,
                  gamma = 10 ^ (-6:1),
                  cost = 10 ^ (-10:10))
# Eight values of gamma (ranging from 0.000001 to 10) and 21 values of 
#     cost (ranging from .01 to 10000000000) are specified. In all, 168 
#     models (8 × 21) are fit and compared. The model with the fewest 
#     10-fold cross validated errors in the training sample has
#     gamma = 0.01 and cost = 1
tuned
#
fit.svm <- svm(class ~ ., data = df.train,
               gamma = 0.01,
               cost = 1)
#
svm.pred <- predict(fit.svm, na.omit(df.test))
svm.perf <- table(na.omit(df.test)$class, svm.pred, 
                  dnn = c("Actual", "Predicted"))
#
accuracy <- (117 + 77) / (117 + 77 + 3 + 3) # [1] 0.97
####
# One drawback of SVMs is that, like random forests, the resulting 
#     classification rules are difficult to understand and communicate. 
#     They’re essentially a black box. 
# Additionally, SVMs don’t scale as well as random forests when building 
#     models from large training samples. But once a successful model is 
#     built, classifying new observations does scale well.
####
#  The most commonly reported statistic is the accuracy, or how often the 
#     classifier is correct. Although informative, the accuracy is 
#     insufficient by itself
# Sensitivity:  Probability of getting a positive classification when the 
#     true outcome is positive (also called true positive rate or recall)
# Specificity: Probability of getting a negative classification when the 
#     true outcome is negative (also called true negative rate)
# Positive predictive value: Probability that an observation with a positive
#     classification is correctly identified as positive (also called 
#     precision)
# Negative predictive value: Probability that an observation with a negative 
#     classification is correctly identified as negative
# Accuracy: Proportion of observations correctly identified (also called ACC)
####
performance <- function(table, n = 2) {
  if(!all(dim(table) == c(2, 2)))
    stop("Must be a 2 x 2 table")
  tn = table[1, 1]
  fp = table[1, 2]
  fn = table[2, 1]
  tp = table[2, 2]
  sensitivity = tp / (tp + fn)
  specificity = tn / (tn + fp)
  ppp = tp / (tp + fp)
  npp = tn / (tn + fn)
  hitrate = (tp + tn) / (tn + fp + fn + tp)
  result <- paste("Sensitivity = ", round(sensitivity, n),
                  "\nSpecificity =", round(specificity, n),
                  "\nPostive Predictive Value =", round(ppp, n),
                  "\nNegative Predictive Value =", round(npp, n),
                  "\nAccuracy =", round(hitrate, n),
                  "\n", sep = "")
  cat(result)
}
#
performance(logit.pref)
performance(dtree.perf)
performance(ctree.perf)
performance(forest.perf)
performance(svm.perf)
####
#  Although it’s beyond the scope of this chapter, you can often improve a 
#     classification system by trading specificity for sensitivity and vice 
#     versa. 
# In the logistic regression model, predict() was used to estimate the 
#     probability that a case belonged in the malignant group. If the 
#     probability was greater than 0.5, the case was assigned to that group. 
# The 0.5 value is called the threshold or cutoff value. If you vary this 
#     threshold, you can increase the sensitivity of the classification model
#     at the expense of its specificity. predict() can generate probabilities
#     for decision trees, random forests, and SVMs as well (although the 
#     syntax varies by method). 
# The impact of varying the threshold value is typically assessed using a 
#     receiver operating characteristic (ROC) curve. A ROC curve plots 
#     sensitivity versus specificity for a range of threshold values. 
# You can then select a threshold with the best balance of sensitivity and 
#     specificity for a given problem. 
# Many R packages generate ROC curves, including ROCR and pROC. 
# Analytic functions in these packages can help you to select the best 
#     threshold values for a given scenario or to compare the ROC curves 
#     produced by different classification algorithms in order to choose the
#     most useful approach.
####
# Rattle (R Analytic Tool to Learn Easily) offers a graphic user interface 
#     (GUI) for data mining in R.
# Rattle also supports the ability to transform and score data, and it
#     offers a number of data-visualization tools for evaluating models.
require("rattle")
rattle()
#
pima <- read.csv("pima-data 1.csv", header = TRUE)
head(pima)
# Rattle can be particularly useful for comparing the results of various 
#     classification techniques. Because it generates reusable R code in a 
#     log file, it can also be a useful tool for learning the syntax of 
#     many of R’s predictive analytics functions.
####





