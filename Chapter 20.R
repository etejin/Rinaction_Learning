###### Chapter 20 Advanced programming
####
# R is an object-oriented, functional, array programming language in which
#     objects are specialized data structures, stored in RAM, and accessed via
#     names or symbols
# All objects are stored in RAM during program execution, which has significant
#     implications for the analysis of massive datasets
# Every object has attributes: meta-information describing the characteristics 
#     of the object. 
#  A key attribute is an object’s class. R functions use information about
#     an object’s class in order to determine how the object should be handled.
####
# There are two fundamental data types: atomic vectors and generic vectors. 
# Atomic vectors are arrays that contain a single data type (logical, real, 
#     complex, character, or raw). 
# Generic vectors, also called lists, are collections of atomic vectors. 
####
# scalar
# A matrix is an atomic vector that has a dimension attribute, dim,
#     containing two elements (number of rows and number of columns). 
a <- c(1, 2, 3, 4, 5, 6, 7, 8)
class(a)
#
attr(a, "dim") <- c(2, 4)
class(a)
# 
attr(a, "dimnames") <- list(c("A1", "A2"),
                            c("B1", "B2", "B3", "B4"))
#
attr(a, "dim") <- NULL
class(a)
####
# An array is an atomic vector with a dim attribute that has three or more 
#     elements
# The attr() function allows you to create arbitrary attributes and associate 
#     them with an object. Attributes store additional information about an
#     object and can be used by functions to determine how they’re processed.
####
# Lists are collections of atomic vectors and/or other lists. Data frames are
#     a special type of list, where each atomic vector in the collection has
#     the same length.
t <- iris
unclass(t)
# the unclass() function can be used to examine the object’s contents directly.
attributes(t)
####
set.seed(1234)
fit <- kmeans(t[1:4], 3)
# 
names(fit)
length(fit)
class(fit)
str(fit)
unclass(fit)
sapply(fit, class)
####
# Atomic vectors: Elements are extracted using object[index], where object is 
#     the vector and index is an integer vector. If the elements of the 
#     atomic vector have been named, index can also be a character vector 
#     with these names.
# Generic: For lists, components (atomic vectors or other lists) can be 
#     extracted using object[index], where index is an integer vector, which
#     will return as a list
# To get just the elements in the component, use object[[integer]]
# To extract a single named component, you can use the $ notation. In this 
#     case, object[[integer]] and object$name are equivalent
# Notations can be combined to obtain the elements within components.
fit[[2]]
fit[[2]][1,] # extracts the second component of fit (a matrix of means) and 
#     returns the first row (the means for the first cluster on each of the
#     four variables)
####
means <- fit$centers
require("reshape")
dfm <- melt(means)
names(dfm) <- c("Cluster", "Measurement", "Centimeters")
dfm$Cluster <- factor(dfm$Cluster)
head(dfm, 2)
# 
ggplot(dfm, aes(x = Measurement, y = Centimeters, group = Cluster)) + 
  geom_point(size = 3, 
             aes(shape = Cluster, color = Cluster)) +
  geom_line(size = 1, 
            aes(color = Cluster)) +
  ggtitle("profiles for iris clusters")
####
for(i in 1:5) print(1:i)
#
x <- 1:5
y <- 1:5
if(interactive()) {
  plot(x, y)
} else {
  png("1.png")
  plot(x, y)
  dev.off
} 
# If the code is being run interactively, the interactive() function returns
#     TRUE and a plot is sent to the screen. Otherwise, the plot is saved to
#     disk
####
# The ifelse() function is a vectorized version of if(). Vectorization allows
#     a function to process objects without explicit looping. 
pvalues <- runif(20, min = 0, max = 0.1)
results <- ifelse(pvalues > 0.05, "Non Significant", "Significant")
#
for(i in 1:length(pvalues)) {
  if (pvalues[i] < 0.05) results[i] <- "Significant"
  else results[i] <- "Insignificant"
}
####
#  Parameters can be passed by keyword, by position, or both. Additionally, 
#     parameters can have default values.
# This also demonstrates that parameters passed by keyword can
#     appear in any order.
# The return() function returns the object produced by the function.
#     It’s also optional, and if it’s missing, the results of the last 
#     statement in the function is returned
f <- function(x, y, z = 3) {
  results <- x + y^2 + z^3
  return(results)
  }
f(2, 3, 4)
f(2, 3)
# 
args(f) # to view the parameter names and default values
formals(f) # obtain the parameter names and default values programmatically
####
# Objects created outside of any function are global (can be resolved within
#     any function). Objects created within a function are local (available
#     only within the function). 
# Local objects are discarded at the end of function execution. Only objects
#   passed back via the return() function (or assigned using an operator 
#     like <<-) are accessible after the function finishes executing
# Global objects can be accessed (read) from within a function but not 
#     altered (again, unless the <<- operator is used)
# Objects passed to a function through parameters aren’t altered by the
#     function. Copies of the objects are passed, not the objects themselves.
####
# An environment in R consists of a frame and enclosure.
# A frame is set of symbol-value pairs (object names and their contents), 
#     and an enclosure is a pointer to an enclosing environment. 
# The enclosing environment is also called the parent environment. 
x <- 5
myenv <- new.env()
assign("x", "Homer", env = myenv)
ls(myenv)
get("x", env = myenv)
#
myenv$x
myenv$y <- 1:20
#
parent.env(myenv)
####
# Because functions are objects, they also have environments. This is 
#     important when considering function closures (functions that are 
#     packaged with the state that existed when they were created). 
# In general, the value of an object is obtained from its local environment.
#     If the object isn’t found in its local environment, R searches in the 
#     parent environment, then the parent’s parent environment, and so on, 
#     until the object is found. 
# If R reaches the empty environment and still hasn’t found the object, it 
#     throws an error. This is called lexical scoping
####
# R is an object-oriented programming (OOP) language that’s based on the use
#     of generic functions. 
#  R has two separate OOP models. 
# The S3 model is older, simpler, and less structured. 
# The S4 model is newer and more structured. 
# The S3 approach is easier to use, and most applications in R use this model.
####
# R uses the class of an object to determine what action to take when a
#     generic function is called.
# The primarily limitation of the S3 object model is the fact that any class
#     can be assigned to any object. There are no integrity checks.
# In the S4 approach, classes aredefined as abstract objects that have slots
#     containing specific types of information (that is, typed variables). 
#   Object and method construction are formally defined, with rules that are 
#   enforced.
####
# “A power user is someone who spends an hour tweaking their code so that it
#     runs a second faster.”
#  Several coding techniques can help to make your programs more efficient:
# ■ Read in only the data you need.
# ■ Use vectorization rather than loops whenever possible.
# ■ Create objects of the correct size, rather than resizing repeatedly.
# ■ Use parallelization for repetitive, independent tasks.
####
d <- read.csv("reshape.txt", header = TRUE, sep = "",
                colClasses = c("numeric", NULL, NULL, NULL))
# Variables associated with a NULL colClasses value are skipped. 
#     As the number of rows and columns in the text file increases, the 
#     speed gain becomes more significant
####
# Here, vectorization means using R functions that are designed to process 
#     vectors in a highly optimized manner. Examples in the base 
#     installation include ifelse(), colSums(), colMeans(), rowSums(), and 
#     rowMeans(). 
####
# It’s more efficient to initialize objects to their required final size and 
#     fill in the values than it is to start with a smaller object and grow
#     it by appending values. 
####
# Parallelization involves chunking up a task, running the chunks 
#     simultaneously on two or more cores, and combining the results.
####
# “Why is my code taking so long?” R provides tools for profiling programs in
#     order to identify the most time-consuming functions.
# Place the code to be profiled between Rprof() and Rprof(NULL). 
# Then execute summaryRprof() to get a summary of the time spent executing 
#     each function.
####















