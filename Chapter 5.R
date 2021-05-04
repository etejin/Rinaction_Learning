##### Chapter 5 Advanced data management
####
stu <- read.csv("students.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)
#
ceiling(2.333) # [1] 3, smallest integer not less than 2.333
floor(2.333) # [1] 2, largest integer not greater than 2.333
trunc(2.333) # [1] 2, integer formed by truncating values in x toward 0
#
round(2.333, 2) # [1] 2.33, round x to the specified number of decimal palces
signif(2.333, 2) # [1] 2.3, round x to the specified number of significant
#     digits
#
log(10)
log10(10)
log(10, base = 10)
#
exp(10)
####
a <- c(1, 2, 3, 4)
#
median(a)
mad(a) # median absolute deviation
#
quantile(stu$Math, c(0.34, 0.97)) # find the 34th and 97th percentiles of x
#
diff(a)
diff(a, lag = 2)
#
scale(stu$Math) # column center (center = TRUE) or standardize data object 
#     (center = TREU, scale = TRUE)
scale(stu$Math) * 10 + 50 # to standardize each column to an arbitrary mean (10)
#     and standard deviation (50)
####
a <- pretty(c(-5, 5), 15)
b <- dnorm(a)
plot(a, b, type = "l")
#
pnorm(1.96)
#
qnorm(0.57, mean = 500, sd = 100)
#
rnorm(50, 50, 10)
####
set.seed(111)
runif(1000)
####
m <- c(230.7, 146.7, 3.6) # the vectors of means
sigma <- matrix(c(15360.8, 6721.2, -47.1,
                  6721.2, 4700.9, -16.5,
                  -47.1, -16.5, 0.3), 3, 3) # the correlation matrix
set.seed(123)
mydata <- MASS::mvrnorm(500, m, sigma) # multivarate normal distributions
mydata <- as.data.frame(mydata)
names(mydata) <- c("y", "x1", "x2")
#
dim(mydata)
head(mydata)
####
a <- c("Xuanjing Jin", "Kecheng Jin")
#
nchar(a)
#
substr(a[1], nchar(a[1])-2, nchar(a[1])) # extract a string
substr(a, nchar(a)-2, nchar(a)) # extract vectors of strings
#
substr(a[2], nchar(a[2])-2, nchar(a[2])) <- "Lin" # to subtract
a
####
b <- sample(letters, 5, replace = FALSE)
grep("a", b, fixed = TRUE)
grep("h", b, fixed = TRUE) # find the number of place that "h" locates
grep(c("f", "z"), b, fixed = TRUE) # cannot do vectors
#
grep("Lin", a, fixed = TRUE) # find the number of places that "Lin"
#     locates
####
paste("Today is", date())
#
b <- paste(a, "likes", "TV.")
####
sub("TV", "Ice Cream", b, fixed = TRUE) 
# [1] "Xuanjing Jin likes Ice Cream." "Kecheng Lin likes Ice Cream." 
#
c <- c("I like apple", "she likes apple, too")
sub("apple", "peach", c)
# [1] "I like peach"         "she likes peach, too"
#
c <- c("I like apple and she likes apple, too")
sub("apple", "peach", c) # it will only substitute the first apple
# [1] "I like peach and she likes apple, too"
# 
gsub("apple", "peach", c) # replace all the items in one strings
# [1] "I like peach and she likes peach, too"
####
?strsplit
b <- strsplit(c, "")
class(b) # [1] "list"
# 
class(b[[1]]) # [1] "character", two ways to transfer the list to
#     character
class(unlist(b)) # [1] "character"
#
noquote(b[[1]]) # delete the quotes
# [1] I   l i k e   a p p l e   a n d   s h e   l i k e s   a p p l e
#     [33] ,   t o o
####
b <- c("J.X.J.")
strsplit(b, ".") # [1] "" "" "" "" "" ""
strsplit(b, "[.]") # [1] "J" "X" "J"
####
b <- getwd()
strsplit(b, "/")
# [1] ""            "Users"       "pioneer"     "Desktop"    
# [5] "R"           "2020 WINTER" "DATA" 
####
strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
strReverse(c("abc", "Statistics"))
# so the only difference bewteen lapply and sapply is, the result from lapply can 
#     still use the lapply or sapply, while the results from sapply cannot
#
strHappy <- function(x) 
  noquote(
    sapply(lapply(strsplit(x, ""), rev), paste, "-HAPPY-"))
strHappy(c("amy", "lenny"))
####
a <- "^[hc]at" # regular expression,
#     search regular expression entry in  wikipedia
b <- c("fat", "cat", "tat", "kat", "hat", "mat", "sat")
unlist(strsplit(b, a))
# [1] "fat" ""    "tat" "kat" ""    "mat" "sat"
#
sub(a, "apple",b)
# [1] "fat"   "apple" "tat"   "kat"   "apple" "mat"   "sat" 
#
grep(a, b) # [1] 2 5
####
a <- c("lalalalal", "lolololo")
b <- toupper(a) # upper case
tolower(b) # lower case
####
a
rep(a, 3) # repeat three times
#
a <- -5:5
pretty(a, 4) # n is the integer giving the desired number of intervals
#     Non-integer values are rounded down
# this function creates pretty breakpoints. divide a continuous variable
#     x into n intervals by selecting n+1 equally spaced rounded values, 
#     often used in plotting
####
t <- titanic_imputed
#
for (i in 1:length(levels(t$class))){
  cat("The average mean of fare with regard to", levels(t$class)[i],
      "class in Titanic is", 
      signif(lapply(split(t$fare, t$class), func)[[i]], 3), "\b.\n")
 }
#
func3 <- function(i) cat("The average mean of fare with regard to", levels(t$class)[i],
      "class in Titanic is", 
      signif(lapply(split(t$fare, t$class), func)[[i]], 3), "\b.\n")
func3(3)
#
paste("The average mean of fare with regard to class in Titanic", 
      lapply(split(t$fare, t$class), func))
#
cat("happy", "\t", "nan", "\b.\n", "lenny") # "\t" represents tab
####
#
func3 <- function(x, y) paste(x, y ^ 2, sep = "-")
t <- esoph
#
lapply(stu, summary)
#
sapply(stu, class)
#
apply(stu[-1], 1, sum)
#
stu <- cbind(stu[1], apply(stu[2:4], 2, as.numeric))
#
tapply(t$ncases, t$agegp, sum)
#
mapply(func3, stu$Student, stu$English)
#
Map(func3, stu$Student, stu$Science)
Map(func3, stu[1:2]) # cannot
#
do.call(cbind, lapply(stu, class)) # which is equal to sapply(stu, class)
#
do.call(cbind, split(t$ncases, t$agegp))
do.call(rbind, split(t, t$agegp))
#
do.call("+", stu[3:4]) # do.call only needs two columns
#
do.call("paste", stu[1:2], sep = "-") # cannot do this
do.call("paste", c(stu[1:2], sep = "-"))
Map(func3, stu[1], stu[2])
mapply(func3, stu[1], stu[2])
#
by(t, t$agegp, 
   function(t) lm(ncases ~ ncontrols, t))
#
split(t, t$agegp)
#
unstack(t$ncases, t$agegp)
####
# combine test score into a single performance indicator for each students
# grade each student from A to F based on their relative standing
# sort the roster by last name followed by first name
##
sapply(stu, class)
name <- strsplit(stu$Student, "_")
#
z <- scale(stu[2:4])
score <- apply(z, 1, func)
#
breaks <- quantile(score, c(1, 0.8, 0.6, 0.4, 0.2, 0))
f <- cut(score, c(1.5, 0.75, 0.43, -0.36, -0.89, -1.2), c("A", "B", "C", "D", "F"))
#
transform(stu, Firstname = sapply(name, "[", 1),
               Lastname = sapply(name, "[", 2),
               Score = score, 
               Grade = f)
stu <- .Last.value[-1]
stu <- stu[order(stu$Firstname, stu$Lastname),]
####
stu <- cbind(stu[,c(1, 2, 3, 6, 7)], apply(stu[4:5], 2, as.character))
first <- lapply(name, "[", 2)
#
for(i in 1:length(stu$Grade)) {
  cat("The grade of", first[[i]], "is", stu$Grade[[i]], "\b.\n")
}
####
i <- 10
while (i < 0) {
  print("student"); i <- i - 1
}
####
if (is.character(stu$Grade)) stu$Grade <- as.factor(stu$Grade) else print("GRADE is already a factor")
#
ifelse(stu$Grade == "A", "Excellent", "So-so")
#
fruit <- c("Apple", "Banana")
for (i in fruit) {
  print(
    switch(i, 
           Apple = "1",
           Peach = "2",
           Banana = "3")
  )
}
####
mystats <- function(x, parametric = TRUE, print = FALSE) {
  if (parametric) {
    center <- func(x); spread <- sd(x)
  } else {
    center <- median(x); spread <- mad(x)
  }
  if (print & parametric) {
    cat("Mean = ", center, "\t", "SD = ", spread, "\n", "\n")
  } else if (print & !parametric) {
    cat("Median = ", center, "\t", "MAD = ", spread, "\n", "\n")
  }
  result <- list(center = round(center, 3), 
                 spread = round(spread, 3))
  return(result)
}
#
a <- rnorm(200, mean = 20, sd = 3)
mystats(a)
mystats(a, parametric = FALSE, print = TRUE)
####
mystats <- function(x, Math = TRUE, Science = TRUE, English = FALSE) {
  if (Math & Science) {
    if (z$Math[x] > z$Science[x]) {
      cat(First[x], 
          "has better Math performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    } else {
      cat(First[x], 
          "has better Science performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    }
  } else if (Math & !Science) {
      cat(First[x], "has no Science Score", "\b!\n")
    } else if (!Math & Science) {
      cat(First[x], "has no Math Score", "\b!\n")
    }
  if (Math & English) {
    if (z$Math[x] > z$English[x]) {
      cat(First[x], 
          "has better Math performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    } else {
      cat(First[x], 
          "has better English performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    }
  } else if (Math & !English) {
    cat(First[x], "has no English Score", "\b!\n")
  } else if (!Math & English) {
      cat(First[x], "has no Math Score", "\b!\n")
    }
  if (Science & English) {
    if (z$Science[x] > z$English[x]) {
      cat(First[x], 
          "has better Science performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    } else {
      cat(First[x], 
          "has better English performance, while the final rank in class is", 
          Rank[x], "\b.\n")
    }
  } else if (Science & !English) {
    cat(First[x], "has no English Score", "\b!\n")
  } else if (!Science & English) {
    cat(First[x], "has no Science Score", "\b!\n")
}}
mystats(2, English = FALSE)
mystats(3, Science = TRUE, English = TRUE, Math = FALSE)
mystats(4, Math = TRUE, Science = FALSE, English = TRUE)
####
?strftime
mydate <- function(type = "default") {
  switch(type, 
         default = format(Sys.time(), "%F"),
         long = format(Sys.time(), "%A %B %D %Y"),
         short = format(Sys.time(), "%a %b %d %y"),
         shorter = format(Sys.time(), "%x"),
         formal = format(Sys.time(), "%X"),
         cat(type, "is not a recognized type", "\b.\n")
         )
}
#
mydate()
mydate("short")
mydate("shoter")
mydate("shorter")
mydate("long")
mydate("formal")
####
car <- mtcars[1:5, 1:8]
t(car) # transpose
####
t <- esoph
aggregate(t[4:5], list(AgeGroup = t$agegp), median)
aggregate(t[5], list(AgeGroup = t$agegp,
                       Case = t$ncases >= 3), 
          median)
#
aggregate(state.x77, list(Region = state.region), func)
aggregate(state.x77,
          list(Region = state.region,
               Cold = state.x77[,"Frost"] > 130),
          func)
#
aggregate(. ~ Species, data = iris, func) # the later one is factor
#
aggregate(len ~ ., data = ToothGrowth, func)
#
ag <- aggregate(len ~ ., data = ToothGrowth, func)
xtabs(len ~ ., data = ag)
####
require("reshape2")
mydata <- read.csv("reshape.txt", header = TRUE, sep = "")
#
md <- melt(mydata, id = c("ID", "Time")) # when we melt a data, we restructure it into
#     the format in which each measured variable in its own row along with the
#     ID variables needed to uniquely identify it.
####
dcast(md, Time ~ variable, func) # here md must be multed data
dcast(md, ID + Time  ~ variable, func)
dcast(md, ID + variable  ~ Time, func)
####

