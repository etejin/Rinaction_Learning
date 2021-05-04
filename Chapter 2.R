###### Chapter 2 creating a dataset
####
dim1 <- paste("A", 1:2, sep = "-")
dim2 <- paste("B", 1:7, sep = "-")
dim3 <- paste("C", 1:10, sep = "-")
b <- array(1:140, dim = c(2, 7, 10), 
           dimname = list(dim1, dim2, dim3)) # dimname must be a lit
####
data(titanic_imputed, package = "DALEX")
#
with(titanic_imputed, {
  boxplot(age ~ class, col = col[1:6])
  coplot(fare ~ sibsp | gender,
         col = col[8])
})
####
attach(titanic_imputed)
#
x <- unlist(density(age)[1])
y <- unlist(density(age)[2])
#
region.x <- x[x >= 40 & x <= 60]
region.y <- y[x >= 40 & x <= 60]
region.x <- c(x[1], region.x, tail(x, 1))
region.y <- c(0, region.y, 0)
#
hist(age, 20, prob = T, col = col[1:20])
lines(density(age), lwd = 3, lty = 3, col = col3[20])
polygon(region.x, region.y, density = -1, col = col[3])
#
detach(titanic_imputed)
####
a <- sample(c(1, 2), 200, replace = TRUE)
b <- factor(a, levels = c(1, 2), labels = c("Female", "Male"))
c <- factor(a)
####

