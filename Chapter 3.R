###### Chapter 3 getting started with graphs
source("chandir")
col <- rainbow(20, rev = TRUE, alpha = 0.4)
####
t <- LifeCycleSavings
head(t, 2)
#
plot(dist ~ speed, type = "b", cars)
####
par() # will come out a list of current grapical settings
par(no.readonly = TRUE) # to list changeable current graphical settings
#
opar <- par(no.readonly = TRUE) # save the oringinal settings
par(pch = 17, lty = 3, lwd = 2) # create new one temporarily
plot(dist ~ speed, type = "b", cars)
par(opar) # to restore the original settings
####
data("Bfox", package = "carData")
data("titanic_imputed", package = "DALEX")
##
opar <- par(no.readonly = TRUE)
par(pch = 17, lty = 3, col = "#00B3FF66")
#
plot(sr ~ pop15, t)
boxplot(fare ~ gender, titanic_imputed)
coplot(fare ~ age | gender, titanic_imputed)
plot(fare ~ age, pch = as.integer(gender), titanic_imputed)
par(opar)
#
opar$lty # check whether it restores
opar$adj
# this way suit those change the parameter together
####
plot(cars, type = "b", pch = 17, cex = 1.5, 
     lty = 2, lwd = 2, 
     col = col[19]) # pch specifies the symbols using when plotting
#     points; cex specifies the scale of the points
####
d <- density(Bfox$womwage)
#
x <- unlist(density(Bfox$womwage)[1])
y <- unlist(density(Bfox$womwage)[2])
region.x <- x[x >= 18 & x <= 25]
region.y <- y[x >= 18 & x <= 25]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
#
h <- hist(Bfox$womwage, 10, prob = TRUE)
#
col2 <- ifelse(h$density >= 0.05, col[1], col[5])
#
rel.h <- rank(h$density) / length(h$density)
col3 <- grey((1-rel.h), alpha = 0.7)
#
hist(Bfox$womwage, 10, prob = TRUE, col = col[1:10]) # when color histogram
#     must use hist its own data
lines(d, lty = 2, lwd = 5, col = col[15])
polygon(region.x, region.y, density = 10, col = col[20], lwd = 5)
####
t <- esoph
bar <- tapply(t$ncontrols, t$agegp, func)
#
rel.h <- rank(bar) / length(bar)
col2 <- grey((1-rel.h), 0.7)
#
barplot(bar, col.axis = "red", col.lab = "blue", 
        col.main = "orange", col.sub = "purple", 
        fg = "brown", bg = "green",
        main = "Barplot of controls base on Age Groups",
        sub = "2021-01-01",
        xlab = "Age",
        ylab = "Controls",
        col = col2) # change the color
abline(h = 0)
####
require("RColorBrewer")
display.brewer.all() # Set 3, Purples, YIOrBr, YIGn, Reds, PuBu,
#     BuPu, Pastel1, Pastel2, PRGn
col3 <- brewer.pal(6, "PRGn")
col4 <- brewer.pal(6, "BuPu")
#
barplot(bar, col = col3)
####
barplot(bar, main = "Barplot of controls base \n on Age Groups",
        sub = "2021-01-01", xlab = "Age", ylab = "Controls",
        cex.axis = 0.7, cex.lab = 1, cex.main = 1.5, cex.sub = 1.2,
        col = col4, col.sub = col3[2])  # change the cex
####
col3 <- brewer.pal(length(bar), "Pastel1")
barplot(bar, main = "Barplot of controls base \n on Age Groups",
        sub = "2021-01-01", xlab = "Age", ylab = "Controls",
        font.axis = 3, font.lab = 2, font.main = 4, font.sub = 1,
        family = "mono",
        col = col3) # change the fonts
####
require("Hmisc")
a <- 1:10
b <- 2 * a
c <- 10 / a
par(mar = c(3, 5, 4, 3) + 0.1)
plot(a, b, type = "b", pch = 17, col = col3[4], lty = 2, cex = 2,
     ann = FALSE)
lines(a, c, type = "b", pch = 20, col = col3[1], lty = 2, cex = 2)
axis(4, at = c, labels = round(c, 2), 
     col.axis = "red", tck = -0.01) # side = 1, 2, 3, 4 
#     represent bottom, left, top and right.
mtext("y = 1/x", side = 3, line = 3, las = 0, 
      cex.lab = 1, col = "blue") # las specify whether parallel or 
#     perpendicular
minor.tick(nx = 2, ny = 2, tick.ratio = 0.5,)
legend(2, 20, c("Y", "Z"), pch = c(17, 20), col = c(col3[4], col3[1]))
##
par(opar)
opar$mar
####
t <- Bfox
require("zoo")
date <- as.Date("1946-01-01")
date <- seq(date, len = 30, by = "1 year")
t <- as.zoo(Bfox, date)
plot(t$menwage, t$womwage, pch = 17)
text(t$menwage, t$womwage,
     index(t), cex = 0.4, pos = 4)
####
quartzFonts() # serif, sans(default), mono, 
#
plot(1:5, 1:5, type = "b")
text(2, 2, "happy", family = "mono")
text(3, 3, "sad", family = "sans")
text(4, 4, "so-so", family = "serif")
####
?plotmath
par(mar = c(4, 4, 4, 2), mai = c(0.5, 0.5, 0.5, 0.5), pin = c(3, 3))
plot(1:5, 1:5, type = "b", col = col3[2], pch = 17)
text(2, 2, expression(x %+-% y), pos = 4)
text(3, 3, expression(x %~~% y), pos = 4)
text(4, 4, expression(x %prop% y), pos = 4)
text(5, 5, expression(sqrt(x, y)), pos = 2)
text(1, 1, expression(underline(x %=~% y)), pos = 4)
minor.tick(2, 2, 0.5)
opar$mai
opar$pin
par(opar)
####
source("christmas.R")
#
par(mfrow = c(2, 2))
#
plot(phone.daily, lty = c(1, 2, 3), lwd = 3, 
     col = c(col[2], col3[6], col3[10]), screen = 1, 
     col.lab = "blue")
minor.tick(3, 3, 0.25) # 1
legend(index(phone.daily)[3], 140, names(phone.daily), 
       lty = c(1, 2, 3), col = c(col[2], col3[6], col3[10]),
       text.width = 0.7, x.intersp = 0.2, y.intersp = 0.5, 
       seg.len = 0.6, xjust = 0.4, yjust = 1, xpd = TRUE,
       inset = 0.2, merge = TRUE, trace = TRUE) # 2
#
plot(ipad.daily, lty  = 2, lwd = 3, col = col3[6], screen = 1, 
     col.axis = "red")
title("HAPPY", family = "mono", font.main = 3) # 3
mtext("LALLA", 3) # 4
#
plot(total.daily, lty = 3, lwd = 3, col = col3[1], screen = 1)
minor.tick(2, 2, 0.5)
#
par(opar)
opar$mfrow
####
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE),
       widths = c(2, 3), heights = c(3, 3))
plot(phone.daily, screen = 1)
plot(ipad.daily, screen = 1)
plot(total.daily, screen = 1)
#
par(opar)
####
par(fig = c(0, 0.8, 0, 0.8))
plot(cars)
par(fig = c(0, 0.8, 0.4, 1), new = TRUE)
boxplot(cars$speed, horizontal = TRUE, axes = FALSE)
par(fig = c(0.5, 1, 0, 0.8), new = TRUE)
boxplot(cars$dist, axes = FALSE)
#
mtext("Enhanced Scatter Plot", side = 3, family = "mono",,
      line = -4, outer = TRUE)
par(opar)
####
### par(mar = c(bottom, left, top, right)) # margin 
### par(fig = c(x1, x2, y1, y2))
### par(mfrow = c(x, y))
### layout(martix(c(a, b, c, d)), widths = d, heights = e)
## lines
## axis
## mtext
## text
## legend
## title
## abline # reference line
## minor.tick
## plot.math
# pch
# font
# cex
# col
# lty
# lwd







