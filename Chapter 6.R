###### Chapter 6 Basic graphs
require("vcd")
####
head(state.x77, 2) 
head(state.region)
#
t <- data.frame(state.region, state.x77)
head(t, 3)
#
mean <- aggregate(t$Murder, by = list(StateRegion = t$state.region), func)
mean <- mean[order(mean$x),]
#
col <- brewer.pal(6, "Purples")
#
barplot(mean$x, col = col[1:4],
        names.arg = mean$StateRegion)
abline(h = 0)
####
require("gplots")
ci.lower <- tapply(t$Murder, t$state.region, 
                   function(x) t.test(x)$conf.int[1])
ci.upper <- tapply(t$Murder, t$state.region, 
                   function(x) t.test(x)$conf.int[2])
#
col <- brewer.pal(6, "Set3")
#
barplot2(mean$x, col = col[1:4], plot.ci = TRUE,
         ci.l = sort(ci.lower), ci.u = sort(ci.upper),
         names.arg = mean$StateRegion)
####
heights <- tapply(t$Population, t$state.region, func)
ci.lower <- tapply(t$Population, t$state.region, 
                   function(x) t.test(x)$conf.int[1])
ci.upper <- tapply(t$Population, t$state.region,
                   function(x) t.test(x)$conf.int[2])
#
barplot2(sort(heights), plot.ci = TRUE, 
         ci.l = sort(ci.lower), ci.u = sort(ci.upper),
         xpd = TRUE, col = col[3:6])
####
rel.h <- rank(heights) / length(heights)
col2 <- grey((1-rel.h), alpha = 0.7)
barplot(heights, col = col2)
# 
barplot(heights, width = rel.h, col = col2) # width, cool~~
####
t <- Arthritis
plot(t$Improved) # use plot to  plot the categorial variables, it will 
#     automactically generate the bar plot
#
plot(t$Improved, horiz = TRUE) # generate it horizontally
#
barplot(table(t$Improved)) # use barplot generates, we need use table to count,
#     because barplot() functions nees a group of vectors
#
barplot(table(t$Improved), horiz = TRUE)
####
require("RColorBrewer")
col <- brewer.pal(9, "BuPu")
#
counts <- table(t$Improved, t$Treatment) 
barplot(counts, beside = FALSE, col = c(col[2], col[4], col[6]),
        main = "Stacked Barplot",
        legend = rownames(counts)) # when the 'heights' become a matrix, rather 
#     than  vectors, barplot will generate stacked barplot or grouped bar plot;
#     beside is FALSE for default; legend is only useful when height is matrix
#
barplot(counts, beside = TRUE, col = c(col[1], col[3], col[5]),
        main = "Grouped Barplot",
        legend = rownames(counts))
####
col <- brewer.pal(6, "PRGn")
#
barplot(counts, col = col[3:5],
        names.arg = c("This is \n Placebo", 
                      "This is \n Treated")) # we can make the 
#     long name into two lines
#
barplot(counts, col = col[1:3],
        cex.names = 0.8,
        names.arg = c("This is Placebo", 
                      "This is Treated")) # we can make the font size small
#
barplot(counts, col = col[2:4],
        names.arg = c("This is \nPlacebo", 
                      "This is \nTreated"),
        horiz = TRUE) 
####
vcd::spine(counts)
#
t <- esoph
sapply(t, class)
#
vcd::spine(agegp ~ alcgp, data = t, breaks = 3) # only for
#     two factors
#### 
# Barplot cannot only plot the counts of one or two categorical
#     variables, but also can plot the percentage of one categorical
#     variable in another categorical variable; besides, Barplot
#     also can plot more meanings of continuous variables, like
#     mean, median, confidence interval, they also can add different
#     color systems
####
#
col <- brewer.pal(4, "Pastel1")
pie(mean$x, labels = mean$StateRegion)
#
per <- round(mean$x / sum(mean$x), 3)
lbls <- paste(mean$StateRegion, " ", per, "%", sep = "")
#
pie(per, labels = lbls, col = col)
####
require("plotrix")
#
plotrix::pie3D(per, labels = lbls, col = col, 
      explode = 0.1)
####
brewer.pal.info
col <- brewer.pal(4, "YlOrBr")
#
counts <- table(state.region)
lbls <- paste(rownames(counts), "\n", counts, sep = "")
plotrix::pie3D(table(state.region), labels = lbls,
      explode = 0.15,
      col = col)
####
col <- brewer.pal(4, "Oranges")
#
plotrix::fan.plot(per, labels = lbls, col = col) # make it easier
#     to compare the values of slices
####
col <- brewer.pal(4, "PiYG")
#
plotrix::fan.plot(counts, labels = lbls, col = col)
####
col2 <- rainbow(15, rev = TRUE, alpha = 0.4)
#
t <- data.frame(state.region, state.x77)
hist(t$Murder, 15, col = col2[1:15])
#
hist(t$Murder, 15, col = col2[1:15], prob = TRUE)
lines(density(t$Murder), lwd = 3, lty = 3, col = "red")
####
hist(t$Life.Exp, 15, col = col2, prob = TRUE)
rug(jitter(t$Life.Exp)) # with rug plot, which is one 
#     dimensional representation of the actual data values
lines(density(t$Life.Exp), lty = 2, lwd = 3, col = col[3])
####
col <- brewer.pal(10, "PRGn")
#
h <- hist(t$Murder, 10, col = col)
xfit <- seq(min(t$Murder), max(t$Murder), len = 15)
yfit <- dnorm(xfit, mean = func(t$Murder), 
              sd = sd(t$Murder))
yfit <- yfit * diff(h$mids[1:2]) * length(t$Murder)
lines(xfit, yfit, col = "red", lwd = 3) # superimposed 
#     normal curve
box() # add box
Hmisc::minor.tick(nx = 2, ny = 2, tick.ratio = 0.5)
####
h <- hist(t$Murder, 15, prob = T)
rel.h <- rank(h$density) / length(h$density)
col <- grey((1-rel.h), alpha = 0.7)
hist(t$Murder, 15, prob = TRUE, col = col)
####
col <- rainbow(10, rev = TRUE, alpha = 0.4)
#
d <- density(t$Murder)
#
x <- d$x
y <- d$y
q <- quantile(x, c(0.25, 0.75))
#
region.x <- x[x >= q[1] & x <= q[2]]
region.y <- y[x >= q[1] & x <= q[2]]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
#
hist(t$Murder, 15, prob = TRUE, col = col)
lines(density(t$Murder), lwd = 3, lty = 2, col = "red")
polygon(region.x, region.y, density = 10, col = "blue")
####
opar <- par(no.readonly = TRUE)
#
par(fig = c(0, 0.8, 0, 0.8))
plot(t$Murder, t$Population, pch = 18)
par(fig = c(0, 0.8, 0.5, 1), new = TRUE)
boxplot(t$Murder, horizontal = TRUE, ann = FALSE, 
        axes = FALSE)
par(fig = c(0.6, 1, 0, 0.8), new = TRUE)
boxplot(t$Population, axes = FALSE)
mtext("Enhanced Scatter Plot", side = 3, family = "mono",
      line = -2, outer = TRUE)
par(opar)
####
col <- brewer.pal(4, "PRGn")
#
x <- d$x
y <- d$y
q <- quantile(x, c(0.75, 0.95))
region.x <- x[x >= q[1] & x <= q[2]]
region.y <- y[x >= q[1] & x <= q[2]]
region.x <- c(region.x[1], region.x, tail(region.x, 1))
region.y <- c(0, region.y, 0)
#
plot(d)
polygon(region.x, region.y, density = 10, col = col[2])
####
update.packages(checkBuilt=TRUE, ask=FALSE)
#
capabilities()
#
install.packages("sm", dependencies = TRUE)
require("sm")
#
sm.density.compare(iris$Sepal.Length, iris$Species,
                   col = col[1:3])
legend(locator(1), levels(iris$Species),
       fill = 1:length(levels(iris$Species)))
####
b <- boxplot(stu$Math) 
boxplot.stats(b$stats)
#
boxplot(iris$Sepal.Length ~ iris$Species) # are quite 
#     symmetric, do not have mild positive skew (the upper
#     whisker is longer than the lower whisker)
boxplot.stats(b$stats, do.out = FALSE, do.conf = FALSE)
####
boxplot(iris$Sepal.Length ~ iris$Species, 
        varwidth = TRUE, notch = TRUE,
        col = col[3]) # notched box plots, while varwidth = 
#     TRUE produces box plots with widths that are proportional 
#     to their sample sizes
#  if two boxplots notches do not overlap, there's strong 
#     evidence that their median differ
####
t <- esoph
sapply(t, class)
boxplot(t$ncontrols ~ t$agegp * t$tobgp, varwidth = TRUE,
        notch = TRUE, col = col[3:4]) # the col set for two, will recycle,
#     comparing for two factors based on one numerical variables.
#
boxplot(t$ncontrols ~ t$agegp * t$alcgp, varwidth = TRUE,
        col = col[3:4], cex.axis = 0.5) 
####
require("vioplot")
vioplot(t$ncontrols[t$agegp == "25-34"],
                 t$ncontrols[t$agegp == "35-44"],
                 t$ncontrols[t$agegp == "45-54"],
                 names = levels(t$agegp)[1:3],
                 col = col[1:3]) # good for examing variation, combination of 
#     boxplot and kernel density plot
####
violin_plot(t[4:5]) # violin plot only for numeric variables
#
count <- aggregate(t[4:5], list(Age = t$agegp), func)
violin_plot(count[2:3]) # the white dot is the median, while the thin 
#     black box is the whiskers, the outer shape shows the kernel density 
#     plot
####
col <- brewer.pal(5, "Reds")
dotchart(stu$Math, labels = stu$First, col = col[3], pch = 17)
####
head(stu, 3)
#
breaks <- quantile(stu$Score, c(1, 0.8, 0.6, 0.4, 0.2, 0))
f <- cut(stu$Score, breaks, labels = c("A", "B", "C", "D", "F"), 
         include.lowest = TRUE)
transform(stu, Grade = f)
stu2 <- .Last.value
#
col <- brewer.pal(5, "Purples")
#
for (i in 1:length(levels(stu$Grade))) {
  stu2$Color[stu2$Grade == levels(stu$Grade)[i]] <- col[i]
}

dotchart(stu$Math, labels = stu$First, 
         groups = stu$Grade, 
         gpch = 16:18,
         gcolor = "red",
         col = stu2$Color)
####





