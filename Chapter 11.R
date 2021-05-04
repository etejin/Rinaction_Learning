###### Chapter 11 Intermediate graphs
####
t <- mtcars
t$cyl <- factor(t$cyl)
#
col <- RColorBrewer::brewer.pal(3, "Pastel1")
col2 <- rainbow(15, rev = TRUE, alpha = 0.7)
# 
plot(t$mpg ~ t$wt, pch = as.integer(t$cyl), col = col2[1:3])
abline(lm(t$mpg ~ t$wt)) # add a linear line of the best fit
lines(lowess(t$wt, t$mpg), col = "green", lty = 2, lwd = 3) # is used to
#     add a smoothed line. This smoothed line is a nonparametric fit line based 
#     on locally weighted polynomial regression. 
####
require("car")
car::scatterplot(mpg ~ wt | cyl, data = t, 
                 lwd = 2, 
                 legend = list(inset = 0.02, cex = 0.8),
                 id = list(n = 1),
                 ellipse = list(levels = c(0.5, 0.95), 
                                robust = TRUE,
                                fill = TRUE,
                                alpha = 0.3),
                 smooth = list(var = TRUE, span = 0.75, 
                               lwd = 3, lwd.var = 2),
                 boxplot = "xy")
# The span parameter controls the amount of smoothing in the loess line. 
#     Larger values lead to smoother fits.
####
scatterplot(infantMortality ~ ppgdp, log = "xy", data = UN, 
            id = list(n = 5))
#
scatterplot(prestige ~ income | type, data=Prestige,
            smooth=list(var=FALSE, span=0.75, lwd=4, lwd.var=2))
####
pairs(~ mpg + disp + drat + wt, mtcars) # scatter plot matrices
#
pairs( ~ mpg + disp + drat + wt, mtcars, upper.panel = NULL)
####
car::scatterplotMatrix(~ mpg + disp + drat + wt, data = mtcars,
                       smooth = list(lty = 2, spread = TRUE, 
                                     col.var = col2[2]))
# spread = FALSE suppress lines showing spread and asymmetry
####
TeachingDemos::pairs2(t[,1:3], t[, 2:4], panel = panel.smooth)
####
HH::xysplom(t[, 2:5])
####
ResourceSelection::kdepairs(t[, 1:3], n = 25, 
                            density = TRUE, contour = TRUE) 
# Scatterplot matrix with 2D kernel density
####
require("SMPracticals")
SMPracticals::pairs.mod(mtcars[, 1:4])
SMPracticals::pairs.mod(mathmarks)
####
set.seed(123)
n <- 1000
c1 <- matrix(rnorm(n, mean = 0, sd = 0.5), ncol = 2)
c2 <- matrix(rnorm(n, mean = 3, sd = 2), ncol = 2)
d <- rbind(c1, c2)
d <- as.data.frame(d)
names(d) <- c("x", "y")
plot(d$x, d$y, pch = 19)
#
smoothScatter(d$x, d$y) # a kernel-density estimate to produce
#     smoothed color density representations of the scatter plot. 
# to solve the scatterplot with too much overlapping
# we can discern the density here
####
bin <- hexbin::hexbin(d$x, d$y, xbins = 50) # Basic components are 
#     a cell id and a count of points falling in each occupied cell.
plot(bin,  main = "Hexagonal Binning with 10, 000 Observations")
# Data concentrations are easy to see, and counts can be read from 
#     the legend
####
?smoothScatter
# 
Lab.palette <- colorRampPalette(c("blue", "orange", "red"), space = "Lab")
smoothScatter(d, colramp = Lab.palette,
                     ## pch=NA: do not draw them
                     nrpoints = 250, ret.selection=TRUE)
####
?IDPmisc::ipairs
require("IDPmisc")
#
AQ <- airquality
AQ$Month <- as.factor(AQ$Month)
#
zmax <- ipairs(AQ, pixs=2, main="Air Quality") # Pixel size in mm on the 
#     plotting device.
ipairs(AQ, pixs=2, zmax=zmax, main="Air Quality",
       border = TRUE) # Maximum number of counts per pixel in the plot. 
#     When NULL, each scatter plot has its individual scale. If a 
#     number >= maximum number of counts per pixel is supplied, the
#     scale will be identical for all scatter plots. The maximum number 
#     of counts per pixel is delivered by the return value.
####
t <- mtcars
scatterplot3d::scatterplot3d(t$mpg, t$cyl, t$disp,
                             main = "basic 3D scatter plot")
#
scatterplot3d::scatterplot3d(t$mpg, t$cyl, t$disp,
                             pch = 16,
                             highlight.3d = TRUE,
                             type = "h")
#
s3d <- scatterplot3d::scatterplot3d(t$wt, t$disp, t$mpg,
                                    pch = 16, highlight.3d = TRUE,
                                    type = "h")
fit <- lm(mpg ~ wt + disp, t)
s3d$plane3d((fit)) # add a regression plane
# The plane represents the predicted values, and the points are the 
#     actual values. The vertical distances from the plane to the 
#     points are the residuals. Points that lie above the plane are 
#     under-predicted, whereas points that lie below the line are 
#     over-predicted.
####
require("rgl")
rgl::plot3d(t$wt, t$disp, t$mpg) # interactive 3d scatter plot
####
require("car")
scatter3d(t$wt, t$disp, t$mpg)
####
col2 <- rainbow(20, rev = TRUE, alpha = 0.3)
#
r <- sqrt(t$disp/pi) # calculate radius
symbols(t$wt, t$mpg, circle = r, inches = 0.2,
        fg = "white", bg = col2[4])
text(t$wt, t$mpg, rownames(t), cex = 0.4, pos = 2) # the option inches 
#     is a scaling factor that can be used to control the size of the 
#     circles (the default is to make the largest circle 1 inch)
####
t <- subset(Orange, Tree == 1)
plot(t$age, t$circumference)
plot(t$age, t$circumference, type = "b") # the most common one for line 
#     charts
plot(t$age, t$circumference, type = "o") # Over-plotted points (that 
#     is, lines overlaid on top of points)
plot(t$age, t$circumference, type = "c") # points empty joined by lines
#
plot(t$age, t$circumference, type = "h")
#
plot(t$age, t$circumference, type = "s") # first runs then rises
plot(t$age, t$circumference, type = "S") # first rises than runs
#
plot(t$age, t$circumference, type = "p")
####
sapply(t, class)
t$Tree <- as.numeric(t$Tree)
ntree <- max(t$Tree)
#
xrange <- range(t$age)
yrange <- range(t$circumference)
#
plot(xrange, yrange, type = "n",
     xlab = "Age (days)",
     ylab = "Circumstance (mm)")
#
col <- RColorBrewer::brewer.pal(10, "RdBu")
linetype <- 1:ntree
plotchar <- seq(17, 17 + ntree, 1)
# 
for (i in 1:ntree) {
  tree <- subset(t, Tree == i)
  lines(tree$age, tree$circumference,
        lwd = 2, lty = linetype[i], 
        col = col[i], pch = plotchar[i])
}
title("Tree Growth")
legend(xrange[1], yrange[2], 1:ntree, cex = 0.8, 
       col = col, pch = plotchar,
       lty = linetype)
#legend("topleft", as.character(ntree), cex = 0.8,
#       col = col, pch = plotchar,
#       lty = linetype)
####
require("corrgram")
round(cor(t), 2)
corrgram::corrgram(t, order = TRUE, 
                   lower.panel = panel.shade,
                   upper.panel = panel.pie,
                   text.panel = panel.txt,
                   main = "corrgram of mtcars intercorrelation")
## panel.shade
# lower panel a blue color and hashing that goes from lower left to upper
#     right represent a positive correlation between the two variables
#     that meet at that cell
# The darker and more saturated the color, the greater the magnitude of
#     the correlation. 
# the rows and columns have been reordered (using principal compo-
#     nents analysis) to cluster variables together that have similar 
#     correlation patterns.
## panel.pie
# Here, color plays the same role, but the strength of the correlation 
#     is displayed by the size of the filled pie slice. Positive 
#     correlations fill the pie starting at 12 o’clock and moving in a
#     clockwise direction. Negative correlations fill the pie by moving 
#     in a counterclockwise direction.
####
labs <- colnames(t)
corrgram(t, order = TRUE, lower.panel = panel.fill, upper.panel = panel.conf,
         diag.panel = panel.density, 
         outer = list(bottom = list(labels = labs, cex = 1.5, srt = 60),
                      left = list(labels = labs, cex = 1.5, srt = 30)))
####
corrgram(t, order = TRUE, lower.panel = panel.ellipse, upper.panel = panel.pts,
         diag.panel = panel.minmax)
# The lower triangle contains smoothed best-fit lines and confidence ellipses
####
corrgram(t, lower.panel = panel.shade, upper.panel = NULL, 
         text.panel = panel.txt, 
         font.labels = 2, label.srt = -30)
####
col <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                            "darkkhaki", "darkgreen"))
corrgram(t, order = TRUE, lower.panel = panel.ellipse, 
         upper.panel = panel.pie, diag.panel = panel.density, 
         outer = list(bottom = list(labels = labs, cex = 1.5, font = 3, srt = 60),
                      left = list(labels = labs, cex = 1.5, font = 2, srt = 30)),
         col.regions = col)
####
corrgram(iris, lower.panel=panel.pts, upper.panel=panel.conf,
diag.panel=panel.density)
##
corrgram(auto, order=TRUE, main="Auto data (PC order)",
        # When order=TRUE, the variables are reordered using a principal 
        # component analysis of the correlation matrix. 
        # Reordering can help make patterns of bivariate relationships 
        # more obvious.
        lower.panel=panel.ellipse,
        # lower.panel and upper.panel separate panel functions used below/above
        # the diagonal
        upper.panel=panel.bar, 
        # off-diagnoal panels are specified with panel.pts, panel.pie, panel.shade,
        #     panel.fill, panel.bar, panel.ellipse, panel.conf, panel.cor
        diag.panel=panel.minmax,
        # diag.panel is the panel function used on the diagonal
        # diag.panels are specified with panel.txt, panel.minmax, panel.density
        col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1",
                                       "darkkhaki", "darkgreen"))
        # col.regions to return a vector of color
        )
##
labs <- colnames(state.x77)
corrgram(state.x77, oma=c(7, 7, 2, 2),
         outer.labels=list(bottom=list(labels=labs,cex=1.5,srt=60),
                           # srt is the string rotation for labels
                           left=list(labels=labs,cex=1.5,srt=30)))

mtext("Bottom", side=1, cex=2, line = -1.5, outer=TRUE, xpd=NA)
mtext("Left", side=2, cex=2, line = -1.5, outer=TRUE, xpd=NA)
####
# effectsize::interpret, giving some criterions to interpret correlation,
#     standard difference d (cohen's d), odds ratio, coefficient of 
#     determination (R2), and the interpretation of other indices
####
require("vcd")
ftable(Titanic)
mosaic(Titanic, shade = TRUE, legend = TRUE)
# adding the option shade=TRUE colors the figure based on Pearson residuals
#     from a fitted model (independence by default)
# the option legend=TRUE displays legend for these residuals.
#
mosaic(~ Class + Sex + Age + Survived, Titanic, shade = TRUE, legend = TRUE)
# In this example, the blue shading indicates cross-classifications that 
#     occur more often than expected, assuming that survival is unrelated to class, gender, and
#     age. 
# Red shading indicates cross-classifications that occur less often than 
#     expected under the independence model. 
# The graph indicates that more first-class women survived and more
#     male crew members died than would be expected under an 
#     independence model.
# Fewer third-class men survived than would be expected if survival was 
#     independent of class, gender, and age. 
####
?mosaic
#
mosaic(PreSex, condvars = c(1,4))
# convar is the vector of integers or character strings indicating 
#     conditioning variables, if any. The table will be permuted 
#     to order them first.
mosaic(~ ExtramaritalSex + PremaritalSex | MaritalStatus + Gender,
       data = PreSex)
#
mosaic(HairEyeColor, shade = TRUE, expected = list(c(1,2), 3))
# expected: optionally, an array of expected values of the same 
#     dimension as x, or alternatively the corresponding 
#     independence model specification as used by loglin or 
#     loglm (see strucplot).
## Model of joint independence of sex from hair and eye color.  Males
## are underrepresented among people with brown hair and eyes, and are
## overrepresented among people with brown hair and blue eyes, but not
## "significantly".
####





