###### Chapter 19 Advanced graphics with ggplot2
####
#  In addition to base graphics, graphics systems are provided by the grid, 
#     lattice, and ggplot2 packages.
#  The grid graphics system provides low-level access to graphic primitives, 
#     giving programmers a great deal of flexibility in the creation of graphic
#     output. 
# The lattice package provides an intuitive approach for examining multivariate
#     relationships through conditional one-, two-, or three-dimensional 
#     graphs called trellis graphs.
# The ggplot2 package provides a method of creating innovative graphs based
#     on a comprehensive graphical “grammar.”
####
# grid graphics offer a lower-level alternative to the standard graphics 
#     system. The user can create arbitrary rectangular regions on graphics 
#     devices, define coordinate systems for each region, and use a rich set
#     of drawing primitives to control the arrangement and appearance of 
#     graphic elements. 
# grid package doesn’t provide functions for producing statistical graphics or
#     complete plots. 
####
# Basically, trellis graphs display the distribution of a variable or the 
#     relationship between variables, separately for each level of one or 
#     more other variables. Built using the grid package, the lattice package
#     has grown beyond Cleveland’s original approach to visualizing 
#     multivariate data and now provides a comprehensive alternative system
#     for creating statistical graphics in R. 
####
# The intention of the ggplot2 package is to provide a comprehensive, 
#     grammar-based system for generating graphs in a unified and coherent 
#     manner, allowing users to create new and innovative data visualizations.
# The power of this approach has led ggplot2 to become an important tool for 
#     visualizing data using R.
####
require("ggplot2")
#  In ggplot2, plots are created by chaining together functions using the
#     plus (+) sign. Each function modifies the plot created up to that point. 
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() + 
  labs(title = "Automobile Data", x = "Weight", y = "Miles Per Gallon")
# The ggplot() function initializes the plot and specifies the data source
#     (mtcars) and variables (wt, mpg) to be used. 
# The options in the aes() function specify what role each variable will play.
#     (aes stands for aesthetics, or how information is represented visually.)
# The ggplot() function sets up the graph but produces no visual output on its
# own.
# Geometric objects (called geoms for short), which include points, lines, 
#     bars, box plots, and shaded regions, are added to the graph using one or 
#     more geom functions. 
####
ggplot(data = mtcars, aes(x = wt, y = mpg)) + 
  geom_point(pch = 17, color = col[1]) + 
  geom_smooth(method = "lm", col = "red", linetype = 2)
# The geom_smooth() function adds a “smoothed” line. Here a linear fit is 
#     requested (method="lm") and a red color="red") dashed (linetype=2) line
#     of size 1 (size=1) is produced. By default, the line includes 95% 
#     confidence intervals (the darker band). 
####
#  Grouping displays two or more groups of observations in a single plot. 
#     Groups are usually differentiated by color, shape, or shading. 
# Faceting displays groups of observations in separate, side-by-side plots. 
#     The ggplot2 package uses factors when defining groups or facets. 
####
t <- mtcars
t$am <- factor(t$am, levels = c(0, 1),
               labels = c("Automatic", "Manual"))
t$vs <- factor(t$vs, levels = c(0, 1),
               labels = c("V-Engine", "Straight Engine"))
t$cyl <- factor(t$cyl)
#
ggplot(t, aes(x = hp, y = mpg,
                   shape = cyl, color = cyl)) + 
  geom_point(size = 3) + 
  facet_grid(am ~ vs) + 
  labs(title = "automobile data by  engline type", 
       x = "horsepower", y = "miles per gallon")
# facet_grid() forms a matrix of panels defined by row and column faceting 
#     variables. It is most useful when you have two discrete variables, and
#     all combinations of the variables exist in the data. 
# If you have only one variable with many levels, try facet_wrap().
# A scatterplot showing the relationship between horsepower and 
#     gas mileage separately for transmission and engine type. The number of
#     cylinders in each automobile engine is represented by both shape and
#     color.  
# In this case, am and vs are the faceting variables, and cyl is the grouping
#     variable. 
####
# Whereas the ggplot() function specifies the data source and variables to be
#     plotted, the geom functions specify how these variables are to be 
#     visually represented 
data(singer, package = "lattice")
t <- singer
# 
ggplot(data = t, aes(x = height)) + 
  geom_histogram()
# 
ggplot(data = t, aes(x = voice.part, y = height)) + 
  geom_boxplot()
#
ggplot(data = t, aes(x = voice.part)) + 
  geom_boxplot() 
# The geom_histogram() function defaults to counts on the y-axis when no y 
#     variable is specified.
####
# position: Position of plotted objects such as bars and points. For bars, 
#     "dodge" places grouped bar charts side by side, "stacked" vertically
#     stacks grouped bar charts, and "fill" vertically stacks grouped bar 
#     charts and standardizes their heights to be equal. For  points, 
#     "jitter" reduces point overlap
# sides: Placement of rug plots on the graph ("b" = bottom, "l" = left, 
#     "t" = top, "r" = right,  "bl" = both bottom and left, and so on). 
#### 
data(Salaries, package = "carData")
t <- Salaries
summary(t)
# 
ggplot(t, aes(x = rank, y = salary)) + 
  geom_boxplot(fill = "cornflowerblue", color = "black", notch = TRUE) + 
  geom_point(position = "jitter", color = col[1], alpha = 0.3) + 
  geom_rug(sides = "l", color = "black")
# points use "jitter" to aviod overlap
# rug plot in the left side to show the spread of salaries
# Additionally, the variance in salaries increases with greater rank, with a 
#     large range of salaries for full professors. 
####
t <- singer
ggplot(t, aes(x = voice.part, y = height)) + 
  geom_violin(fill = col[3]) + 
  geom_boxplot(fill = "lightgreen", width = 0.2)
# The real power of the ggplot2 package is realized when geoms are combined 
#     to form new types of plots
# The box plots show the 25th, 50th, and 75th percentile scores for each 
#     voice part in the singer dataframe, along with any outliers. 
# The violin plots provide more visual cues as to the distribution of scores
#     over the range of heights for each voice part.
####
# Grouping is accomplished in ggplot2 graphs by associating one or more 
#     grouping variables with visual characteristics such as shape, color,
#     fill, size, and line type. 
# The aes() function in the ggplot() statement assigns variables to roles
#     (visual characteristics of the plot), so this is a natural place to 
#     assign grouping variables
t2 <- Salaries
ggplot(t2, aes(x = salary, fill = rank)) + 
  geom_density(alpha = 0.3)
# plots three density curves in the same graph (one for each level of 
#     academic rank) and distinguishes them by fill color.
#  As rank increases, so does the range of salaries. This is especially true
#     for full professors, who have wide variation in their incomes. 
####
ggplot(t2, aes(x = yrs.since.phd, y = salary, color = rank, shape = sex)) + 
  geom_point()
####
ggplot(t2, aes(x = rank, fill = sex)) + 
  geom_bar(position = "stack") +
  labs(title = "postion = stack")
#
ggplot(t2, aes(x = rank, fill = sex)) + 
  geom_bar(position = "fill") + 
  labs(title = "position = fill", y = "proportion")
#
ggplot(t2, aes(x = rank, fill = sex)) + 
  geom_bar(position = "dodge") + 
  labs(title = "postion = dodge")
####
ggplot(t2, aes(x = sex, fill = rank)) + geom_bar()
# 
ggplot(t2, aes(x = sex)) + geom_bar(fill = col[2])
# In general, variables should go inside aes(), and assigned constants
#     should go outside aes()
####
# You can create trellis graphs (called faceted graphs in ggplot2) using the
#     facet_wrap() and facet_grid() functions.
ggplot(t, aes(x = height)) + 
  geom_histogram() +
  facet_wrap(~voice.part, nrow = 4)
# 
ggplot(t, aes(x = height)) + 
  geom_histogram(fill = col[2], notch = TRUE) +
  facet_wrap(~voice.part, ncol = 2)
# Separating the eight distributions into their own small, side-by-side plots
#     makes them easier to compare
####
ggplot(t2, aes(x = yrs.since.phd, y = salary, color = rank, shape = rank)) +
  geom_point() +
  facet_grid(.~sex)
# Separating the eight distributions into their own small, side-by-side plots
#     makes them easier to compare     
# Scatterplot of years since graduation and salary. 
# Academic rank is represented by color and shape, and sex is faceted.
####
ggplot(t, aes(x = height, fill = voice.part)) + 
  geom_density(alpha = 0.3) + 
  facet_grid(voice.part~.)
# Note that the horizontal arrangement facilitates comparisons among the 
#     groups
####
# The ggplot2 package contains a wide range of functions for calculating 
#     statistical summaries that can be added to graphs. 
#  These include functions for binning data and calculating densities, 
#     contours, and quantiles. 
# method=: Smoothing function to use. Allowable values include lm, glm, 
#     smooth, rlm, and gam, for linear, generalized linear, loess, robust 
#     linear, and generalized additive modeling, respectively. smooth is the 
#     default.
# formula=: Formula to use in the smoothing function. Examples include 
#     y~x (the default), y~log(x), y~poly(x,n) for an nth degree polynomial 
#     fit, and y~ns(x,n) for a  spline fit with n degrees of freedom.
# se: Plots confidence intervals (TRUE/FALSE). TRUE is the default.
# level: Level of confidence interval to use (95% by default).
# fullrange: Specifies whether the fit should span the full range of the plot
#     (TRUE) or just the data (FALSE). FALSE is the default.
ggplot(t2, aes(x = yrs.since.phd, y = salary)) + 
  geom_point() + 
  geom_smooth()
# The plot suggests that the relationship between experience and salary 
#     isn’t linear, at least when considering faculty who graduated many 
#     years ago.
####
ggplot(t2, aes(x = yrs.since.phd, y = salary, shape = sex, color = rank,
               linetype = sex)) + 
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
              se = FALSE, size = 1)
# The confidence limits are suppressed to simplify the graph (se=FALSE). 
# Genders are differentiated by symbol shape and line type. 
# rank is differentiated by color
# Scatterplot of years since graduation vs. salary with separate fitted 
#     quadratic regression lines for rank and sex
####
# The ggplot2 package contains a wide range of statistical functions
#     (called stat functions) for calculating the quantities necessary to 
#     produce a variety of data visualizations. 
# Typically, geom functions call the stat functions implicitly, and you won’t
#     need to deal with them directly. But it’s useful to know they exist.
#     Each stat function has help pages that can aid you in understanding how
#     the geoms work. 
# For example, the geom_smooth() function relies on the stat_smooth() function
#     to calculate the quantities needed to plot a fitted line and its 
#     confidence limits. 
# The help page for geom_smooth() is sparse, but the help page for 
#     stat_smooth() contains a wealth of useful information. When exploring
#     how a geom works and what options are available, be sure to check out
#     both the geom function and its related stat function(s).
####
# You’ll learn how to customize the appearance of axes (limits, tick marks, 
#     and tick mark labels), the placement and content of legends, and the 
#     colors used to represent variable values. 
# You’ll also learn how to create custom themes (allowing you to add a 
#     consistent look and feel to your graphs) and arrange several plots into
#     a single graph. 
####
ggplot(t2, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c("AsstProf", "AssocProf", "Prof"),
                   labels = c("Assistant\n Professor",
                              "Associate\n Professor",
                              "Full\n Professor")) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000),
                     labels = c("$50K", "$100K", "$150K", "$200K")) +
  labs(title = "faculty salary by rank and sex", x = "", y = "")
# scale_x_discrete(), scale_y_discrete():
#     breaks= places and orders the levels of a factor, 
#     labels= specifies the labels for these levels,  
#     limits= indicates which levels should be displayed
# scale_x_continuous(), scale_y_continuous():
#     breaks= specifies tick marks, 
#     labels= specifies labels for tick marks, 
#     limits= controls the range of the values displayed.
####
#  When modifying a legend’s title, you have to take into account whether
#     the legend is based on color, fill, size, shape, or a combination.
#  The placement of the legend is controlled by the legend.position option 
#     in the theme() function. Possible values include "left", "top", 
#     "right" (the default), and "bottom". 
# Alternatively, you can specify a two-element vector that gives the 
#     position within the graph. 
ggplot(t2, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks = c("AsstProf", "AssocProf", "Prof"),
                   labels = c("Assistance\n Professor",
                              "Associate\n Professor",
                              "Full\n Professor")) +
  scale_y_continuous(breaks = c(50000, 100000, 150000, 200000),
                   labels = c("$50K", "$100K", "$150K", "$200K")) + 
  labs(title = "faculty salary by rank and sex", 
       x = "", y = "", fill = "Gender") +
  theme(legend.position = c(0.2, 0.8))
# the upper-left corner of the legend was placed 10% from the left
#     edge and 80% from the bottom edge of the graph. 
# If you want to omit the legend, use legend.position="none". 
# The theme() function can change many aspects of a ggplot2 graph’s 
#     appearance
####
# The ggplot2 package uses scales to map observations from the data space to
#     the visual space. Scales apply to both continuous and discrete 
#     variables. 
# Continuous scales can map numeric variables to other characteristics of 
#     the plot.
t3 <- mtcars
ggplot(t3, aes(x = wt, y = mpg, size = disp)) + 
  geom_point(shape = 21, color = col[2], fill = col[3]) + 
  labs(x = "weight", y = "mile per gallon", 
       title = "bubble chart", size = "engine \ndisplacement")
# The aes() parameter size=disp generates a scale for the continuous
#     variable disp (engine displacement) and uses it to control the size
#     of the points. 
####
# In the discrete case, you can use a scale to associate visual cues 
#     (for example, color, shape, line type, size, and transparency) with
#     the levels of a factor. 
ggplot(t2, aes(x = yrs.since.phd, y = salary, color = rank)) +
  scale_color_manual(values = col[1:3]) +
  geom_point(size = 2, shape = 19)
# uses the scale_color_manual() function to set the point colors for the
#     three academic ranks.
####
ggplot(t2, aes(x = yrs.since.phd, y = salary, color = rank)) +
  scale_color_brewer(palette = "PiYG") +
  geom_point(size = 2, shape = 10)
####
# Themes allow you to control the overall appearance of these graphs. 
#     Options in the theme() function let you change fonts, backgrounds, 
#     colors, gridlines, and more.
# Themes can be used once or saved and applied to many graphs.
mytheme <- theme(plot.title = element_text(face = "bold.italic",
                                           size = 14, 
                                           color = "brown"),
                 axis.title = element_text(face = "bold.italic",
                                           size = 10, 
                                           color = "brown"),
                 axis.text = element_text(face = "bold",
                                          size = 9,
                                          color = "darkblue"),
                 panel.background = element_rect(fill = "white",
                                                 color = "darkblue"),
                 panel.grid.major.y = element_line(color =  "grey",
                                                   linetype = 1),
                 panel.grid.minor.y = element_line(color = "grey",
                                                   linetype = 2),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "top")
#
ggplot(t2, aes(x = rank, y = salary, fill = sex)) +
  geom_boxplot() +
  labs(title = "salary by rank and sex", x = "rank", y = "salary") +
  mytheme
####
# The easiest way to place multiple ggplot2 graphs in a single figure is to
#     use the grid.arrange() function in the gridExtra package. 
p1 <- ggplot(t2, aes(x = rank)) + geom_bar()
p2 <- ggplot(t2, aes(x = sex)) + geom_bar()
p3 <- ggplot(t2, aes(x = yrs.since.phd, y = salary)) + geom_point()
#
require("gridExtra")
grid.arrange(p1, p2, p3, ncol = 2)
# Note the difference between faceting and multiple graphs.
# Faceting creates an array of plots based on one or more categorical
#     variables. 
# In this section, you’re arranging completely independent plots into a 
#     single graph.
####
ggsave(filename = "p1.png", plot = p1, width = 5, height = 4)
# You can save the graph in a different format by setting the file extension
#     to ps, tex, jpeg, pdf, jpeg, tiff, png, bmp, svg, or wmf. The wmf 
#     format is only available on Windows machines.
ggsave("whole.jpeg", width = 10, height = 8)
# If you omit the plot= option, the most recently created graph is saved. 
####
