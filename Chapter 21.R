###### Chapter 21 Creating a packages
####
pkg <- "npar_1.0.tar.gz"
loc <- "http://www.statmethods.net/RiA"
url <- paste(loc, pkg, sep = "/")
download.file(url, pkg)
install.packages(pkg, repos = NULL, type = "source")
####
# Nonparametric methods are a data-analytic approach that is particularly 
#     useful when the assumptions of traditional parametric methods 
#     (such as normality and constant variance) can’t be met. 
require("npar")
t <- life
hist(t$hlef, breaks = 10)
# Clearly the outcome variable is negatively skewed, with fewer scores at the
#     low end
ggplot(t, aes(x = region, y = hlef)) +
  geom_point() + 
  theme_bw()
# The variance of HLE scores across regions can be visualized using a 
#     side-by-side dot chart
# two important ANOVA assumptions (normality and homogeneity of variance)
####
# Given a numerical dependent variable and a categorical grouping variable, it
#     provides
# ■ An omnibus Kruskal–Wallis test that the groups don’t differ.
# ■ Descriptive statistics for each group.
# ■ Post-hoc comparisons (Wilcoxon rank-sum tests) comparing groups two at a
#     time. The test p-values can be adjusted to take multiple testing into 
#     account.
# ■ Annotated side-by-side box plots for visualizing group differences.
results <- oneway(hlef ~ region, t) # computes nonparametric group comparisons,
#     including an omnibus test and post-hoc pairwise group comparisons.
summary(results)
# First, a Kruskal–Wallis test is performed b. This is an overall test of 
#     whether there are HLE differences between the regions. The small p-value
#     (.00005) suggests that there are.
# The HLE estimates are highest for the Northeast (median = 15.7 years) and 
#     lowest for the South (median = 13.0 years). The smallest variability 
#     among the states occurs in the Northeast (mad = 0.59), and the largest
#     occurs in the South (mad = 1.48).
# Although the Kruskal–Wallis test indicates that there are HLE differences 
#     among the regions, it doesn’t indicate where the differences lie. To 
#     determine this, you compare the groups two at a time using a Wilcoxon
#     rank-sum test d. With four groups, there are 4 × (4 – 1) / 2 or 6 
#     pairwise comparisons. 
# When computing multiple comparisons, you have to be concerned with alpha 
#     inflation: an increase in the probability of declaring groups to be 
#     significantly different when in fact they aren’t. For six independent
#     comparisons, the chances of finding at least one erroneous difference by
#     chance is 1 – (1 – .05)^6 or 0.26. 
# With a chance of finding at least one false pairwise difference hovering
#     around one in four, you’ll want to adjust the p-value for each 
#     comparison upward (make each test more stringent and less likely to 
#     declare a difference). 
# Doing so keeps the overall family-wise error rate (the probability of
#     finding one or more erroneous differences in a set of comparisons) at a
#     reasonable level (say, .05). 
# The p.adjust() function adjusts p-values to account for multiple
#     comparisons using one of several methods. The Bonferonni correction is 
#     perhaps the most well-known, but the Holm correction is more powerful 
#     and thus set as the default. 
plot(results, col = col[1], main = "multiple comparisons",
     xlab = "US region", 
     ylab = "females' healthy life expectacy (years) at Age 65 ")
####
# oneway indicates that there is a single grouping factor. 
####












