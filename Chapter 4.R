###### Chapter 4
####
leadership <- read.csv("wm.txt", header = TRUE, sep = "")
head(leadership, 2)
leadership$Age[leadership$Age == 99] <- NA
#
f <- cut(leadership$Age, c(20, 30, 40, 50), 
         labels = c("Young", "Middle Aged", "Elder"))
transform(leadership, AgeGroup = f)
####
require("car")
#
f <- car::recode(leadership$Age, "25 = 'Young'; 
                 32:39 = 'Middle Aged';
                 else = 'Elder'",
                 as.factor = TRUE)
transform(leadership, f = f)
####
require("doBy")
x <- rep(c(1, 2, 3), 10)
doBy::recodeVar(x, src = c(1, 3), tgt = c("HAPPY", "SAD"), 
                default = "So-So")
####
fix(leadership) # unable to use
####
names(leadership)[2] <- "Test Date"
####
plyr::rename(leadership,
             c(q1 = "Q1",
               q2 = "Q2",
               q3 = "Q3",
               q4 = "Q4",
               q5 = "Q5"))
####
rowSums(leadership[6:10]) # the fifth rows will be NA
rowSums(leadership[6:10], na.rm = TRUE)
####
colSums(is.na(leadership))
#
leadership[which(is.na(leadership$Age))]
####
na.omit(leadership) # delete any observations with NA, this method is
#     called listwise deletion
####
Sys.Date()
date()
#
format(Sys.Date(), "%a %d %m %Y")
format(Sys.Date(), "%F")
format(Sys.Date(), "%c")
####
myformat <- c("%m/%d/%y")
leadership$`Test Date` <- as.Date(leadership$`Test Date`, myformat) 
# it will automatically transfer it to the default form
####
birth <- as.Date("1999-10-19")
today <- Sys.Date()
difftime(Sys.Date(), birth, 
         units = "hours" )
####
birth <- as.character(birth) # transfer back to character, to do 
#     some character functions
####
?as.Date
as.Date(birth, tz = "NZ") # time zone
####
?strftime
format(Sys.time(), "%H:%M:%OS3") # "09:47:30.505"
format(Sys.time(), "%Y/%m/%d")
####
leadership <- plyr :: rename(leadership,  c(`Test Date`= "Date"))
#
func <- function(x) sum(x, na.rm = TRUE) / length(x)
lapply(leadership[6:10], func)
m <- .Last.value
leadership[4, 9] <- m[[4]]
leadership[4, 10] <- m[[5]]
#
transform(leadership, 
          Score = rowSums(leadership[7:10], na.rm = TRUE))
leadership <- .Last.value
#
with(leadership, {
  leadership[order(Date, Age),]
  leadership[order(Score, -Age),]}) # order is for ascending, and
#       add "-" will show descending
####
sort(leadership$Date) # the difference between sort and  order is that
#     sort only can order one variable while order is more than one
####
data(sleep)
t <- split(sleep, sleep$group)
cbind(t[[1]], t[[2]])
#
merge(t[[1]], t[[2]], by = "ID")
#
rbind(t[[1]], t[[2]])
####
myvar <- names(leadership) %in% c("q3", "q4")
leadership[!myvar] # delete q3, q4
#
leadership$q3 <- leadership$q4 <- NULL # delete q3, q4
####
with(leadership, {
  leadership[q1 >= m[[1]] & q3 >= m[[3]],]
})
#
leadership$Date <- as.Date(leadership$Date, myformat) 
with(leadership, {
  start <- as.Date("2014-01-01")
  end <- as.Date("2014-10-31")
  leadership[which(Date >= start & Date <= end),]
})
####
subset(leadership, Gender == "M" & Country == "US",
       select = c(1,q1:q5))
subset(leadership, Age <= 40  & q1 >= m[[1]] | Score <= 15,
       select = c(1, q1:q5))
####
leadership[sample(1:ncol(leadership), 2, replace = FALSE),]
####
require("survey")
require("sampling")
####
require("sqldf")
sqldf("select * from mtcars where carb = 1 order by mpg",
               row.names = TRUE)
sqldf("select  avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear
      from mtcars where cyl in (4, 6) group by gear")
####


