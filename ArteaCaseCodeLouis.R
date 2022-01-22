######Imports
install.packages("readxl")
library("readxl")
library(ggplot2)
library(gridExtra)
library(dplyr)
##### Data Handling

ABtest <- read_excel("521703-XLS-ENG.xlsx", sheet=2)
nextcampaign <- read_excel("521703-XLS-ENG.xlsx", sheet=3)

summary(ABtest)

attach(ABtest)

###Histogram plots of every variable
histplot = function (data, column) {
  ggplot(data, aes_string(x = column)) +
    geom_histogram(fill = "blue") +
    xlab(column) 
    
}
list.histplots <- lapply(colnames(ABtest), histplot, data = ABtest)
names(list.histplots) <- colnames(ABtest)

#Arrange in grid
n <- length(list.histplots)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots, ncol=nCol))

##Histogram plots of every variable: group test_coupon=1
list.histplots_gp1 <- lapply(colnames(ABtest), histplot, data = ABtest[test_coupon == 1,])
names(list.histplots_gp1) <- colnames(ABtest)

#Arrange in grid
n <- length(list.histplots_gp1)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots_gp1, ncol=nCol))

##Histogram plots of every variable: group test_coupon=0
list.histplots_gp0 <- lapply(colnames(ABtest), histplot, data = ABtest[test_coupon == 0,])
names(list.histplots_gp0) <- colnames(ABtest)

#Arrange in grid
n <- length(list.histplots_gp0)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.histplots_gp0, ncol=nCol))

###Scatter plots of every variable
pairs(ABtest[,2:12], pch=10, lower.panel=NULL)

###Scatter plots to check relation between revenue_after and explanatory variables
scatplot_rev = function (data, column) {
  ggplot(data, aes_string(y=ABtest$revenue_after, x=column)) +
    geom_point() +
    xlab(column) +
    ylab("revenue_after")
  
}
vec.vars <- c(2:12)
list.scatplots_rev <- lapply(colnames(ABtest[,vec.vars]), scatplot_rev, data = ABtest)
names(list.scatplots_rev) <- colnames(ABtest[,vec.vars])

n <- length(list.scatplots_rev)
nCol <- floor(sqrt(n))
do.call("grid.arrange", c(list.scatplots_rev, ncol=nCol))


###Check normality of distributions
#For revenue_after: not normal
with(ABtest, shapiro.test(revenue_after[test_coupon == 0]))
with(ABtest, shapiro.test(revenue_after[test_coupon == 1]))

 

###Hypothesis testing: check if two groups are comparable
#For that, use a non parametric approach due to large differences in distributions
#Wilcoxon two-sample so Mann-Whitney, null hypothesis of same distribution
wilc.df <- data.frame(matrix(ncol = (length(ABtest) - 1), nrow = 1))
colnames(wilc.df) <- colnames(ABtest[,2:12])

wilc.df["trans_after"] <- wilcox.test(trans_after ~ test_coupon, data = ABtest)$p.value #trans_after
wilc.df["revenue_after"] <- wilcox.test(revenue_after ~ test_coupon, data = ABtest)$p.value #Revenue_after
wilc.df["minority"] <- wilcox.test(minority ~ test_coupon, data = ABtest)$p.value #minority
wilc.df["female"] <- wilcox.test(female ~ test_coupon, data = ABtest)$p.value #female
wilc.df["channel_acq"] <- wilcox.test(channel_acq ~ test_coupon, data = ABtest)$p.value #channel_acq
wilc.df["num_past_purch"] <- wilcox.test(num_past_purch ~ test_coupon, data = ABtest)$p.value #num_past_purch
wilc.df["spent_last_purchase"] <- wilcox.test(spent_last_purchase ~ test_coupon, data = ABtest)$p.value #spent_last_purchase
wilc.df["weeks_since_visit"] <- wilcox.test(weeks_since_visit ~ test_coupon, data = ABtest)$p.value #weeks_since_visit
wilc.df["browsing_minutes"] <- wilcox.test(browsing_minutes ~ test_coupon, data = ABtest)$p.value #browsing_minutes
wilc.df["shopping_cart"] <- wilcox.test(shopping_cart ~ test_coupon, data = ABtest)$p.value #shopping_cart

#Can see that all of the variables have similar distributions even in groups


#Sum of revenue by test coupon group
sum.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=sum)
#Mean of revenue by test coupon group
avg.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=mean)
#Stdev of revenue
std.rev <- aggregate(ABtest$revenue_after, by=list(Category=ABtest$test_coupon), FUN=sd)

#Sum of transactions by test coupon group
sum.trans <- aggregate(ABtest$trans_after, by=list(Category=ABtest$test_coupon), FUN=sum)
#Mean of revenue by test coupon group
avg.trans <- aggregate(ABtest$trans_after, by=list(Category=ABtest$test_coupon), FUN=mean)



#Basic linear regressions
test.lreg <- lm(revenue_after ~ revenue_after + test_coupon + minority + female + channel_acq + 
                  num_past_purch  + weeks_since_visit + shopping_cart + spent_last_purchase, data = ABtest)
summary(test.lreg)

test.lreg_transac <- lm(trans_after ~ revenue_after + test_coupon + minority + female + channel_acq + 
                          num_past_purch  + weeks_since_visit + shopping_cart + spent_last_purchase, data = ABtest)
summary(test.lreg_transac)

