#load relevant packages
library(readr)
library(dplyr)

#convert the numeric variables to numeric & factor variables to factors 
AB_test = AB_test %>% 
  mutate_at(vars(trans_after,revenue_after,num_past_purch,spent_last_purchase,
                 weeks_since_visit, browsing_minutes),as.numeric) %>% 
  mutate_at(vars(id,minority,female,test_coupon, channel_acq, shopping_cart),
            as.factor)

#recode the channel_acq factor to set an appropriate baseline
levels(AB_test$channel_acq) <- c("Google", "Facebook", "Instragram","Referral", "Other")


attach(AB_test)
##Create boxplot
library(ggplot2)

p<-ggplot(AB_test, aes(x=test_coupon, y=revenue_after, color=test_coupon)) +
  geom_boxplot()


###Hypothesis Testing to check difference between groups and see if coupons were randomly assigned among customers

t.test(trans_after ~ test_coupon, data = AB_test)$p.value
t.test(revenue_after ~ test_coupon, data = AB_test)$p.value
t.test(num_past_purch ~ test_coupon, data = AB_test)$p.value
t.test(weeks_since_visit ~ test_coupon, data = AB_test)$p.value
t.test(browsing_minutes ~ test_coupon, data = AB_test)$p.value
t.test(spent_last_purchase ~ test_coupon, data = AB_test)$p.value

## All the p-values are greater than 0.05, so it indicates a weak evidence against the null hypothesis . 
## Then we fail to reject the null hypothesis.(Ho: test_coupon=0 = test_coupon=1)
## Therefore, there is no significant difference between the two groups and coupons have been randomly assigned.


####Regression to check if there is evidence that coupons increase transactions or revenues or both?

mreg1=lm(trans_after~test_coupon+minority+female+channel_acq+num_past_purch+spent_last_purchase+weeks_since_visit+browsing_minutes+shopping_cart)
summary(mreg1)
## P-value for test_coupon is < 0.05 --> Coupons increase number of transactions
mreg2=lm(revenue_after~test_coupon+minority+female+channel_acq+num_past_purch+spent_last_purchase+weeks_since_visit+browsing_minutes+shopping_cart)
summary(mreg2)
## P-value for test_coupon is > 0.05 ---> Coupons does not have a significant impact on revenue.
## if Artea's goal is to increase sales revenues, coupons only may not be the best strategy to target potential customers.
## We could focus on Marketing strategy (advertising) than sending coupon to motivate customers using channel_aqc platforms (based on P-values, it seems to have positive impact on revenue than coupons)



#### checking impact of coupon on customer that have products in their shopping_cart using interaction "shopping_cart*test_coupon"
mreg3=lm(revenue_after~test_coupon+minority+female+channel_acq+num_past_purch+spent_last_purchase+weeks_since_visit+browsing_minutes+shopping_cart+shopping_cart*test_coupon)
summary(mreg3)

## P-value for shopping_cart*test_coupon < 0.05. Meaning coupons are effective for people who added products to their shopping cart during their last 
## visit on the website. (Strategy: Sending coupons to customers with products on their shopping cart)
## P-value for browsing_minutes > 0.5--> this could imply that the more minutes customers spent on the website the more issues they find on the 
## products and then end up not adding to their shopping cart and less revenue.