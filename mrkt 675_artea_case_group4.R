library(dplyr)
library(ggplot2)

#loading artea dataset

artea_ab = read.csv("521703-XLS-ENG.xlsx - AB_test.csv")
artea_nc = read.csv("521703-XLS-ENG.xlsx - Next_Campaign.csv")

attach(artea_ab)
attach(artea_nc)

# summary of dataset
summary(artea_ab)
summary(artea_nc)

################ Graphical analysis ########################################
# analysis at channel level
df = artea_ab %>% group_by(channel_acq,test_coupon )  %>%
  summarise(revenue_after = sum(revenue_after),
            .groups = 'drop')

df$channel_acq = ifelse(df$channel_acq=="1","Google",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="2","Facebook",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="3","Instagram",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="4","Referral",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="5","Other",df$channel_acq)

df$test_coupon = ifelse(df$test_coupon=="1","Yes","No")

#ggplot(df, aes(fill=channel_acq, y=revenue_after, x=test_coupon)) + geom_bar(position="stack", stat = "identity")

ggplot(df, aes(fill=channel_acq, y=revenue_after, x=test_coupon)) + 
  geom_bar(position="dodge", stat="identity")

# at gender and minor level
df2 = artea_ab %>% group_by(female,minority )  %>%
  summarise(revenue_after = sum(revenue_after),
            .groups = 'drop')

df2$female = ifelse(df2$female=="1","Yes","No")
df2$minority = ifelse(df2$minority=="1","Yes","No")

ggplot(df2, aes(fill=minority, y=revenue_after, x=female)) + 
  geom_bar(position="dodge", stat="identity")

# female expenditure analysis
df3 = artea_ab %>% group_by(female)  %>%
  summarise(revenue_after = sum(revenue_after),
            .groups = 'drop')

df3$female = ifelse(df3$female=="1","Yes","No")

ggplot(df3, aes(y=revenue_after, x=female)) + 
  geom_bar(stat="identity")

# Minority expenditure analysis
df4 = artea_ab %>% group_by(minority )  %>%
  summarise(revenue_after = sum(revenue_after),
            .groups = 'drop')

df4$minority = ifelse(df4$minority=="1","Yes","No")

ggplot(df4, aes(y=revenue_after, x=minority)) + 
  geom_bar(stat="identity")

#################### General analysis of variables with test coupon ##################
# average weeks since visits for coupon holders
df4 = artea_ab %>% group_by(test_coupon)  %>%
  summarise(avg_weeks = mean(weeks_since_visit),
            .groups = 'drop')

df4$test_coupon = ifelse(df4$test_coupon=="1","Yes","No")

ggplot(df4, aes(y=avg_weeks, x=test_coupon)) + 
  geom_bar(stat="identity")

# time spent on web
df5 = artea_ab %>% group_by(channel_acq,test_coupon)  %>%
  summarise(avg_browse_mins = mean(browsing_minutes),
            .groups = 'drop')

df5$test_coupon = ifelse(df5$test_coupon=="1","Yes","No")
df5$channel_acq = ifelse(df5$channel_acq=="1","Google",df5$channel_acq)
df5$channel_acq = ifelse(df5$channel_acq=="2","Facebook",df5$channel_acq)
df5$channel_acq = ifelse(df5$channel_acq=="3","Instagram",df5$channel_acq)
df5$channel_acq = ifelse(df5$channel_acq=="4","Referral",df5$channel_acq)
df5$channel_acq = ifelse(df5$channel_acq=="5","Other",df5$channel_acq)

ggplot(df5, aes(fill= test_coupon,y=avg_browse_mins, x=channel_acq)) + 
  geom_bar(position="dodge",stat="identity")

# number of past purchases at coupon level
df6 = artea_ab %>% group_by(channel_acq,test_coupon)  %>%
  summarise(avg_past_purchase = mean(num_past_purch),
            .groups = 'drop')

df6$test_coupon = ifelse(df6$test_coupon=="1","Yes","No")
df6$channel_acq = ifelse(df6$channel_acq=="1","Google",df6$channel_acq)
df6$channel_acq = ifelse(df6$channel_acq=="2","Facebook",df6$channel_acq)
df6$channel_acq = ifelse(df6$channel_acq=="3","Instagram",df6$channel_acq)
df6$channel_acq = ifelse(df6$channel_acq=="4","Referral",df6$channel_acq)
df6$channel_acq = ifelse(df6$channel_acq=="5","Other",df6$channel_acq)

ggplot(df6, aes(fill= test_coupon,y=avg_past_purchase, x=channel_acq)) + 
  geom_bar(position="dodge",stat="identity")

########## graphical analysis of transactions ####################################
# analysis at channel level
df = artea_ab %>% group_by(channel_acq,test_coupon )  %>%
  summarise(trans_after = sum(trans_after),
            .groups = 'drop')

df$channel_acq = ifelse(df$channel_acq=="1","Google",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="2","Facebook",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="3","Instagram",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="4","Referral",df$channel_acq)
df$channel_acq = ifelse(df$channel_acq=="5","Other",df$channel_acq)

df$test_coupon = ifelse(df$test_coupon=="1","Yes","No")

#ggplot(df, aes(fill=channel_acq, y=revenue_after, x=test_coupon)) + geom_bar(position="stack", stat = "identity")

ggplot(df, aes(fill=channel_acq, y=trans_after, x=test_coupon)) + 
  geom_bar(position="dodge", stat="identity")

# at gender and minor level
df2 = artea_ab %>% group_by(female,minority )  %>%
  summarise(trans_after = sum(trans_after),
            .groups = 'drop')

df2$female = ifelse(df2$female=="1","Yes","No")
df2$minority = ifelse(df2$minority=="1","Yes","No")

ggplot(df2, aes(fill=minority, y=trans_after, x=female)) + 
  geom_bar(position="dodge", stat="identity")

# female expenditure analysis
df3 = artea_ab %>% group_by(female)  %>%
  summarise(trans_after = sum(trans_after),
            .groups = 'drop')

df3$female = ifelse(df3$female=="1","Yes","No")

ggplot(df3, aes(y=trans_after, x=female)) + 
  geom_bar(stat="identity")

# Minority expenditure analysis
df4 = artea_ab %>% group_by(minority )  %>%
  summarise(trans_after = sum(trans_after),
            .groups = 'drop')

df4$minority = ifelse(df4$minority=="1","Yes","No")

ggplot(df4, aes(y=trans_after, x=minority)) + 
  geom_bar(stat="identity")

################# t-test for variables #############################################
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$num_past_purch)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$num_past_purch)

t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$spent_last_purchase)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$spent_last_purchase)

t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$weeks_since_visit)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$weeks_since_visit)

t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$browsing_minutes)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$browsing_minutes)

# Just for reference (not useful to compare the data split was done randomly since 
#                     the columns used below came after the promotion)

t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$trans_after)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$trans_after)

t.test(artea_ab$test_coupon[artea_ab$test_coupon=="1"], artea_ab$revenue_after)
t.test(artea_ab$test_coupon[artea_ab$test_coupon=="0"], artea_ab$revenue_after)

########################### dummy variables for channels####################################
artea_ab$facebook <- ifelse(artea_ab$channel_acq == 2, 1, 0)
artea_ab$google <- ifelse(artea_ab$channel_acq == 1, 1, 0)
artea_ab$instagram <- ifelse(artea_ab$channel_acq == 3, 1, 0)
artea_ab$other <- ifelse(artea_ab$channel_acq == 5, 1, 0)
artea_ab$referral <- ifelse(artea_ab$channel_acq == 4, 1, 0)

#artea_ab = artea_ab %>% 
#  mutate_at(vars(trans_after,revenue_after,num_past_purch,spent_last_purchase,
#                 weeks_since_visit, browsing_minutes),as.numeric) %>% 
#  mutate_at(vars(id,minority,female,test_coupon, channel_acq, shopping_cart),
#            as.factor)

################# correlation within data ##########################################
# transaction after
cor(artea_ab$trans_after, artea_ab$revenue_after, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$test_coupon, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$minority, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$female, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$facebook, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$google, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$instagram, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$other, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$referral, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$num_past_purch, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$spent_last_purchase, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$weeks_since_visit, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$browsing_minutes, method = 'spearman')
cor(artea_ab$trans_after, artea_ab$shopping_cart, method = 'spearman')

# revenue after
cor(artea_ab$revenue_after, artea_ab$revenue_after, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$test_coupon, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$minority, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$female, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$facebook, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$google, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$instagram, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$other, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$referral, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$num_past_purch, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$spent_last_purchase, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$weeks_since_visit, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$browsing_minutes, method = 'spearman')
cor(artea_ab$revenue_after, artea_ab$shopping_cart, method = 'spearman')

####################################### regression model######################################
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")

library(tidyverse)
library(caret)
library(leaps)
library(MASS)
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
step.model <- train(revenue_after ~ facebook+
                                    google+
                                    instagram+
                                    other+
                                    referral+
                                    num_past_purch+
                                    spent_last_purchase+
                                    weeks_since_visit+
                                    browsing_minutes+
                                    shopping_cart, 
                    data = artea_ab,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

step.model$bestTune

summary(step.model$finalModel)

coef(step.model$finalModel, 5)

model = lm(revenue_after ~ 
#     female+ (removing female to validate the next campaign data set as it don't have female)
     google+
     num_past_purch+
     spent_last_purchase+
     weeks_since_visit+
     shopping_cart, 
   data = artea_ab)

summary(model)

########################### Model validation on next campaign dataset ###################################

test_data = artea_nc

test_data$facebook <- ifelse(test_data$channel_acq == 2, 1, 0)
test_data$google <- ifelse(test_data$channel_acq == 1, 1, 0)
test_data$instagram <- ifelse(test_data$channel_acq == 3, 1, 0)
test_data$other <- ifelse(test_data$channel_acq == 5, 1, 0)
test_data$referral <- ifelse(test_data$channel_acq == 4, 1, 0)

sapply(test_data, class)

predictions <- predict(model, test_data)

data.frame(R2 = R2(predictions, test_data $ Volume), 
           RMSE = RMSE(predictions, test_data $ Volume), 
           MAE = MAE(predictions, test_data $ Volume))

######### Regression of trans after and revenue after without backward  elimination and max r-squared value
######### Also, using features which are not present in validation set (next campaign data)

model2 = lm(revenue_after ~ 
             test_coupon+
             facebook+
             google+
             instagram+
             other+
             referral+
             num_past_purch+
             spent_last_purchase+
             weeks_since_visit+
             browsing_minutes+
             shopping_cart, 
           data = artea_ab)

summary(model2)

model3 = lm(trans_after ~ 
             test_coupon+
             facebook+
             google+
             instagram+
             other+
             referral+
             num_past_purch+
             spent_last_purchase+
             weeks_since_visit+
             browsing_minutes+
             shopping_cart, 
           data = artea_ab)

summary(model3)

#################################################################################
############## Considering shopping cart * test coupon for regression ###########

artea_ab$coup_cart = (artea_ab$test_coupon * artea_ab$shopping_cart) 
model4 = lm(revenue_after ~ 
              test_coupon+
              channel_acq+
              #facebook+
              #google+
              #instagram+
              #other+
              #referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_cart, 
            data = artea_ab)

summary(model4)

############## Considering facebook * test coupon for regression ###########

artea_ab$coup_facebook = artea_ab$test_coupon * artea_ab$facebook 
model5 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_facebook, 
            data = artea_ab)

summary(model5)

############## Considering google * test coupon for regression ###########

artea_ab$coup_google = artea_ab$test_coupon * artea_ab$google 
model6 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_google, 
            data = artea_ab)

summary(model6)

############## Considering instagram * test coupon for regression ###########

artea_ab$coup_instagram = artea_ab$test_coupon * artea_ab$instagram 
model7 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_instagram, 
            data = artea_ab)

summary(model7)

############## Considering referral * test coupon for regression ###########

artea_ab$coup_referral = artea_ab$test_coupon * artea_ab$referral
model8 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_referral, 
            data = artea_ab)

summary(model8)

############## Considering other * test coupon for regression ###########

artea_ab$coup_other = artea_ab$test_coupon * artea_ab$other 
model9 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_other, 
            data = artea_ab)

summary(model9)

############## Considering browsing minutes * test coupon for regression ###########

artea_ab$coup_browse_mins = ifelse(artea_ab$browsing_minutes >= mean(artea_ab$browsing_minutes) & artea_ab$test_coupon == 1, artea_ab$browsing_minutes, 0)
model10 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_browse_mins, 
            data = artea_ab)

summary(model10)

############## Considering week since visit <= 1 * test coupon for regression ###########

artea_ab$coup_week_visit = ifelse(artea_ab$weeks_since_visit <= 1 & artea_ab$test_coupon == 1, 1, 0)
model11 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_week_visit, 
            data = artea_ab)

summary(model11)

############## Considering avg number of past purchase * test coupon for regression ###########

artea_ab$coup_past_pur = ifelse(artea_ab$num_past_purch >= mean(artea_ab$num_past_purch) & artea_ab$test_coupon == 1, 1, 0)
model12 = lm(revenue_after ~ 
              test_coupon+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              coup_past_pur, 
            data = artea_ab)

summary(model12)

##################################################################################################
################# Validating above models on next campaign data set 
################# three conditions: 
################# 1. All customers are sent coupons, 
################# 2. No coupons are sent to any customers 
################# 3. On groups created above in different models

##### model 4 ######
###  three situation for creating coup cart (0 for all,1 for all and 1 for people having cart loaded)
### (line 513, 517), (line 514,518 and (line 515, 519)

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$shopping_cart > 0, 1, 0)

test_data$coup_cart = 1
test_data$coup_cart = 0
test_data$coup_cart = test_data$test_coupon * test_data$shopping_cart

sapply(test_data, class)

pred1 <- predict(model4, test_data)
pred2 <- predict(model4, test_data)
pred3 <- predict(model4, test_data)

sum(pred1)
sum(pred2)
sum(pred3)

data.frame(R2 = R2(predictions, test_data), 
           RMSE = RMSE(predictions, test_data), 
           MAE = MAE(predictions, test_data))

##### model 6 ######

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$google > 0, 1, 0)

test_data$coup_google = 1
test_data$coup_google = 0
test_data$coup_google = test_data$test_coupon * test_data$google

sapply(test_data, class)

pred1 <- predict(model6, test_data)
pred2 <- predict(model6, test_data)
pred3 <- predict(model6, test_data)

sum(pred1)
sum(pred2)
sum(pred3)

##### model 7 ######

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$shopping_cart > 0, 1, 0)

test_data$coup_instagram = 1
test_data$coup_instagram = 0
test_data$coup_instagram = test_data$test_coupon * test_data$coup_instagram

sapply(test_data, class)

pred1 <- predict(model7, test_data)
pred2 <- predict(model7, test_data)
pred3 <- predict(model7, test_data)

sum(pred1)
sum(pred2)
sum(pred3)

##### model 12 ######

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$shopping_cart > 0, 1, 0)

test_data$coup_past_pur = 1
test_data$coup_past_pur = 0
test_data$coup_past_pur = ifelse(test_data$num_past_purch >= mean(test_data$num_past_purch) & test_data$test_coupon == 1, 1, 0)

sapply(test_data, class)

pred1 <- predict(model12, test_data)
pred2 <- predict(model12, test_data)
pred3 <- predict(model12, test_data)

sum(pred1)
sum(pred2)
sum(pred3)

### model13 (facebook+cart)
artea_ab$face_cart = (artea_ab$facebook * artea_ab$shopping_cart) 
model13 = lm(revenue_after ~ 
              test_coupon+
              channel_acq+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              face_cart , 
            data = artea_ab)

summary(model13)

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$facebook == 1 & test_data$shopping_cart > 1, 1, 0)

test_data$face_cart = 1
test_data$face_cart = 0
test_data$face_cart = ifelse(test_data$facebook == 1 & test_data$shopping_cart > 1, 1, 0)

sapply(test_data, class)

pred1 <- predict(model13, test_data)
pred2 <- predict(model13, test_data)
pred3 <- predict(model13, test_data)

sum(pred1)
sum(pred2)
sum(pred3)

## model 14 (instagram + cart)

artea_ab$insta_cart = (artea_ab$instagram * artea_ab$shopping_cart) 
model14 = lm(revenue_after ~ 
              test_coupon+
              channel_acq+
              facebook+
              google+
              instagram+
              other+
              referral+
              num_past_purch+
              spent_last_purchase+
              weeks_since_visit+
              browsing_minutes+
              shopping_cart+
              insta_cart, 
            data = artea_ab)

summary(model14)

test_data$test_coupon = 1
test_data$test_coupon = 0
test_data$test_coupon = ifelse(test_data$instagram == 1 & test_data$shopping_cart > 0, 1, 0)

test_data$insta_cart = 1
test_data$insta_cart = 0
test_data$insta_cart = ifelse(test_data$instagram == 1 & test_data$shopping_cart > 0, 1, 0)

sapply(test_data, class)

pred1 <- predict(model14, test_data)
pred2 <- predict(model14, test_data)
pred3 <- predict(model14, test_data)

sum(pred1)
sum(pred2)
sum(pred3)
