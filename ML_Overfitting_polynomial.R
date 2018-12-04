#################################ML ASSIGNMENT-1##################################
###################################################################################

#Installing Packages

install.packages("ggplot2")
library(ggplot2)

#Data 
df = read.csv("C://Users//Neeraj//Desktop//New folder//median_housing_price.csv")

dim(df)

#Considering median_income and median_house_value 
#as independent and dependent variables respectively 
plot(df2$median_income, df2$median_house_value, method = "scatter")



#Splitting data into train and test split

set.seed(1)

rand = sample(1:nrow(df), 50)
test = df[rand, ]
train = df[-rand, ]

########################################Problem -1################################
#Making my sample size varying(20 - len(train set)) training my model on order 7##
#polynomial regression model and plotting test rmse vs complexity               ##
##################################################################################

#Building model 6 for sample of 20

r = sample(1:nrow(train), 20)
tr_7 = train[r, ]
tr_7


m7 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
          + I(median_income^4) + I(median_income^5) 
         + I(median_income^6) +I(median_income^7), tr_7)

m7

#plotting for sample 20
plot(tr_7$median_income, tr_7$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_7$median_income), fitted(m7)[order(tr_7$median_income)], col='brown', type='l',pch=20)


#TRAIN AND TEST ACCURACY for sample size 20
sum(m7$residuals^2)
prd_7 = predict(m7, newdata=test)
test_err_s20 = sum((prd_7-test$median_income)^2)
test_err_s20

 
#Building model 6 for sample of 70

r1 = sample(1:nrow(train), 70)
tr_71 = train[r1, ]
dim(tr_71)


m7_2 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
         + I(median_income^4) + I(median_income^5) 
         + I(median_income^6) +I(median_income^7), tr_71)

m7_2


#plotting for sample 70
plot(tr_71$median_income, tr_71$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_71$median_income), fitted(m7_2)[order(tr_71$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 70
sum(m7_2$residuals^2)
prd_71 = predict(m7_2, newdata=test)
test_err_s70 = sum((prd_71-test$median_income)^2)
test_err_s70

#Building model 6 for sample of 150

r2 = sample(1:nrow(train), 150)
tr_72 = train[r2, ]
dim(tr_72)


m7_3 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_72)

m7_3


#plotting for sample 150
plot(tr_72$median_income, tr_72$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_72$median_income), fitted(m7_3)[order(tr_72$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 150
sum(m7_3$residuals^2)
prd_72 = predict(m7_3, newdata=test)
test_err_s150 = sum((prd_72-test$median_income)^2)
test_err_s150


#Building model 6 for sample of 190

r3 = sample(1:nrow(train), 190)
tr_73 = train[r3, ]
dim(tr_73)


m7_4 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_73)

m7_4


#plotting for sample 190
plot(tr_73$median_income, tr_73$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_73$median_income), fitted(m7_4)[order(tr_73$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 190
sum(m7_4$residuals^2)
prd_73 = predict(m7_4, newdata=test)
test_err_s190 = sum((prd_73-test$median_income)^2)
test_err_s190

#Building model 6 for sample of 390

r4 = sample(1:nrow(train), 390)
tr_74 = train[r4, ]
dim(tr_74)


m7_5 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_74)

m7_5

#plotting for sample 390
plot(tr_74$median_income, tr_74$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_74$median_income), fitted(m7_5)[order(tr_74$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 390
sum(m7_5$residuals^2)
prd_74 = predict(m7_5, newdata=test)
test_err_s390 = sum((prd_74-test$median_income)^2)
test_err_s390


#Building model 6 for sample of 690

r5 = sample(1:nrow(train), 690)
tr_75 = train[r5, ]
dim(tr_75)


m7_6 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_75)

m7_6

#plotting for sample 690
plot(tr_75$median_income, tr_75$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_75$median_income), fitted(m7_6)[order(tr_75$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 690
sum(m7_6$residuals^2)
prd_75 = predict(m7_6, newdata=test)
test_err_s690 = sum((prd_75-test$median_income)^2)
test_err_s690

#Building model 6 for sample of 1000

r6 = sample(1:nrow(train), 1000)
tr_76 = train[r6, ]
dim(tr_76)


m7_7 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_76)

m7_7

#plotting for sample 1000
plot(tr_76$median_income, tr_76$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_76$median_income), fitted(m7_7)[order(tr_76$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 1000
sum(m7_7$residuals^2)
prd_76 = predict(m7_7, newdata=test)
test_err_s1000 = sum((prd_76-test$median_income)^2)
test_err_s1000




#Building model 6 for sample of 1500

r7 = sample(1:nrow(train), 1500)
tr_77 = train[r7, ]
dim(tr_77)


m7_8 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_77)

m7_8


#plotting for sample 1500
plot(tr_77$median_income, tr_77$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_77$median_income), fitted(m7_8)[order(tr_77$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 1500
sum(m7_8$residuals^2)
prd_77 = predict(m7_8, newdata=test)
test_err_s1500 = sum((prd_77-test$median_income)^2)
test_err_s1500


#Building model 6 for sample of 1975
r8 = sample(1:nrow(train), 1975)
tr_78 = train[r8, ]
dim(tr_78)


m7_9 <- lm(median_house_value ~ median_income + I(median_income^2) + I(median_income^3) 
           + I(median_income^4) + I(median_income^5) 
           + I(median_income^6) +I(median_income^7), tr_78)

m7_9



#plotting for sample 1975
plot(tr_78$median_income, tr_78$median_house_value, pch = 19, cex = 0.5)
lines(sort(tr_78$median_income), fitted(m7_9)[order(tr_78$median_income)], col='brown', type='l',pch=20)



#TRAIN AND TEST ACCURACY for sample size 1975
sum(m7_9$residuals^2)
prd_78 = predict(m7_9, newdata=test)
test_err_s1975 = sum((prd_78-test$median_income)^2)
test_err_s1975


k = c(test_err_s20,test_err_s70,test_err_s150, test_err_s190,test_err_s390,
      test_err_s690,test_err_s1000, test_err_s1500,test_err_s1975)
n = c(20, 70,150,190,390,690,1000,1500,1975)

plot(n,k, type = "l",main = "Test Error vs Sample Size(For Order 7 Model)", 
     xlab = "Sample Size", ylab = "Test Error", cex = 0.5, col = 'red')


####################################Problem-2#############################################
##For sample size of 100 understanding model behavior as order of model keeps increasing#
##########################################################################################


#Fitting 1st order polynomial regression model  for sample of 100
set.seed(2)
r9 = sample(1:nrow(train), 100)
tr_79 = train[r9, ]
dim(tr_79)
View(train)


m_1 = lm(median_house_value ~ median_income, tr_79)
m_1

#plotting for order 1 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_1)[order(tr_79$median_income)], col='brown', type='l',pch=20)

#Fitting 2nd order polynomial regression model  for sample of 100
m_2 = lm(median_house_value ~ median_income + I(median_income^2), tr_79)
m_2

#plotting for order 2 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_2)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 3rd order polynomial regression model  for sample of 100
m_3 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3), tr_79)
m_3

#plotting for order 3 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_3)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 4th order polynomial regression model  for sample of 100
m_4 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4), tr_79)
m_4

#plotting for order 4 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_4)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 5th order polynomial regression model  for sample of 100
m_5 = lm(median_house_value ~ median_income + I(median_income^2) + 
      I(median_income^3) + I(median_income^4) + I(median_income^5), tr_79)
m_5

#plotting for order 5 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_5)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 6th order polynomial regression model  for sample of 100
m_6 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4) + I(median_income^5) +
           I(median_income^6), tr_79)
m_6

#plotting for order 6 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_6)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 7th order polynomial regression model  for sample of 100
m_7 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4) + I(median_income^5) +
           I(median_income^6) + I(median_income^7), tr_79)
m_7

#plotting for order 6 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_7)[order(tr_79$median_income)], col='brown', type='l',pch=20)



#Fitting 8th order polynomial regression model  for sample of 100
m_8 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4) + I(median_income^5) +
           I(median_income^6) + I(median_income^7) + I(median_income^8), tr_79)
m_8

#plotting for order 8 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_8)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 9th order polynomial regression model  for sample of 100
m_9 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4) + I(median_income^5) +
           I(median_income^6) + I(median_income^7) + I(median_income^8) +
           I(median_income^9), tr_79)
m_9

#plotting for order 9 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_9)[order(tr_79$median_income)], col='brown', type='l',pch=20)


#Fitting 10 order polynomial regression model  for sample of 100
m_10 = lm(median_house_value ~ median_income + I(median_income^2) + 
           I(median_income^3) + I(median_income^4) + I(median_income^5) +
           I(median_income^6) + I(median_income^7) + I(median_income^8) +
           I(median_income^9) + I(median_income^10), tr_79)
m_10

#plotting for order 10 polynomial model
plot(tr_79$median_income, tr_79$median_house_value, pch = 19, cex = 0.5,
     xlab = "Median Income", ylab = "Median house value", main = "Polynomial regression of different orders")
lines(sort(tr_79$median_income), fitted(m_10)[order(tr_79$median_income)], col='brown', type='l',pch=20)



#Plotting diffrenet order polynomial regression lines together
lines(sort(tr_79$median_income), fitted(m_1)[order(tr_79$median_income)], col='brown', type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_2)[order(tr_79$median_income)], col='red',   type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_3)[order(tr_79$median_income)], col='blue',   type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_4)[order(tr_79$median_income)], col='yellow', type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_5)[order(tr_79$median_income)], col='black',  type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_6)[order(tr_79$median_income)], col='orange', type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_7)[order(tr_79$median_income)], col='purple', type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_8)[order(tr_79$median_income)], col='voilet', type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_9)[order(tr_79$median_income)], col='magenta',type='l',pch=20)
lines(sort(tr_79$median_income), fitted(m_10)[order(tr_79$median_income)], col='green', type='l',pch=20)


####################################Problem-3#############################################
#For sample size of 140 changing the seed, fitting the model on different orders###
###################################################################################

#Buildind polynomial regression model for order 1 of seed(3)
set.seed(3)
r10 = sample(1:nrow(train), 140)
tr_80 = train[r10, ]
dim(tr_80)

model1 = lm(median_house_value ~ median_income, tr_80)
model1

#Predicting the model on test data
pd1 = predict(model1, newdata = test)
tst_rss1= sum((pd1-test$median_income)^2)
tst_rss1

#Calculating RMSE
rmse1 = sqrt(tst_rss1/50)
rmse1

#Buildind polynomial regression model for order 2 of seed(3)
model2 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
model2

#Predicting the model on test data
pd2 = predict(model2, newdata = test)
tst_rss2= sum((pd2-test$median_income)^2)
tst_rss2

#Calculating RMSE
rmse2 = sqrt(tst_rss2/50)
rmse2

#Buildind polynomial regression model for order 3 of seed(3)
model3 = lm(median_house_value ~ median_income + I(median_income^2) +I(median_income^3),tr_80)
model3

#Predicting the model on test data
pd3 = predict(model3, newdata = test)
tst_rss3= sum((pd3-test$median_income)^2)
tst_rss3
#Calculating RMSE
rmse3 = sqrt(tst_rss3/50)
rmse3

#Buildind polynomial regression model for order 7 of seed(3)
model4 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7), tr_80)
model4

#Predicting the model on test data
pd4 = predict(model4, newdata = test)
tst_rss4= sum((pd4-test$median_income)^2)
tst_rss4
#Calculating RMSE
rmse4 = sqrt(tst_rss4/50)
rmse4

#Buildind polynomial regression model for order 8 of seed(3)
model5 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model5

#Predicting the model on test data
pd5 = predict(model5, newdata = test)
tst_rss5= sum((pd5-test$median_income)^2)
tst_rss5

#Calculating RMSE
rmse5 = sqrt(tst_rss5/50)
rmse5

#Buildind polynomial regression model for order 9 of seed(3)
model6 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8)
            +I(median_income^9),  tr_80)
model6

#Predicting the model on test data
pd6 = predict(model6, newdata = test)
tst_rss6= sum((pd6-test$median_income)^2)
tst_rss6

#Calculating RMSE
rmse6 = sqrt(tst_rss6/50)
rmse6

#Buildind polynomial regression model for order 10 of seed(3)
model7 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8)
            +I(median_income^9) + I(median_income^10),  tr_80)
model7

#Predicting the model on test data
pd7 = predict(model7, newdata = test)
tst_rss7= sum((pd7-test$median_income)^2)
tst_rss7

#Calculating RMSE
rmse7 = sqrt(tst_rss7/50)
rmse7

k1 = c(rmse1,rmse2,rmse3,rmse4,rmse5,rmse6,rmse7)
k2 = c(1,2,3,7,8,9,10)

plot(k2, k1, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'blue')


#Buildind polynomial regression model for order 1 of seed(10)
set.seed(10)
r10 = sample(1:nrow(train), 140)
tr_80 = train[r10, ]
dim(tr_80)

model11 = lm(median_house_value ~ median_income, tr_80)
model11

#Predicting the model on test data
pd11 = predict(model11, newdata = test)
tst_rss11= sum((pd11-test$median_income)^2)
tst_rss11

#Calculating RMSE
rmse11 = sqrt(tst_rss11/50)
rmse11

#Buildind polynomial regression model for order 2 of seed(33)
model12 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
model12

#Predicting the model on test data
pd12 = predict(model12, newdata = test)
tst_rss12= sum((pd12-test$median_income)^2)
tst_rss12

#Calculating RMSE
rmse12 = sqrt(tst_rss12/50)
rmse12


#Buildind polynomial regression model for order 3 of seed(33)
model13 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3), tr_80)
model13

#Predicting the model on test data
pd13 = predict(model13, newdata = test)
tst_rss13= sum((pd13-test$median_income)^2)
tst_rss13

#Calculating RMSE
rmse13 = sqrt(tst_rss13/50)
rmse13

#Buildind polynomial regression model for order 7 of seed(33)
model14 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7), tr_80)
model14

#Predicting the model on test data
pd14 = predict(model14, newdata = test)
tst_rss14= sum((pd14-test$median_income)^2)
tst_rss14

#Calculating RMSE
rmse14 = sqrt(tst_rss14/50)
rmse14

#Buildind polynomial regression model for order 8 of seed(33)
model15 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model15

#Predicting the model on test data
pd15 = predict(model15, newdata = test)
tst_rss15= sum((pd15-test$median_income)^2)
tst_rss15

#Calculating RMSE
rmse15 = sqrt(tst_rss15/50)
rmse15

#Buildind polynomial regression model for order 9 of seed(33)
model16 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9), tr_80)
model16

#Predicting the model on test data
pd16 = predict(model16, newdata = test)
tst_rss16= sum((pd16-test$median_income)^2)
tst_rss16

#Calculating RMSE
rmse16 = sqrt(tst_rss16/50)
rmse16

#Buildind polynomial regression model for order 10 of seed(33)
model17 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9) + I(median_income^10), tr_80)
model17

#Predicting the model on test data
pd17 = predict(model17, newdata = test)
tst_rss17 = sum((pd17-test$median_income)^2)
tst_rss17

#Calculating RMSE
rmse17 = sqrt(tst_rss17/50)
rmse17

k3 = c(rmse11, rmse12, rmse13, rmse14, rmse15, rmse16, rmse17)
k4 = c(1,2,3,7,8,9,10)
plot(k4, k3, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'red')



#Buildind polynomial regression model for order 1 of seed(20)
set.seed(20)
r10 = sample(1:nrow(train), 140)
tr_80 = train[r10, ]
dim(tr_80)

model18 = lm(median_house_value ~ median_income, tr_80)
model18

#Predicting the model on test data
pd18 = predict(model18, newdata = test)
tst_rss18= sum((pd18-test$median_income)^2)
tst_rss18

#Calculating RMSE
rmse18 = sqrt(tst_rss18/50)
rmse18

#Buildind polynomial regression model for order 2 of seed(20)
model19 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
model19

#Predicting the model on test data
pd19 = predict(model19, newdata = test)
tst_rss19= sum((pd19-test$median_income)^2)
tst_rss19

#Calculating RMSE
rmse19 = sqrt(tst_rss19/50)
rmse19


#Buildind polynomial regression model for order 3 of seed(20)
model20 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3), tr_80)
model20

#Predicting the model on test data
pd20 = predict(model20, newdata = test)
tst_rss20= sum((pd20-test$median_income)^2)
tst_rss20

#Calculating RMSE
rmse20 = sqrt(tst_rss20/50)
rmse20

#Buildind polynomial regression model for order 7 of seed(20)
model21 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7), tr_80)
model21

#Predicting the model on test data
pd21 = predict(model21, newdata = test)
tst_rss21= sum((pd21-test$median_income)^2)
tst_rss21

#Calculating RMSE
rmse21 = sqrt(tst_rss21/50)
rmse21

#Buildind polynomial regression model for order 8 of seed(20)
model22 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model22

#Predicting the model on test data
pd22 = predict(model22, newdata = test)
tst_rss22= sum((pd22-test$median_income)^2)
tst_rss22

#Calculating RMSE
rmse22 = sqrt(tst_rss22/50)
rmse22

#Buildind polynomial regression model for order 9 of seed(20)
model23 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9), tr_80)
model23

#Predicting the model on test data
pd23 = predict(model23, newdata = test)
tst_rss23= sum((pd23-test$median_income)^2)
tst_rss23

#Calculating RMSE
rmse23 = sqrt(tst_rss23/50)
rmse23

#Buildind polynomial regression model for order 10 of seed(20)
model24 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9) + I(median_income^10), tr_80)
model24

#Predicting the model on test data
pd24 = predict(model24, newdata = test)
tst_rss24 = sum((pd24-test$median_income)^2)
tst_rss24

#Calculating RMSE
rmse24 = sqrt(tst_rss24/50)
rmse24

k5 = c(rmse18, rmse19, rmse20, rmse21, rmse22, rmse23, rmse24)
k6 = c(1,2,3,7,8,9,10)
plot(k6, k5, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'red')


#Buildind polynomial regression model for order 1 of seed(30)
set.seed(30)
r10 = sample(1:nrow(train), 140)
tr_80 = train[r10, ]
dim(tr_80)

model25 = lm(median_house_value ~ median_income, tr_80)
model25

#Predicting the model on test data
pd25 = predict(model25, newdata = test)
tst_rss25= sum((pd25-test$median_income)^2)
tst_rss25

#Calculating RMSE
rmse25 = sqrt(tst_rss25/50)
rmse25

#Buildind polynomial regression model for order 2 of seed(30)
model26 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
model26

#Predicting the model on test data
pd26 = predict(model26, newdata = test)
tst_rss26= sum((pd26-test$median_income)^2)
tst_rss26

#Calculating RMSE
rmse26 = sqrt(tst_rss26/50)
rmse26


#Buildind polynomial regression model for order 3 of seed(30)
model27 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3), tr_80)
model27

#Predicting the model on test data
pd27 = predict(model27, newdata = test)
tst_rss27= sum((pd27-test$median_income)^2)
tst_rss27

#Calculating RMSE
rmse27 = sqrt(tst_rss27/50)
rmse27

#Buildind polynomial regression model for order 7 of seed(30)
model28 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7), tr_80)
model28

#Predicting the model on test data
pd28 = predict(model28, newdata = test)
tst_rss28= sum((pd28-test$median_income)^2)
tst_rss28

#Calculating RMSE
rmse28 = sqrt(tst_rss28/50)
rmse28

#Buildind polynomial regression model for order 8 of seed(30)
model29 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model29

#Predicting the model on test data
pd29 = predict(model29, newdata = test)
tst_rss29= sum((pd29-test$median_income)^2)
tst_rss29

#Calculating RMSE
rmse29 = sqrt(tst_rss29/50)
rmse29

#Buildind polynomial regression model for order 9 of seed(30)
model30 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9), tr_80)
model30

#Predicting the model on test data
pd30 = predict(model30, newdata = test)
tst_rss30= sum((pd30-test$median_income)^2)
tst_rss30

#Calculating RMSE
rmse30 = sqrt(tst_rss30/50)
rmse30

#Buildind polynomial regression model for order 10 of seed(30)
model31 = lm(median_house_value ~ median_income + I(median_income^2) +
               I(median_income^3) + I(median_income^7) + I(median_income^8) +
               I(median_income^9) + I(median_income^10), tr_80)
model31

#Predicting the model on test data
pd31 = predict(model31, newdata = test)
tst_rss31 = sum((pd31-test$median_income)^2)
tst_rss31

#Calculating RMSE
rmse31 = sqrt(tst_rss31/50)
rmse31

k7 = c(rmse25, rmse26, rmse27, rmse28, rmse29, rmse30, rmse31)
k8 = c(1,2,3,7,8,9,10)
plot(k8, k7, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'orange')

#################################Problem-4###########################################
##For sample size of 400 changing the seed, fitting the model on different orders
####################################################################################

#Buildind polynomial regression model for order 1 of seed(55)
set.seed(55)
r10 = sample(1:nrow(train), 400)
tr_80 = train[r10, ]
dim(tr_80)

model32 = lm(median_house_value ~ median_income, tr_80)
model32

#Predicting the model on test data
pd32 = predict(model32, newdata = test)
tst_rss32= sum((pd32-test$median_income)^2)
tst_rss32

#Calculating RMSE
rmse32 = sqrt(tst_rss32/50)
rmse32

#Buildind polynomial regression model for order 2 of seed(55)
mode33 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
mode33

#Predicting the model on test data
pd33 = predict(mode33, newdata = test)
tst_rss33= sum((pd33-test$median_income)^2)
tst_rss33

#Calculating RMSE
rmse33 = sqrt(tst_rss33/50)
rmse33

#Buildind polynomial regression model for order 3 of seed(55)
model34 = lm(median_house_value ~ median_income + I(median_income^2) +I(median_income^3),tr_80)
model34

#Predicting the model on test data
pd34 = predict(model34, newdata = test)
tst_rss34= sum((pd34-test$median_income)^2)
tst_rss34
#Calculating RMSE
rmse34 = sqrt(tst_rss34/50)
rmse34

#Buildind polynomial regression model for order 7 of seed(55)
model35 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7), tr_80)
model35

#Predicting the model on test data
pd35 = predict(model35, newdata = test)
tst_rss35= sum((pd35-test$median_income)^2)
tst_rss35
#Calculating RMSE
rmse35 = sqrt(tst_rss35/50)
rmse35

#Buildind polynomial regression model for order 8 of seed(55)
model36 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model36

#Predicting the model on test data
pd36 = predict(model36, newdata = test)
tst_rss36= sum((pd36-test$median_income)^2)
tst_rss36

#Calculating RMSE
rmse36 = sqrt(tst_rss36/50)
rmse36

#Buildind polynomial regression model for order 9 of seed(55)
model37 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8)
            +I(median_income^9),  tr_80)
model37

#Predicting the model on test data
pd37 = predict(model37, newdata = test)
tst_rss37= sum((pd37-test$median_income)^2)
tst_rss37

#Calculating RMSE
rmse37 = sqrt(tst_rss37/50)
rmse37

#Buildind polynomial regression model for order 10 of seed(55)
model38 = lm(median_house_value ~ median_income + I(median_income^2)
            +I(median_income^3) + I(median_income^7) + I(median_income^8)
            +I(median_income^9) + I(median_income^10),  tr_80)
model38

#Predicting the model on test data
pd38 = predict(model38, newdata = test)
tst_rss38= sum((pd38-test$median_income)^2)
tst_rss38

#Calculating RMSE
rmse38 = sqrt(tst_rss38/50)
rmse38

k9 = c(rmse32,rmse33,rmse34,rmse35,rmse36,rmse37,rmse38)
k10 = c(1,2,3,7,8,9,10)

plot(k10, k9, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'blue')

#******************************Same sample size changing seed 70********************  

#Buildind polynomial regression model for order 1 of seed(70)
set.seed(70)
r10 = sample(1:nrow(train), 400)
tr_80 = train[r10, ]
dim(tr_80)

model32 = lm(median_house_value ~ median_income, tr_80)
model32

#Predicting the model on test data
pd32 = predict(model32, newdata = test)
tst_rss32= sum((pd32-test$median_income)^2)
tst_rss32

#Calculating RMSE
rmse32 = sqrt(tst_rss32/50)
rmse32

#Buildind polynomial regression model for order 2 of seed(55)
mode33 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
mode33

#Predicting the model on test data
pd33 = predict(mode33, newdata = test)
tst_rss33= sum((pd33-test$median_income)^2)
tst_rss33

#Calculating RMSE
rmse33 = sqrt(tst_rss33/50)
rmse33

#Buildind polynomial regression model for order 3 of seed(55)
model34 = lm(median_house_value ~ median_income + I(median_income^2) +I(median_income^3),tr_80)
model34

#Predicting the model on test data
pd34 = predict(model34, newdata = test)
tst_rss34= sum((pd34-test$median_income)^2)
tst_rss34
#Calculating RMSE
rmse34 = sqrt(tst_rss34/50)
rmse34

#Buildind polynomial regression model for order 7 of seed(55)
model35 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7), tr_80)
model35

#Predicting the model on test data
pd35 = predict(model35, newdata = test)
tst_rss35= sum((pd35-test$median_income)^2)
tst_rss35
#Calculating RMSE
rmse35 = sqrt(tst_rss35/50)
rmse35

#Buildind polynomial regression model for order 8 of seed(55)
model36 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model36

#Predicting the model on test data
pd36 = predict(model36, newdata = test)
tst_rss36= sum((pd36-test$median_income)^2)
tst_rss36

#Calculating RMSE
rmse36 = sqrt(tst_rss36/50)
rmse36

#Buildind polynomial regression model for order 9 of seed(55)
model37 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9),  tr_80)
model37

#Predicting the model on test data
pd37 = predict(model37, newdata = test)
tst_rss37= sum((pd37-test$median_income)^2)
tst_rss37

#Calculating RMSE
rmse37 = sqrt(tst_rss37/50)
rmse37

#Buildind polynomial regression model for order 10 of seed(55)
model38 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9) + I(median_income^10),  tr_80)
model38

#Predicting the model on test data
pd38 = predict(model38, newdata = test)
tst_rss38= sum((pd38-test$median_income)^2)
tst_rss38

#Calculating RMSE
rmse38 = sqrt(tst_rss38/50)
rmse38

k11 = c(rmse32,rmse33,rmse34,rmse35,rmse36,rmse37,rmse38)
k12 = c(1,2,3,7,8,9,10)

plot(k12, k11, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'blue')


#******************************Same sample size changing seed 88************************  

#Buildind polynomial regression model for order 1 of seed(88)
set.seed(88)
r10 = sample(1:nrow(train), 400)
tr_80 = train[r10, ]
dim(tr_80)

model32 = lm(median_house_value ~ median_income, tr_80)
model32

#Predicting the model on test data
pd32 = predict(model32, newdata = test)
tst_rss32= sum((pd32-test$median_income)^2)
tst_rss32

#Calculating RMSE
rmse32 = sqrt(tst_rss32/50)
rmse32

#Buildind polynomial regression model for order 2 of seed(55)
mode33 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
mode33

#Predicting the model on test data
pd33 = predict(mode33, newdata = test)
tst_rss33= sum((pd33-test$median_income)^2)
tst_rss33

#Calculating RMSE
rmse33 = sqrt(tst_rss33/50)
rmse33

#Buildind polynomial regression model for order 3 of seed(55)
model34 = lm(median_house_value ~ median_income + I(median_income^2) +I(median_income^3),tr_80)
model34

#Predicting the model on test data
pd34 = predict(model34, newdata = test)
tst_rss34= sum((pd34-test$median_income)^2)
tst_rss34
#Calculating RMSE
rmse34 = sqrt(tst_rss34/50)
rmse34

#Buildind polynomial regression model for order 7 of seed(55)
model35 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7), tr_80)
model35

#Predicting the model on test data
pd35 = predict(model35, newdata = test)
tst_rss35= sum((pd35-test$median_income)^2)
tst_rss35
#Calculating RMSE
rmse35 = sqrt(tst_rss35/50)
rmse35

#Buildind polynomial regression model for order 8 of seed(55)
model36 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model36

#Predicting the model on test data
pd36 = predict(model36, newdata = test)
tst_rss36= sum((pd36-test$median_income)^2)
tst_rss36

#Calculating RMSE
rmse36 = sqrt(tst_rss36/50)
rmse36

#Buildind polynomial regression model for order 9 of seed(55)
model37 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9),  tr_80)
model37

#Predicting the model on test data
pd37 = predict(model37, newdata = test)
tst_rss37= sum((pd37-test$median_income)^2)
tst_rss37

#Calculating RMSE
rmse37 = sqrt(tst_rss37/50)
rmse37

#Buildind polynomial regression model for order 10 of seed(55)
model38 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9) + I(median_income^10),  tr_80)
model38

#Predicting the model on test data
pd38 = predict(model38, newdata = test)
tst_rss38= sum((pd38-test$median_income)^2)
tst_rss38

#Calculating RMSE
rmse38 = sqrt(tst_rss38/50)
rmse38

k13 = c(rmse32,rmse33,rmse34,rmse35,rmse36,rmse37,rmse38)
k14 = c(1,2,3,7,8,9,10)

plot(k14, k13, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'red')


#******************************Same sample size changing seed 183************************  

#Buildind polynomial regression model for order 1 of seed(183)
set.seed(183)
r10 = sample(1:nrow(train), 400)
tr_80 = train[r10, ]
dim(tr_80)

model32 = lm(median_house_value ~ median_income, tr_80)
model32

#train rss
train_err1 = sum(model32$residuals^2)
train_err1
train_rmse1 = sqrt(train_err1/400)
train_rmse1
#Predicting the model on test data
pd32 = predict(model32, newdata = test)
tst_rss32= sum((pd32-test$median_income)^2)
tst_rss32

#Calculating RMSE
rmse32 = sqrt(tst_rss32/50)
rmse32

#Buildind polynomial regression model for order 2 of seed(55)
mode33 = lm(median_house_value ~ median_income + I(median_income^2), tr_80)
mode33

#train rss
train_err2 = sum(mode33$residuals^2)
train_rmse2 = sqrt(train_err2/400)
train_rmse2

#Predicting the model on test data
pd33 = predict(mode33, newdata = test)
tst_rss33= sum((pd33-test$median_income)^2)
tst_rss33

#Calculating RMSE
rmse33 = sqrt(tst_rss33/50)
rmse33

#Buildind polynomial regression model for order 3 of seed(55)
model34 = lm(median_house_value ~ median_income + I(median_income^2) +I(median_income^3),tr_80)
model34

#train rss
train_err3 = sum(model34$residuals^2)
train_rmse3 = sqrt(train_err3/400)
train_rmse3

#Predicting the model on test data
pd34 = predict(model34, newdata = test)
tst_rss34= sum((pd34-test$median_income)^2)
tst_rss34
#Calculating RMSE
rmse34 = sqrt(tst_rss34/50)
rmse34

#Buildind polynomial regression model for order 7 of seed(55)
model35 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7), tr_80)
model35


#train rss
train_err4 = sum(model35$residuals^2)
train_rmse4 = sqrt(train_err4/400)
train_rmse4

#Predicting the model on test data
pd35 = predict(model35, newdata = test)
tst_rss35= sum((pd35-test$median_income)^2)
tst_rss35

#Calculating RMSE
rmse35 = sqrt(tst_rss35/50)
rmse35

#Buildind polynomial regression model for order 8 of seed(55)
model36 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8), tr_80)
model36


#train rss
train_err5 = sum(model36$residuals^2)
train_rmse5 = sqrt(train_err5/400)
train_rmse5


#Predicting the model on test data
pd36 = predict(model36, newdata = test)
tst_rss36= sum((pd36-test$median_income)^2)
tst_rss36

#Calculating RMSE
rmse36 = sqrt(tst_rss36/50)
rmse36

#Buildind polynomial regression model for order 9 of seed(55)
model37 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9),  tr_80)
model37


#train rss
train_err6 = sum(model37$residuals^2)
train_rmse6 = sqrt(train_err6/400)
train_rmse6
#Predicting the model on test data
pd37 = predict(model37, newdata = test)
tst_rss37= sum((pd37-test$median_income)^2)
tst_rss37

#Calculating RMSE
rmse37 = sqrt(tst_rss37/50)
rmse37

#Buildind polynomial regression model for order 10 of seed(55)
model38 = lm(median_house_value ~ median_income + I(median_income^2)
             +I(median_income^3) + I(median_income^7) + I(median_income^8)
             +I(median_income^9) + I(median_income^10),  tr_80)
model38


#train rss
train_err7 = sum(model38$residuals^2)
train_rmse7 = sqrt(train_err7/400)
train_rmse7
#Predicting the model on test data
pd38 = predict(model38, newdata = test)
tst_rss38= sum((pd38-test$median_income)^2)
tst_rss38

#Calculating RMSE
rmse38 = sqrt(tst_rss38/50)
rmse38


k15 = c(rmse32,rmse33,rmse34,rmse35,rmse36,rmse37,rmse38)
k16 = c(1,2,3,7,8,9,10)

plot(k16, k15, type = 'l', xlab = "Complexity", ylab = "Test Rmse", 
     main = "Test Rmse vs Complexity", col = 'red')

#######################################Problem - 5#################################
#Plotting test error vs train error for above results                          ####
##################################################################################

k17 = c(train_rmse1,train_rmse2,train_rmse3,train_rmse4,train_rmse5,train_rmse6,
        train_rmse7)
k18 = c(rmse32,rmse33,rmse34,rmse35,rmse36,rmse37,rmse38)
k19 = c(1,2,3,7,8,9,10)

plot(k19, k17, type = 'l', xlab = "Complexity", ylab = "RMSE",
     main = "Complexity vs RMSE(Train,Test)" , col = 'red')
par(new=TRUE)
plot(k19, k18, type = 'l',xlab = "Complexity", ylab = "", axes=FALSE, col = 'blue')

legend("bottomright", legend = c("Train RMSE", "Test RMSE"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8, box.lty = 2)

