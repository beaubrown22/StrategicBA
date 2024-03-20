

##### week 12 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

#load the packages we need
library(broom)

################## interactions ##################

# load the interaction data
demand_interaction = read.csv("data_interaction.csv", header = T,
                              stringsAsFactors = T, sep = ",")

# look at the summary of data
summary(demand_interaction)

# run basic regression without interactions
demand_reg = lm(demand ~ price, data = demand_interaction)
summary(demand_reg)

# run regression taking into account the day of week effect
demand_reg_dofw = lm(demand ~ price + factor(dayOfWeek), data = demand_interaction)
summary(demand_reg_dofw)

# run regression taking into account the day of week effect AND income effect
demand_reg_interact = lm(demand ~ price*income + factor(dayOfWeek), data = demand_interaction)
summary(demand_reg_interact)

###############################################

# load the ca school data
ca_school = read.csv("data_caschool.csv", header = T, sep = ",",
                     stringsAsFactors = T)

# observe the data
str(ca_school)

# plot the raw data
plot(testscr ~ income, data = ca_school)

# run regression model 
output_reg = lm(testscr ~ income, data = ca_school)
summary(output_reg)

# create quadratic income variable
ca_school$income2 = ca_school$income^2

# create cubic income variable
ca_school$income3 = ca_school$income^3

# run regression with quadratic term
output_reg2 = lm(testscr ~ income + income2,
                 data = ca_school)
summary(output_reg2)

# calculate the marginal effect of increasing income
# income increase from 10 to 11
# first calculate predict_yhat when income=10
pred_data1 = data.frame(income = 10, income2 = 10^2)
yhat1 = predict(output_reg2, pred_data1)

# second calculate predict_yhat when income=11
pred_data2 = data.frame(income = 11, income2 = 11^2)
yhat2 = predict(output_reg2, pred_data2)

# now get the difference between the two
yhat2 - yhat1

# calculate the marginal effect directly
# formula: deltay = beta1_hat + 2*beta2_hat*income
# extract coefficient estimates from regression
neat_output = tidy(output_reg2)
neat_output

beta1_hat = neat_output$estimate[2]
beta2_hat = neat_output$estimate[3]

# set initial income level and use the formula
x_income = 10
marginal_y = beta1_hat + 2*beta2_hat*x_income
marginal_y

###############################################
# linear-log model
# read the data and observe the data
sales_ad = read.csv("data_salesad.csv", header = T, sep = ",",
                    stringsAsFactors = T)

str(sales_ad)

# plot the raw data points
plot(sales ~ advert, data = sales_ad)

# regular regression
reg_linear = lm(sales ~ advert, data = sales_ad)
summary(reg_linear)

# create log(ad)
# first check to make sure advert is positive
summary(sales_ad$advert) 
sales_ad$lnad = log(sales_ad$advert)

# run linear-log regression
reg_linearlog = lm(sales ~ lnad, data = sales_ad)
summary(reg_linearlog)


# calculate the exact value impact on sales
# first predict sale when ad=200
pre_sale1 = data.frame(lnad = log(200))
yhat1_ad = predict(reg_linearlog, pre_sale1)

# second predict sale when ad=201
pre_sale2 = data.frame(lnad = log(201))
yhat2_ad = predict(reg_linearlog, pre_sale2)

# now calculate the difference between the two
exact_value = yhat2_ad - yhat1_ad

# use approximation to calculate
# extract coefficient estimate from regression output
neat_linearlog = tidy(reg_linearlog)
lnb1 = neat_linearlog$estimate[2]

# calculate the approximation
appr_ad = 0.005*lnb1*1000


###############################################
# log-linear model
# create log(sale), first check if sale is positive only
summary(sales_ad$sales)
sales_ad$lnsale = log(sales_ad$sales)

# run log-linear regression
reg_loglinear = lm(lnsale ~ advert, data = sales_ad)
summary(reg_loglinear)

# evaluate the impact at a baseline of sales (get the mean of sales)
avg_sale = mean(sales_ad$sales)

# log-log model
reg_loglog = lm(lnsale ~ lnad, data = sales_ad)
summary(reg_loglog)

# plot the result
# plot 4 graphs together for comparison
# specify number of rows and number of columns
par(mfrow = c(2,2))  

# regular linear model
plot(sales ~ advert, data = sales_ad)
abline(reg_linear, col = "red")

# linear-log model
plot(sales ~ lnad, data = sales_ad,
     main = "linear-log")
abline(reg_linearlog, col = "red")

# log-linear model
plot(lnsale ~ advert, data = sales_ad,
     main = "log-linear")
abline(reg_loglinear, col = "red")

# log-log model
plot(lnsale ~ lnad, data = sales_ad,
     main = "log-log")
abline(reg_loglog, col = 'red')

# reset the plotting device back to normal
par(mfrow = c(1,1))

