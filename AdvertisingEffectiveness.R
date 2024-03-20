

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in


# load the store sales and size data
store_sales = read.csv("data_sale_size.csv", header = T, sep = ",", stringsAsFactors = T)

# observe the data set
str(store_sales)

# summary of the data
summary(store_sales)

# make a scatter plot to see the relationship between store sales and store size
# specify blue dots (you can choose the color you want)
plot(store_sales$size, store_sales$sales, col = "blue")

# run regression model of store sales(y) on store size(x)
output_reg = lm(sales ~ size, data = store_sales)

# look at the regression output
summary(output_reg)

# get the estimated coefficients
output_reg$coefficients

# now add the regression line to the previous scatter plot
plot(store_sales$size, store_sales$sales, col = "blue")
abline(output_reg, col = "red")

# save the plot
jpeg("store_size_reg.jpeg")
plot(store_sales$size, store_sales$sales, col = "blue")
     abline(output_reg, col = "red")
     dev.off()

# make prediction with the regression output
# what is the expected store sales if the store size is 50?
new_size = data.frame(size = 50)
predict(output_reg, newdata = new_size)

# $31,392,340

# construct confidence intervals at 95% level
confint(output_reg, level = 0.95)

# construct confidence intervals at 99% level
confint(output_reg, level = 0.99)

# find confidence interval for predicted store sales if the store size is 50
predict(output_reg, newdata = new_size, interval = "confidence") #default CI 95%

predict(output_reg, newdata = new_size, interval = "confidence", level = 0.99)

########## ebay Ads Experiments Revisit ##########
# load ebay data
ebay = read.csv("data_ebay.csv", header = T, sep = ",", stringsAsFactors = T)
     
# create log(revenue)
ebay$log_rev = log(ebay$revenue)

# treatment is after a certain date, the DMAs in the treatment group were no longer shown search ads from eBay.
# is there any revenue change after the ads were turned off?
# revenue change for treatment group and for control group
# obtain a subset of the treatment group and control group

treat_group = subset(ebay, ebay$isTreatmentGroup==1)
control_group = subset(ebay, ebay$isTreatmentGroup!=1)

# obtain the revenue change before and after the treatment: run the regression

treat_group_reg = lm(log_rev ~ isTreatmentPeriod, data = treat_group)

control_group_reg = lm(log_rev ~ isTreatmentPeriod, data = control_group)

# look at the regression results

summary(treat_group_reg)

summary(control_group_reg)

# is turning off Ads from ebay effective?
# obtain subset of post-treatment data

postTreatment = subset(ebay, ebay$isTreatmentPeriod == 1)

# run the regression to estimate the ad effectiveness

ad_effect_reg = lm(log_rev ~ isTreatmentGroup, data = postTreatment)

summary(ad_effect_reg)