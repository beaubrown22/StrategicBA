

##### week 11 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

#load the packages we need
library(broom)

# load the store sales and size data
store_sales = read.csv("data_sale_size.csv", header = T, sep = ",",
                       stringsAsFactors = T)

# observe the data set
str(store_sales)

# run regression model of store sales(y) on store size(x)
output_reg = lm(sales~size, data = store_sales)

# look at the regression output
summary(output_reg)

# obtain a neat version of the regression output
neat_output = tidy(output_reg)

# obtain estimate coefficient and standard errors
betahat = neat_output$estimate       
bse = neat_output$std.error         

# obtain summary statistics (such as R-square) from the regression output
s_out = glance(output_reg)
s_out$r.squared

# calculate 95% CI using t-distribution
confint(output_reg, level = 0.95)

# calculate 95% CI using normal distribution (more common in practice and when nobs is large)
bhat1 = betahat[2]
bse1 = bse[2]
uci = bhat1 + 1.96*bse1
lci = bhat1 - 1.96*bse1
uci
lci

################# Hypothesis Testing ###################

###### two-sided hypothesis testing
# get beta hats (the coefficient estimates)
bhat = neat_output$estimate
bhat1 = bhat[2]      # the first element in bhat is the estimate for the intercept: bhat0

# get standard error of the coefficient estimate
bse = neat_output$std.error
bse1 = bse[2]

# set the beta value under null hypothesis: the number 200 is researcher's own choice
b_null = 200

# calculate the t-statistic using formula
t_stat = (bhat1 - b_null)/bse1
t_stat  
  
# calculate the p-value according to the formula
# here we have a two-sided test
pvalue_2side = 2*pnorm(-t_stat)
pvalue_2side  
# then we compare the pvalue with 0.01 or 0.05, we will reject the null with a small pvalue  


##### one-sided hypothesis testing
# t_stat is calculated the same
# now the alternative hypothesis H1 is beta1 > 200
pvalue_larg = 1 - pnorm(t_stat)
pvalue_larg
# then we compare the pvalue with 0.01 or 0.05, we will reject the null with a small pvalue 


# one-sided hypothesis testing with alternative H1 is beta1 < 200
pvalue_smal = pnorm(t_stat)
pvalue_smal
# then we compare the pvalue with 0.01 or 0.05, we will reject the null with a small pvalue 


##### two-sided hypothesis testing
# now the beta value under null hypothesis is 0:
b_null_sig = 0
t_stat_sig = (bhat1 - b_null_sig)/bse1
pvalue_sig = 2*pnorm(-t_stat_sig)
pvalue_sig
# then we compare the pvalue with 0.01 or 0.05, we will reject the null with a small pvalue 


######################### another example #########################
# read the house price data set
hprice = read.csv("data_house_price.csv", header = T,
                  stringsAsFactors = T, sep = ",")

# observe the data set
str(hprice)

# run regression model of house price(y) on house size(x)
price_reg = lm(price~sqrft, data = hprice)

# look at the regression output
summary(price_reg)
glance(price_reg)
tidy(price_reg)

# obtain coefficient estimates and standard errors
neat_hprice = tidy(price_reg)
bhat_p = neat_hprice$estimate
bse_p = neat_hprice$std.error

# calculate 95% CI for the coefficient estimate using normal distribution
hp_bhat1 = bhat_p[2]
hp_bse1 = bse_p[2]
hp_uci = hp_bhat1 + 1.96*hp_bse1
hp_lci = hp_bhat1 - 1.96*hp_bse1
hp_uci
hp_lci        # check whether zero is inside the CI

# hypothesis testing: if we want to test if beta_1 = 0.1 or not
bp_null = 0.1
t = (hp_bhat1 - bp_null)/hp_bse1
bp_pvalue = 2*pnorm(-t)
bp_pvalue     # compare pvalue with 0.01 or 0.05 and decide whether to reject the null hypothesis


