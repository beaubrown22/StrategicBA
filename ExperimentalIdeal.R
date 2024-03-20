

##### Experimental Ideal #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")              # set work directory; change the path inside "your computer/your folder" of your own path
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in


# load the ebay data
ebay = read.csv("data_ebay.csv", header = T, sep = ",",
                stringsAsFactors = T)

# check the imported dataset
str(ebay)
summary(ebay)

### variables in the data
# date: Date of advertising
# dma: designated market area code, basically a city
# isTreatmentPeriod: dummy variable denoting whether date belonged to the treatment period
# isTreatmentGroup: dummy variable denoting whether dma belonged to the treatment group
# revenue: revenue for the dma in dollars

# we usually use log(revenue) to measure the sales, especially when the revenue numbers are large.
# create variable of log revenue
ebay$log_rev = log(ebay$revenue)

# obtain a subset of the treatment group and control group
treat_group = subset(ebay, ebay$isTreatmentGroup == 1)
control_group = subset(ebay, ebay$isTreatmentGroup != 1)

# double check if all the observations are split into either control or treatment group
nrow(treat_group)+nrow(control_group)      # obtain number of rows
nrow(ebay)


# randomization check: this is one of the key features of experiments
# randomization check method 1: use t.test to compare the means of two samples
# we check the mean of log(revenue) BEFORE the treatment date for both groups
# obtain the subset of both groups with observations BEFORE treatment date
pre_treat_group = subset(treat_group, treat_group$isTreatmentPeriod != 1)
pre_control_group = subset(control_group, control_group$isTreatmentPeriod != 1)
t.test(pre_treat_group$log_rev, pre_control_group$log_rev)
# p-value is larger than 0.05, so we cannot reject the null, which is what we want for randomization check.

# randomization check method 2: run regression on data set of PRE-treatment periods
preTreatment = subset(ebay, ebay$isTreatmentPeriod != 1)          # note here we use treatment and control groups together
treatment_reg = lm(log_rev ~ isTreatmentGroup, data = preTreatment)
summary(treatment_reg)
# the estimate coefficient on isTreatmentGroup is not statistically significant.

