
##### week 5 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 

#install.packages("arules")
#install.packages("arulesViz")

library(arules)
library(arulesViz)
library(ggplot2)

########################### Prepare Data Set ###########################
# create a list object of grocery shopping baskets from in-class example
shopping_basket =  
  list(  
    c("bread", "peanut butter", "milk", "fruit", "jelly"),
    c("bread", "jelly", "soda", "potato chips", "milk", "fruit", "vegetables", "peanut butter"),
    c("whipped cream", "fruit", "chocolate sauce", "beer"),
    c("steak", "jelly", "soda", "potato chips", "bread", "fruit"),
    c("jelly", "soda", "peanut butter", "milk", "fruit"),
    c("jelly", "soda", "potato chips", "milk", "bread", "fruit"),
    c("fruit", "soda", "potato chips", "milk"),
    c("fruit", "soda", "peanut butter", "milk"),
    c("fruit", "cheese", "yogurt"),
    c("yogurt", "vegetables", "beer")
  )

# set transaction names for each element
names(shopping_basket) = paste("Transaction", c(1:10), sep = "")

# save the R object as a RData file so that we can reuse it later
save(shopping_basket, file = "data_shopping_basket.RData")

# empty the environment and then load the shopping_basket RData file
rm(list = ls())
load("data_shopping_basket.RData")
class(shopping_basket)

########################################################################

# create a "transactions" type object
trans = as(shopping_basket, "transactions")
class(shopping_basket)
class(trans)

# inspect data (trans is in sparse format)
dim(trans)
trans

# obtain a list of distinct items in the data
itemLabels(trans)

# get the summary of the transaction data
summary(trans)

# summary() tells us how many transactions (rows) and items (columns) in this transaction data
# density tells us the percentage of non-zero cells in this 10x14-matrix.
# element length distribution: a set of 3 items appear in 2 transactions; 
#                              a set of 4 items appear in 3 transactions; 
#                              a set of 8 items appear in 1 transactions; 

# visualize the sparse matrix (grey blocks are non-zero)
image(trans)     # a nice visualization for yourself. No need to report this plot

# display relative item frequency of top 5, top 10 items
# y-axis = support of the item
# item milk has a support of 0.6
itemFrequencyPlot(trans, topN = 5)    # top 5 items
itemFrequencyPlot(trans, topN = 10)   # top 10 items

# use cex.name to control label sizes (default=1)
itemFrequencyPlot(trans, topN = 10, cex.names = 0.7)

# save the plot
jpeg(file = "item_frequency.jpeg", height = 1000, width = 1000)
itemFrequencyPlot(trans, topN = 10,  cex.names = 0.7)
dev.off()


### Analyze the rules using A-Priori Algorithm
# need to set 2 constraints:
# (1) you want to set a threshold for frequent items
#     minimum support: supp = 
# (2) minimum confidence: conf = 

# set minimum support = 0.5, confidence = 0.7, maximum length of items = 8
# maxlen = how many items you want to allow in your rules (including lhs + rhs)
# default maxlen = 10, if we specify maxlen = 3, then only patterns up to length 3 are mined.
rules = apriori(trans, 
                 parameter = list(supp = 0.5, conf = 0.7, 
                                  maxlen = 8, 
                                  target = "rules"))
summary(rules)

# if use default values of constraints
# default supp = 0.1, conf = 0.8, maxlen = 10
rules_default = apriori(trans)
summary(rules_default)         
# we have 809 rules! many of them include infrequent items, which are not very useful


# inspect the rules
inspect(rules)

# if does not want {} for lhs, specify minlen = 2 so there are at least 2 items in the rules
rules2 = apriori(trans, 
                parameter = list(supp = 0.5, conf = 0.7, 
                                 minlen = 2,
                                 maxlen = 8, 
                                 target = "rules"))

inspect(rules2)


# inspect only a few rules
inspect(rules[1:5])     # only inspect first 5 rules

# sort the rules so that the most relevant rules appear first
rules_sort = sort(rules, by = "confidence", decreasing = T)
inspect(rules_sort)


### targeting items: 
# what items do customers buy before buying {fruit}
# then set rhs = "fruit", see what lhs items are
fruit_rules_rhs = apriori(trans,
                          parameter = list(supp = 0.5, conf = 0.7,
                                           maxlen = 8,
                                           minlen = 2,
                                           target = "rules"),
                          appearance = list(default = "lhs", rhs = "fruit"))

# sort the rules, then inspect
fruit_rules_rhs = sort(fruit_rules_rhs, by = "confidence", decreasing = T)
inspect(fruit_rules_rhs)

# what items do customers buy after buying "milk"
# then set lhs = "milk", see what rhs items are
milk_rules_lhs = apriori(trans,
                     parameter = list(supp = 0.5, conf = 0.7,
                                      maxlen = 8,
                                      minlen = 2),
                     appearance = list(lhs = "milk", default = "rhs"))

# sort the rules, then inspect
milk_rules_lhs = sort(milk_rules_lhs, by = "confidence", decreasing = T)
inspect(milk_rules_lhs)


### visualize association rules
# scatter plot for the rules and save the plot
plot(rules) + theme_classic()             # the apriori plot() function uses ggplot
ggsave("apriori_scatter.jpeg", scale = 3)

# graph-based visualization (gives interactive html)
plot(rules, method = "graph", engine = "htmlwidget")
rm(list=ls())
###################### use external csv file data set ########################
### read a csv file for association rule mining
grocery = read.csv("data_groceries.csv", header = F, 
                   sep=",", na = "",         # specity na option so that empty cells are treated as NA
                   stringsAsFactors = T)  # specify the option stringAsFactors so that string variables are read as factors in R
# clean the grocery data set to be a list object so that R can generate a "transaction" type object
# create a purchase variable that obtains all items purchased for a customer
grocery$purchase = paste(grocery$V1, grocery$V2, grocery$V3, grocery$V4, sep = ",")

# create a cleaned_purchase variable that does not contain extra "," or "NA"
grocery$clean_purchase = gsub(",NA", "", grocery$purchase)    # replace characters ",NA" with "" 

# create an empty list object
glist = list()

# get number of observations (rows) of grocery data set
nobs = nrow(grocery)

# put every row of cleaned purchase itemset into 1 element of the list, loop over every row of the grocery data set
for(i in 1:nobs){
  glist[[i]] = grocery$clean_purchase[i]
}

# look at the element of the list, check to make sure the data is cleaned correctly
glist[[1]]   # should corresponding to grocery data 1st row
glist[[2]]   # should corresponding to grocery data 2nd row


# now we can create a "transactions" object from this cleaned list object needed for a-priori algorithm
gtrans = as(glist, "transactions")
class(gtrans)
class(glist)
class(grocery)

# inspect data (trans is in sparse format)
dim(gtrans)
summary(gtrans)
str(grocery)

# repeat code from line 83 for analysis.
### Analyze the rules using A-Priori Algorithm
# need to set 2 constraints:
# (1) you want to set a threshold for frequent items
#     minimum support: supp = 
# (2) minimum confidence: conf = 

# set minimum support = 0.5, confidence = 0.7, maximum length of items = 8
# maxlen = how many items you want to allow in your rules (including lhs + rhs)
# default maxlen = 10, if we specify maxlen = 3, then only patterns up to length 3 are mined.

rules = apriori(gtrans, 
                parameter = list(supp = 0.5, conf = 0.7, 
                                 maxlen = 8, 
                                 target = "rules"))
rules_default = apriori(gtrans)
summary(rules_default)    

inspect(rules)
summary(rules)
