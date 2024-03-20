
##### week 2 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")              # set work directory; change the path inside "your computer/your folder" of your own path
#Alternatively (strongly recommended), use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in


##### Import Data #####
# download the data sets from canvas and put them in your working directory
titanic = read.table("data_titanic.csv", header=TRUE, sep=",")      
diamonds = read.table("data_diamonds.csv", header=TRUE, sep=",")

# check data set imported
str(titanic)
str(diamonds)

### categorical data set titanic
# look at first 6 rows of data
head(titanic)             # default is 6 rows

# look at first 10 rows of data
head(titanic, 10)

# look at last 6 rows of data
tail(titanic)      

# look at summary of data
summary(titanic)     

# construct frequency distribution of survivals by gender
table_survival = table(titanic$gender,titanic$survival)

# look at the table in Console window
table_survival                

# display cells as proportion of rows; 1=rows; 2=columns
# proportion adds up to 1 for each row
prop.table(table_survival,1)  

# proportion adds up to 1 for each column
prop.table(table_survival,2)  


### numeric data set diamonds
# look at first 6 rows of data
head(diamonds)                

# look at last 6 rows of data
tail(diamonds)                              

# look at summary of data
summary(diamonds)                           

# calculate mean of price column in diamonds data set
mean_price = mean(diamonds$price)     


# calculate the median of diamond price
median_price = median(diamonds$price)      


# calculate standard deviation of diamond price
sd_price = sd(diamonds$price)    

# calculate the z-score for diamond price
zscore_price = (diamonds$price - mean_price)/sd_price

# look at the summary statistics of z-score for diamond prices
summary(zscore_price)

# get a sub-dataset whose diamond price is within 1 standard deviation [mean_price-sd, mean_price+sd]
min_price1 = mean_price - sd_price
max_price1 = mean_price + sd_price
diamons_sub1 = diamonds[diamonds$price>=min_price1 & diamonds$price<=max_price1,]
257/380      

# get a sub-dataset whose diamond price is within 2 standard deviation [mean_price-2*sd, mean_price+2*sd]
min_price2 = mean_price - 2*sd_price
max_price2 = mean_price + 2*sd_price
diamons_sub2 = diamonds[diamonds$price>=min_price2 & diamonds$price<=max_price2,]
368/380

# get a sub-dataset whose diamond price is within 3 standard deviation [mean_price-3*sd, mean_price+3*sd]
min_price3 = mean_price - 3*sd_price
max_price3 = mean_price + 3*sd_price
diamons_sub3 = diamonds[diamonds$price>=min_price3 & diamonds$price<=max_price3,]
379/380

# boxplot of diamond price
boxplot(diamonds$price, data = diamonds) 

# boxplot of diamond prices by 3 groups of different sizes
boxplot(diamonds$price ~ diamonds$size, data = diamonds)     

# histogram of diamond price
hist(diamonds$price)                        

# histogram of diamond carats
hist(diamonds$carats)                       
# to save the plot, in the "Plots" window, click "Export" then choose from "Save as" or "Copy to clipboard"

# compute correlation matrix
cor(diamonds)                               

# only compute the correlation of 2nd and 3rd columns
cor(diamonds[,2:3])                         

# compute the correlation between carats and price
cor(diamonds$carats,diamonds$price)         

# scatter plot to see the relationship between carats and price
plot(diamonds$carats, diamonds$price)       



