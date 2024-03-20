
##### week 1 in-class exercise #####
# empty the environment
rm(list=ls())                                   

# set work directory
setwd("your computer/your folder")              # change the path inside "your computer/your folder" of your own path
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in


### build a data frame
# build the price column
price = c(1.01,0.99,0.92,1.08)                          

# build the adv column
adv = c(TRUE, FALSE, TRUE, TRUE)

# build the sales column
sales = c(1500,1600,1200,2000)

# build data frame
regionalSales = data.frame(price,adv,sales)     

# now add row names
rownames(regionalSales) = c("north","east","south","west")    

# look at the data frame in Console
regionalSales     

# change var name
names(regionalSales)=c("Price", "Adv", "Sales")   # use "" for strings

# only change the 2nd name
names(regionalSales)[2] = "advertising"           # R index starts with 1    

### check the built data frame
str(regionalSales)

### Indexing
# cell row 2 (east) col 3 (sales)
regionalSales[2,3]                                # use [] for index   

# referring by names
regionalSales["east","Sales"]            

# any row, column 1
regionalSales[,1]                        

# get column "Price"
regionalSales$Price                               # use $ to refer to variable name        


### Indexing with conditions
# get rows such that sales>1250
regionalSales[regionalSales$Sales > 1250, ]    

# get rows such that advertising=True AND sales>1250
regionalSales[regionalSales$advertising==TRUE & regionalSales$Sales>1250,]

# get rows such that advertising=True AND sales>1250 AND first and second columns only
regionalSales[regionalSales$advertising==TRUE & regionalSales$Sales >1250, c(1,2)]







