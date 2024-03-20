
##### week 3 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")              # set work directory; change the path inside "your computer/your folder" of your own path
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in

install.packages("ggplot2")                    # install the package (only need to do this for the first time)


# load library that we will use
library(ggplot2)

# read data set
salary = read.table("data_salary.csv", header=TRUE, sep=",", stringsAsFactors = T)     # specify the option stringAsFactors so that string variables are read as factors in R

# check the imported dataset
str(salary)

# look at first few rows of data
head(salary)

# look at the summary of data
summary(salary)

# look at the summary of a particular variable in the data
summary(salary$age)

# calculate the mean and standard deviation
mean(salary$age)
sd(salary$age)

# check if there is NA in the data for age variable
is.na(salary$age)                   # returns a long list of logical T or F, not very easy to read        
sum(is.na(salary$age))              # add sum() and obtain a number. The number returned is the number of missing values

# if there is NA in the data, create a subset that contains such missing value obs
missing_age = salary[is.na(salary$age)==TRUE, ]      # this subset has 0 obs since there is no missing value in variable age


### work with plots
# check unique values of education
unique(salary$education)

# bar plot for #works counted by their education
p_edu = ggplot(data = salary) + geom_bar(mapping = aes(x = education))       
p_edu


# flip the coordinates for better reading the plot
p_edu_flip = p_edu + coord_flip()       
p_edi_flip

# add another variable "sex"
p_edu_sex = ggplot(data = salary) + geom_bar(mapping = aes(x = education, fill = sex)) + coord_flip() 
p_edu_sex


# plot a histogram to see frequency distribution of age
p_age = ggplot(data = salary) + geom_histogram(mapping = aes(x = age))
p_age

# You can change the binwidth number to see different graph effects
p_age2 = ggplot(data = salary) + geom_histogram(mapping = aes(x = age), binwidth = 2)   
p_age2

p_age3 = ggplot(data = salary) + geom_histogram(mapping = aes(x = age), binwidth = 0.2)
p_age3

# scatter plot of capital gain by age
p_capital = ggplot(data = salary) + geom_point(mapping = aes(x = age, y = capital.gain))
p_capital

# scatter plot of capital gain by age and sex
p_capital_sex = ggplot(data = salary) + geom_point(mapping = aes(x = age, y = capital.gain, color = sex))
p_capital_sex

# scatter plot of capital gain by age and education
p_capital_edu = ggplot(data = salary) + geom_point(mapping = aes(x = age, y = capital.gain, color = education))
p_capital_edu

# boxplot of hours per week by sex
p_hours = ggplot(data = salary) + geom_boxplot(mapping = aes(x = sex, y = hours.per.week))
p_hours


# facets: split the plot into subplots by sex
p_capital_sex + facet_wrap(~ sex)
p_capital_sex + facet_wrap(~ sex, nrow = 2)      # specify 2 rows for the subplots


# add title to the plot
p_capital_sex + ggtitle("Scatter Plot of Capital Gain")

# add x- and y-axis labels
p_capital_sex + labs(y = "Capital gain", x = "Age of the person")

# include title and xy labels
p_capital_sex_plot = p_capital_sex + ggtitle("Scatter Plot of Capital Gain") + labs(y = "capital gain", x = "age of the person")
p_capital_sex_plot 

# add themes to the plot
p_capital_sex_plot + theme_bw()
p_capital_sex_plot + theme_classic()
p_capital_sex_plot + theme_light()      

# change the position of legend
p_capital_sex_plot + theme(legend.position = "top")
p_capital_sex_plot + theme(legend.position = "bottom")
p_capital_sex_plot + theme(legend.position = "none")   # remove legend 



# save plot
ggsave("plot_capital.jpeg")
ggsave("plot_capital.pdf")                   # you can save as different format
ggsave("plot_capital3.jpeg", scale = 3)      # if the default scale of saved plot does not look good, you can change the scale

