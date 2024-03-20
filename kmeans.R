

##### week 4 in-class exercise #####

rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")              # set work directory; change the path inside "your computer/your folder" of your own path
#Alternatively, use drop-down menu: "Session-Set Working Directory-Choose Directory" to choose the folder you want to work in


##### Clustering Analysis #####
install.packages("cluster")      # install packages needed for cluster analysis
install.packages("factoextra")


library(ggplot2)                 # load the libraries
library(cluster)                 
library(factoextra)


# read comedy data
comedy = read.csv("data_comedy_survey.csv", header=TRUE, sep=",", stringsAsFactors = T)     # specify the option stringAsFactors so that string variables are read as factors in R

# sanity check of the imported data
str(comedy)

# data preparation
#check if there is missing values in the data
sum(is.na(comedy))      

# if there are missing value
# if want to remove those missing values
comedy_noNA = na.omit(comedy)              
# here the new dataframe is exactly the same as old dataframe because we don't have missing values

# if want to impute the missing values with mean for a certain variable
brian_mean = mean(comedy$Brian)
comedy$Brian[is.na(comedy$Brian)==TRUE] = brian_mean    # you can do so similarly with median

brian_median = median(comedy$Brian)
comedy$Brian[is.na(comedy$Brian)==TRUE] = brian_median

# check the class of a variable
class(comedy$ComedyAtt_1)

# check levels of a factor variable
levels(comedy$ComedyAtt_1)

# remove object
rm(comedy_noNA)

# look at number of observations in the data set
nrow(comedy)

# get number of levels for factor variable
level_edu = unique(comedy$Edu)    # get unique levels of each education level
class(level_edu)
length(level_edu)                 # get number of education levels

# plot a boxplot for viewer's liking score of Russell by sex
ggplot(data = comedy) + geom_boxplot(mapping = aes(x = Sex, y = Russell)) + theme_bw()

# plot a boxplot for viewer's liking score of Russell by sex and get each sub-group by education level
ggplot(data = comedy) + geom_boxplot(mapping = aes(x = Sex, y = Russell)) + facet_wrap(~Edu) + theme_bw()

# save the plot
ggsave("boxplot_Russell.jpeg", scale = 3)

####### clustering 
# only include variables that can apply k-means clustering
# exclude variables Sex, Age, Race, Edu
km_comedy = comedy[,1:19]      # only include first 19 columns

# convert survey agree/disagree variables into numeric for k-means analysis
km_comedy = data.frame(sapply(km_comedy,unclass))
# sapply(X, function) - apply function to X
# unclass - a function to remove the class attribute

str(km_comedy)         # check variable types

# standardize variables
km_comedy = scale(km_comedy)

# apply k-means algorithm
km2 = kmeans(km_comedy, centers = 2)                 # specify number of clusters = 2
km2 = kmeans(km_comedy, centers = 2, nstart = 20)    # multiple (20) initial configurations and report on the best one.
km2                                                  # look at the outcomes
km2$cluster                                          # look at each point's cluster assignment
km2$size                                             # the number of points in each cluster
km2$tot.withinss                                     # total within-cluster sum of squares
sum(km2$withinss)                                    # the sum of all the within-cluster sum of squares = total within-cluster sum of squares

# visualize the clusters
fviz_cluster(km2, data = km_comedy)                  
fviz_cluster(km2, data = km_comedy) + theme_bw() + ggtitle("Two Clusters for Comedy Data") # can add elements from ggplot2
ggsave("two_cluster.jpeg", scale = 3)

km3 = kmeans(km_comedy, centers = 3)                    # specify 3 clusters 
fviz_cluster(km3, data = km_comedy) + theme_classic() + ggtitle("Three Clusters for Comedy Data")
ggsave("three_cluster.jpeg", scale = 3)


# decide the optimal number of clusters
fviz_nbclust(km_comedy, kmeans, method = "wss")         # look at the plot to determine the optimal number of clusters
ggsave("elbow_method.jpeg", scale = 3)

fviz_nbclust(km_comedy, kmeans, method = "silhouette")  # find the #clusters such that maximize the avg silhouette width
ggsave("silhouette_method.jpeg", scale = 3)


# segment the data according to finalized clusters
km_optimal = kmeans(km_comedy, centers = 2, nstart = 20)
kmdata1 = comedy[km_optimal$cluster==1,]
kmdata2 = comedy[km_optimal$cluster==2,]


##### Hierarchical Clustering #####
# calculate dissimilarity matrix
dis_matrix = dist(km_comedy, method = "euclidean")

# hierarchical clustering using complete linkage
hc_complete = hclust(dis_matrix, method = "complete")   

# plot the obtained dendrogram
plot(hc_complete)
plot(hc_complete, hang = -1)              # labels at the same level
plot(hc_complete, hang = -1, cex = 0.5)   # scale the plotting text and symbols smaller

# save the graph from plot()
jpeg(file = "dendrogram.jpeg", width = 1000, height = 1000)   # the default values of width and height are both 480, you can set larger values to make the graph larger
plot(hc_complete, hang = -1, cex = 0.5, main = "Dendrogram complete method")
dev.off()

# another function to do hierarchical clustering
hc_c2 = agnes(dis_matrix, method = "complete")     # nice thing about agnes() -> gives agglomerative coefficient
hc_c2$ac           # $ac = agglomerative coefficient -> close to 1 suggests strong clustering structure

# then we can compare across different methods using agnes$ac
m = c( "average", "single", "complete", "ward")
names(m) = c( "average", "single", "complete", "ward")
for (i in 1:4){
  m[i] = agnes(dis_matrix, method = names(m)[i])$ac
}
print(m)       
# look at the agglomerative coefficients, which method identifies the strongest clustering structure? - ward method


hc_ward = agnes(dis_matrix, method = "ward")
jpeg(file = "dendrogram_ward.jpeg", width = 1000, height = 1000)
pltree(hc_ward, hang = -1, cex = 0.5, main = "Dendrogram ward method")
dev.off()


# working with dendrograms: the height of the cut to dendrogram controls the number of clusters obtained
hc_ward2 = hclust(dis_matrix, method = "ward.D2")

# cut tree into several groups
# cut tree into 3 clusters
sub_group1 = cutree(hc_ward2, k = 3)
jpeg(file = "dendrogram_3clusters.jpeg", width = 1000, height = 1000)
plot(hc_ward2, cex = 0.5)
rect.hclust(hc_ward2, k =3, border = 2:4)
dev.off()

# cut tree into 4 clusters
sub_group2 = cutree(hc_ward2, k = 4)
jpeg(file = "dendrogram_4clusters.jpeg", width = 1000, height = 1000)
plot(hc_ward2, cex = 0.5)
rect.hclust(hc_ward2, k =4, border = 2:4)
dev.off()

# visualize and save the results
fviz_cluster(list(data = km_comedy, cluster = sub_group1)) + ggtitle("HC Plot")
ggsave("HCvisualize_3clusters.jpeg", scale = 3)
fviz_cluster(list(data = km_comedy, cluster = sub_group2)) + ggtitle("HC Plot")
ggsave("HCvisualize_4clusters.jpeg", scale = 3)

# cut agnes() tree into 3 groups
hc_ag = agnes(dis_matrix, method = "ward")
cutree(as.hclust(hc_ag), k = 3)       # just need to set the object as.hclust() if using agnes() instead of hclust()


# determine optimal #clusters
fviz_nbclust(km_comedy, FUN = hcut, method = "wss")
fviz_nbclust(km_comedy, FUNcluster = hcut, method = "silhouette")

# use the optimal hc cluster
hc_optimal = hclust(dis_matrix, method = "ward.D2")
hc_group = cutree(hc_optimal, k = 2)



