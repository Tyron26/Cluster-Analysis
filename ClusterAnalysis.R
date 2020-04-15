########################
####Set up the Directory
########################

# Note that if you open Cluster_Mall_Visitors.R 
# by clicking the file, it will automatically 
# set the directory to the folder you are going to working in.
# After setting up, everything will be done in this folder: your code, dataset, output

# Or use setwd() to set the directory to the folder
# After setting up, everything will be done in this folder: your code, dataset, output
# In Windows e.g.: 
# setwd("C:/...your preferred directory")
# In Mac e.g.: 
# setwd("C:\...your preferred directory")

########################################################
#### Call the libraries you use later
#### Make sure you installed the corresponding packages
#######################################################


########################################################
# Step 1 Import the data
########################################################
DT = read.csv("Mall_Visits.csv")


########################################################
# Step 2 Look at the first 5 rows of the data
########################################################

head(DT, 5)
# Look at the last 5 rows of the data
tail(DT, 5)

#Check regularities in the data
summary(DT)

########################################################
# Step 3 Scale the data. 
########################################################
# Note that the first column is ID number
# Scaling ID number does not make any sense
ScaDT = apply(DT[, 2:9], 2, function(x) scale(x)) 

# Show the Euclidean distant for the first 10 customers

########################################################
# Step 4 we choose column 1:6 -- attitudinal questions for segmentation.
# Income and MallVisit will be used for profiling
########################################################

########################################################
# Step 5 we use Euclidean distance
########################################################

#Just get a quick sense of the distance of the first 10 customers
EuclideanD <- dist(ScaDT[1:10, 1:6])
# You cannot directly view EuclideanD by using View(EuclideanD)
# To view the results, you have to make it as a matrix.
EuclideanD = as.matrix(EuclideanD)
View(EuclideanD)
# Let's round up the numbers to the 10th digits
View( round(EuclideanD, 1) )

## You can skip this part with no harm for your exam
## We can define our own distance:
## The two variables are the same if the difference is less or equal to 2
# My_Distance_function<-function(x,y){sum(abs(x-y) > 2)}
# RawD = apply(ScaDT[1:10, ],1,function(i) 
#        apply(ScaDT[1:10, ],1,function(j) My_Distance_function(i,j) ))
# RawD = as.matrix(RawD)
# View( round(RawD, 1) )


########################################################
# Step 6: Cluster method
########################################################

## we first look at Hierarchical Cluster

# Euclidean distance again for all the customers
EuclideanD <- dist(ScaDT[, 1:6])
# hclust() function performs a hierarchical cluster anlaysis based on the distance
Hierarchical_Cluster <- hclust(EuclideanD)
# Plot the Dendrogram
plot(Hierarchical_Cluster)
# Remember to save the Dendrogram plot
png(file="Dendrogram.png",
    width=700, height=500)
plot(Hierarchical_Cluster)
dev.off()

# Or plot the heatmap
heatmap(as.matrix(ScaDT[, 1:6]))
# save the plot
png(file="HeatMap_ClusterAnalysis.png",
    width=700, height=800)
heatmap(as.matrix(ScaDT[, 1:6]))
dev.off()

HC_membership = as.vector(cutree(Hierarchical_Cluster, k = 3))
DT_HC = cbind(DT, HC_membership)
head(DT_HC)

########################################################
# Step 7: Profiling and interpretation
########################################################

prop.table(table(DT_HC$HC_membership))

Mean_by_Group <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

Group_Character_HC = round( Mean_by_Group(DT_HC, DT_HC$HC_membership) )
View(Group_Character_HC)
write.csv(Group_Character_HC, "Group_Character_HC.csv", row.names = FALSE)


########################################################
# Step 6 revisited: K-means
########################################################
set.seed(888)
kmeans_Cluster <- kmeans(ScaDT[, 1:6],centers= 3, iter.max=2000)
Kmeans_membership = as.vector(kmeans_Cluster$cluster)
DT_Kmeans = cbind(DT, Kmeans_membership)
head(DT_Kmeans)

prop.table(table(DT_Kmeans$Kmeans_membership))


Group_Character_Kmeans = round( Mean_by_Group(DT_Kmeans, DT_Kmeans$Kmeans_membership) )
View(Group_Character_Kmeans)
write.csv(Group_Character_Kmeans, "Group_Character_Kmeans.csv", row.names = FALSE)
