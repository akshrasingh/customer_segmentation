# R DATA SCIENCE PROJECT -CUSTOMER SEGMENTATION 
customer_data=read.csv("C:/Users/Akshra_/Downloads/Mall_Customers.csv")
#the structure of the dataframe
str(customer_data)
#the names of the column in dataframe
names(customer_data)
#the first 5 rows of dataframe
head(customer_data)
#To get statistical measure - summary gives minimun ,maximum ,mean ,median ,Ist,2nd ,3rd quartile 
#we get summary of age column in the table 
summary(customer_data$Age) 
#to get standard deviation of age , more sd more the values are deviated from mean
sd(customer_data$Age)
#similarly do it for income and spending data 
summary(customer_data$Annual.Income..k..)
sd(customer_data$Annual.Income..k..)
summary(customer_data$Age)
sd(customer_data$Spending.Score..1.100.)
# Check for missing values
any(is.na(customer_data))
# Replace missing values in a specific column with mean
customer_data$Age[is.na(customer_data$Age)] <- mean(customer_data$Age, na.rm = TRUE)
# Detect and remove outliers using the IQR method for Age column
Q1 <- quantile(customer_data$Age, 0.25)
Q3 <- quantile(customer_data$Age, 0.75)
IQR <- Q3 - Q1
customer_data <- customer_data[!(customer_data$Age < Q1 - 1.5 * IQR | customer_data$Age > Q3 + 1.5 * IQR), ]
# Creating a new feature 'Age_Squared'   
#feature engineering 
customer_data$Age_Squared <- customer_data$Age^2


#part2-------------------------
#data visualisation------------

#barplot 
#for gender comparision

a=table(customer_data$Gender)
barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=c("pink","lightblue"),
        legend=rownames(a))

# histogram 
#to show count of  age 
summary(customer_data$Age)
hist(customer_data$Age,
     col="lightblue",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)
#box plot 
#for analysis of age

boxplot(customer_data$Age,
        col="pink",
        main="Boxplot for Descriptive Analysis of Age")


#histogram
#annual income frequency 
summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,
     col="lightblue",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)


#density plot 
#annual income 
plot(density(customer_data$Annual.Income..k..),
     col="pink",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")
polygon(density(customer_data$Annual.Income..k..),
        col="lightblue")

#boxplot 
#spending box -plot analysis
summary(customer_data$Spending.Score..1.100.)
boxplot(customer_data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="pink",
        main="BoxPlot for Descriptive Analysis of Spending Score")


#hisotgram
#spending frequnecy histogram
hist(customer_data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="lightblue",
     labels=TRUE)
#ml part

library(purrr)
install.packages("ggplot")
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}

k.values <- 1:10


iss_values <- map_dbl(k.values, iss)

plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares",col="red")

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
library(ggplot2)
pcclust$rotation[,1:2]
set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")





ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")






kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("bottomleft",unique(dignm),fill=unique(kCols(digCluster)))
