install.packages('dplyr')
library(dplyr) # for data cleaning

install.packages('ISLR')
library(ISLR) # for college dataset

install.packages('cluster')
library(cluster) # for gower similarity, pam, and diana (a divisive hierarchical method). clara() is also included, which is basically pam by sampling

install.packages('Rtsne')
library(Rtsne) # for t-SNE plot

install.packages('ggplot2')
library(ggplot2) # for visualization


#Clustering requires 
#1. Calculating distance among the observations
#2. Choosing a clustering algorithm
#3. Selecting the number of clusters
  
#distance measures for continuous variables: dist()
#generate a 5x10 matrix with random data
  
set.seed(2345) #to get repeatable results

randDat <- matrix(rnorm(50), nrow=5)
#default dist(): Euclidean distance
dist(randDat)

#Manhattan distance
dist(randDat, method="manhattan")

dist(randDat, method="minkowski", p=4)

#dist(randDat) = dist(randDat, method="minkowski", p=2)
dist(randDat, method="minkowski", p=2)

#dist(randDat, method="manhattan") = dist(randDat, method="minkowski", p=1)
dist(randDat, method="minkowski", p=1)

set.seed(1680) # for reproducibility

data(College, package="ISLR") #with mixed typed variables

#A data frame with 777 observations on the following 18 variables.
#Private A factor with levels No and Yes indicating private or public university
#Apps Number of applications received
#Accept Number of applications accepted
#Enroll Number of new students enrolled
#Top10perc Pct. new students from top 10% of H.S. class
#Top25perc Pct. new students from top 25% of H.S. class
#F.Undergrad Number of fulltime undergraduates
#P.Undergrad Number of parttime undergraduates
#Outstate Out-of-state tuition
#Room.Board Room and board costs
#Books Estimated book costs
#Personal Estimated personal spending
#PhD Pct. of faculty with Ph.D.’s
#Terminal Pct. of faculty with terminal degree
#S.F.Ratio Student/faculty ratio
#perc.alumni Pct. alumni who donate
#Expend Instructional expenditure per student
#Grad.Rate Graduation rate

glimpse(College)

#data transformation
#Acceptance rate is created by diving the number of acceptances by the number of applications
#isElite is created by labeling colleges with more than 50% of their new students who were in the top 10% of their high school class as elite
#mutate() adds new variables and preserves existing; 
  
college_clean <- College %>% mutate(name = row.names(.),
                      accept_rate = Accept/Apps,
                       isElite = cut(Top10perc,
                                                            breaks = c(0, 50, 100),
                                                           labels = c("Not Elite", "Elite"),
                                                          include.lowest = TRUE)) %>%
     mutate(isElite = factor(isElite)) %>%
     select(name, accept_rate, Outstate, Enroll, Grad.Rate, Private, isElite)
  
glimpse(college_clean)


#cluster the same data set using kmeans
#different implementations of kmeans are provided throught the algorithm argument to
#kmeans() method: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen". The default H-W implementation works well in most cases.  
#To know more about the differences see https://core.ac.uk/download/pdf/27210461.pdf 
#kmeans() take numeric data, 

college_clean_n <- College %>%  
   mutate(name = row.names(.), accept_rate = Accept/Apps) %>% 
   select(name, accept_rate, Outstate, Enroll, Grad.Rate, Private, Top10perc)

#turn 'yes' 'no' to 1 and 0
college_clean_n$Private<-as.integer(college_clean_n$Private)-1L
#z-score transformation to scale the variables. Scaling is needed because we will use euclidean distant
college_clean_n %>% mutate_at(scale, .vars=vars(-name))



#get distance matrix, excluding first column: name
#note: nstart is the parameter that allows the user to try multiple sets of initial centroids. You should use a nstart > 1, for example, nstart=25, this will run kmeans nstart number of times, each time with a different set of initial centroids. kmeans will then select the one with the lowest within cluster variation.
dist_mat <- dist(college_clean_n[, -1], method="euclidean")
avgS <- c() #initiate an empty vector
for(k in 2:10){
     kmeans_cl <- kmeans(college_clean_n[,-1], centers=k, iter.max=500, nstart=1)
     s <- silhouette(kmeans_cl$cluster, dist_mat)
     avgS <- c(avgS, mean(s[,3])) # take the mean of sil_width of all observations, and save it in the avgS vector
   }
data.frame(nClus=2:10, Silh=avgS)

plot(2:10, avgS,
           xlab = "Number of clusters",
           ylab = "Silhouette Width")
lines(2:10, avgS)




# Use PAM (partitioning around medoids) to perform the clustering
# Calculate Silhouette width for 2 to 10 clusters using PAM
sil <- c(NA)

for(i in 2:10){
      pam_fit <- pam(gower_mat, diss=TRUE, k=i)
      sil[i] <-pam_fit$silinfo$avg.width }


















#get distance matrix, excluding first column: name
#note: nstart is the parameter that allows the user to try multiple sets of initial centroids. You should use a nstart > 1, for example, nstart=25, this will run kmeans nstart number of times, each time with a different set of initial centroids. kmeans will then select the one with the lowest within cluster variation.

dist_mat <- dist(college_clean_n[, -1], method="euclidean")
avgS <- c() #initiate an empty vector
for(k in 2:10){
    kmeans_cl <- kmeans(college_clean_n[,-1], centers=k, iter.max=500, nstart=1)
    s <- silhouette(kmeans_cl$cluster, dist_mat)
    avgS <- c(avgS, mean(s[,3])) # take the mean of sil_width of all observations, and save it in the avgS vector
   }

data.frame(nClus=2:10, Silh=avgS)

plot(2:10, avgS,
          xlab = "Number of clusters",
          ylab = "Silhouette Width")
lines(2:10, avgS)

#2 clusters seem to be the best
kmeans_fit <- kmeans(college_clean_n[,-1], 2)
#get cluster means
aggregate(college_clean_n[,-1],by=list(kmeans_fit$cluster),FUN=mean)

# append cluster assignment to data
college_clean_n <- data.frame(college_clean_n, kmeans_fit$cluster)
##Cluster Interpretation:via visualization
tsne_obj <- Rtsne(dist_mat, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
     data.frame() %>%
     setNames(c("X", "Y")) %>%
     mutate(cluster = factor(kmeans_fit$cluster),
                       name = college_clean_n$name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
     geom_point(aes(color = cluster))

  

