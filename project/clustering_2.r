# Gapminder Data analysis
# Data Availible from https://www.gapminder.org/data/

install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')

install.packages('cluster')
library(cluster) # for gower similarity, pam, and diana (a divisive hierarchical method). clara() is also included, which is basically pam by sampling

install.packages('Rtsne')
library(Rtsne) # for t-SNE plot

install.packages('ggplot2')
library(ggplot2) # for visualization

# Set the working directory to this script's directory
# windows only
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Import Datasets
# by Indicator name from https://www.gapminder.org/data/

#1800-2013
gdp = read.csv("data/gdp_total_ppp.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2018
child_mortality = read.csv("data/child_mortality_0_5_year_olds_dying_per_1000_born.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2018
children_per_woman = read.csv("data/children_per_woman_total_fertility.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2014
co2_emissions = read.csv("data/co2_emissions_tonnes_per_person.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2018
gini = read.csv("data/gini.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2018
income_per_person = read.csv("data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1800-2018
life_expectancy = read.csv("data/life_expectancy_years.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1970-2015 #could cut 
women_years_school = read.csv("data/mean_years_in_school_women_15_to_24_years.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")
#1960-2008 #could cut 
res_electricity = read.csv("data/residential_electricity_use_total.csv", header = TRUE, check.names = FALSE, encoding="UTF-8")

## Gather as long table
child_mortality_table = child_mortality %>% gather(key = 'year', value = 'child_mortality', -country)
children_per_woman_table = children_per_woman %>% gather(key = 'year', value = 'children_per_woman', -country)
co2_emissions_table = co2_emissions %>% gather(key = 'year', value = 'co2_emissions', -country)
gdp_table = gdp %>% gather(key = 'year', value = 'gdp', -country)
gini_table = gini %>% gather(key = 'year', value = 'gini', -country)
income_table  = income_per_person %>% gather(key = 'income_per_person', value = 'adults_hiv', -country)
life_expectancy_table = life_expectancy %>% gather(key = 'year', value = 'life_expectancy', -country)
women_years_school_table = women_years_school %>% gather(key = 'year', value = 'women_years_school', -country)
res_electricity_table = res_electricity %>% gather(key = 'year', value = 'res_electricity', -country)

# Join tables # could also do with left_joins instead of full_joins to get rid of some NA's. 
total_data = full_join(gdp_table, child_mortality_table, by = c('country','year'))
total_data = full_join(children_per_woman_table, total_data, by = c('country','year'))
total_data = full_join(co2_emissions_table, total_data, by = c('country','year'))
total_data = full_join(gini_table, total_data, by = c('country','year'))
total_data = full_join(income_table, total_data, by = c('country','year'))
total_data = full_join(life_expectancy_table, total_data, by = c('country','year'))
total_data = full_join(women_years_school_table, total_data, by = c('country','year'))
total_data = full_join(res_electricity_table, total_data, by = c('country','year'))

## start working with the data
# more examlpes here https://tidyr.tidyverse.org/ https://uc-r.github.io/tidyr and 
# most importantly https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf 

# Make Trimmed version of data by dropping all rows with NA's. 
data_trimmed = drop_na(total_data)

# make country a factor
data_trimmed$country <- as.factor(data_trimmed$country)

# make year as numeric
data_trimmed$year <- as.numeric(data_trimmed$year)

#### Normalization function
#Function to perform Min max normalize data
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

### Select year
#1970 and 2008 work nicely
df = total_data  %>%  group_by(country) %>% filter(year == 2008 )
df = drop_na(df)
df = subset(df, select = -year)


### make normalized verion of the data:
df_n = df
#normalize df columns
df_n$res_electricity <- normalize(df$res_electricity)
df_n$women_years_school <- normalize(df$women_years_school)
df_n$life_expectancy <- normalize(df$life_expectancy)
df_n$gini <- normalize(df$gini)
df_n$co2_emissions <- normalize(df$co2_emissions)
df_n$children_per_woman <- normalize(df$children_per_woman)
df_n$gdp <- normalize(df$gdp)
df_n$child_mortality <- normalize(df$child_mortality)

###############################################################
################## PAM clustering ###########################

######### Log Transformed data #########

# Calculate distance among the observations
# using the gower distance
gower_dist <- daisy(df[, -1], metric = "gower", type = list(logratio = 3))
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Use PAM (partitioning around medoids) to perform the clustering
# Calculate Silhouette width for 2 to 10 clusters using PAM
sil <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_mat, diss=TRUE, k=i)
  sil[i] <-pam_fit$silinfo$avg.width }

# Plot silhouette width 
plot(1:10, sil,
           xlab = "Number of clusters",
           ylab = "Silhouette Width")
lines(1:10, sil)

# For silhouette width values (higher is better, clusters = 3 has the highest sil value)
# without log transformation we see that 2 clusters give the highest Silhouette Width with values dropping off as number of clusters increases
# when log transfored 2 is still best but 3 and 4 are closer to the Silhouette Width of 2. 
# plotting the data using 3 clusters it partitions into 3 clusters. 

#Cluster Interpretation:via Descriptive Statistics
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

#add cluster labels to the data. We will use result1 later
df <- data.frame(df, pam_fit$cluster)
#show clustering results by college
result1 <- df %>% dplyr::select(country,pam_fit.cluster)
#View(result1)

#group_by cluster and then compute the summary data (means, median, etc) for each cluster
pam_results <- df %>%
     dplyr::select(-country) %>%
     mutate(cluster = pam_fit$clustering) %>% #add the cluster column
     group_by(cluster) %>% #group universities by its cluster 
     do(the_summary = summary(.)) #do: summarize by group/cluster,add the_summary column

pam_results$the_summary

##Cluster Interpretation: via visualization
#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, or t-SNE. 
#This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization.
#it is a powerful but also sometimes puzzling technique, more see https://distill.pub/2016/misread-tsne/ 

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
     data.frame() %>%
     setNames(c("X", "Y")) %>%
     mutate(cluster = factor(pam_fit$clustering), name = df$country)

ggplot(aes(x = X, y = Y), data = tsne_data) +
     geom_point(aes(color = cluster))

tsne_data %>% select(cluster = 1) 

result1 %>% arrange(pam_fit.cluster)

# The results fit almost perfectly with the understanding of what we'd think of as 1st world (developed) 2nd world and 3rd (developing) countries. 


######### Kmeans
#cluster the same data set using kmeans
#different implementations of kmeans are provided throught the algorithm argument to
#kmeans() method: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen". The default H-W implementation works well in most cases.  
#To know more about the differences see https://core.ac.uk/download/pdf/27210461.pdf 

# Logtransfrom data prior 

df.log = log(df[,2:8]+1)

#kmeans() take numeric data, 
#get distance matrix, excluding first column: name
#note: nstart is the parameter that allows the user to try multiple sets of initial centroids. You should use a nstart > 1, for example, nstart=25, this will run kmeans nstart number of times, each time with a different set of initial centroids. kmeans will then select the one with the lowest within cluster variation.
dist_mat <- dist(df.log, method="euclidean")
avgS <- c() #initiate an empty vector
for(k in 2:10){
     kmeans_cl <- kmeans(df.log, centers=k, iter.max=500, nstart=1)
     s <- silhouette(kmeans_cl$cluster, dist_mat)
     avgS <- c(avgS, mean(s[,3])) # take the mean of sil_width of all observations, and save it in the avgS vector
   }
data.frame(nClus=2:10, Silh=avgS)

plot(2:10, avgS,
             xlab = "Number of clusters",
             ylab = "Silhouette Width")
lines(2:10, avgS)

kmeans_fit <- kmeans(df.log, 2)
#get cluster means
aggregate(df.log,by=list(kmeans_fit$cluster),FUN=mean)


# append cluster assignment to data
df_clean_n <- data.frame(df, kmeans_fit$cluster)
##Cluster Interpretation:via visualization
tsne_obj <- Rtsne(dist_mat, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
   mutate(cluster = factor(kmeans_fit$cluster),
                       name = df_clean_n$name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
     geom_point(aes(color = cluster))

#TSNE is a powerful but sometimes puzzling technique More see https://distill.pub/2016/misread-tsne/
  
  
# append cluster assignment to data
df_clean_n <- data.frame(df_clean_n, kmeans_fit$cluster)
#college_clean_n
result2 <- df_clean_n %>% dplyr::select(country,kmeans_fit.cluster)
View(result2)
result2.copy <- result2

result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==2] <- 0
result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==1] <- 2
result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==0] <- 1
#now create a confusion table to see the differences in the two clusterings
table(result1$pam_fit.cluster, result2$kmeans_fit.cluster)






######### Min Max normalized data #########

# Calculate distance among the observations
# using the gower distance
gower_dist <- daisy(df_n[, -1], metric = "gower", type = list())
summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Use PAM (partitioning around medoids) to perform the clustering
# Calculate Silhouette width for 2 to 10 clusters using PAM
sil <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_mat, diss=TRUE, k=i)
  sil[i] <-pam_fit$silinfo$avg.width }

# Plot silhouette width 
plot(1:10, sil,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil)

# For silhouette width values (higher is better, clusters = 3 has the highest sil value)
# without log transformation we see that 2 clusters give the highest Silhouette Width with values dropping off as number of clusters increases
# when log transfored 2 is still best but 3 and 4 are closer to the Silhouette Width of 2. 
# plotting the data using 3 clusters it partitions into 3 clusters. 

#Cluster Interpretation:via Descriptive Statistics
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)

#add cluster labels to the data. We will use result1 later
df <- data.frame(df, pam_fit$cluster)
#show clustering results by college
result1 <- df %>% dplyr::select(country,pam_fit.cluster)
#View(result1)

#group_by cluster and then compute the summary data (means, median, etc) for each cluster
pam_results <- df %>%
  dplyr::select(-country) %>%
  mutate(cluster = pam_fit$clustering) %>% #add the cluster column
  group_by(cluster) %>% #group universities by its cluster 
  do(the_summary = summary(.)) #do: summarize by group/cluster,add the_summary column

pam_results$the_summary

##Cluster Interpretation: via visualization
#One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, or t-SNE. 
#This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization.
#it is a powerful but also sometimes puzzling technique, more see https://distill.pub/2016/misread-tsne/ 

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering), name = df$country)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

tsne_data %>% select(cluster = 1) 

result1 %>% arrange(pam_fit.cluster)


######### Kmeans
#cluster the same data set using kmeans
#different implementations of kmeans are provided throught the algorithm argument to
#kmeans() method: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen". The default H-W implementation works well in most cases.  
#To know more about the differences see https://core.ac.uk/download/pdf/27210461.pdf 

#kmeans() take numeric data, 
#get distance matrix, excluding first column: name
#note: nstart is the parameter that allows the user to try multiple sets of initial centroids. You should use a nstart > 1, for example, nstart=25, this will run kmeans nstart number of times, each time with a different set of initial centroids. kmeans will then select the one with the lowest within cluster variation.
dist_mat <- dist(df_n[, -1], method="euclidean")
avgS <- c() #initiate an empty vector
for(k in 2:10){
  kmeans_cl <- kmeans(df_n[, -1], centers=k, iter.max=500, nstart=1)
  s <- silhouette(kmeans_cl$cluster, dist_mat)
  avgS <- c(avgS, mean(s[,3])) # take the mean of sil_width of all observations, and save it in the avgS vector
}
data.frame(nClus=2:10, Silh=avgS)

plot(2:10, avgS,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(2:10, avgS)

kmeans_fit <- kmeans(df_n[, -1], 2)
#get cluster means
aggregate(df_n[, -1],by=list(kmeans_fit$cluster),FUN=mean)


# append cluster assignment to data
df_clean_n <- data.frame(df, kmeans_fit$cluster)
##Cluster Interpretation:via visualization
tsne_obj <- Rtsne(dist_mat, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(kmeans_fit$cluster),
         name = df_clean_n$name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#TSNE is a powerful but sometimes puzzling technique More see https://distill.pub/2016/misread-tsne/

# append cluster assignment to data
df_clean_n <- data.frame(df_clean_n, kmeans_fit$cluster)
#college_clean_n
result2 <- df_clean_n %>% dplyr::select(country,kmeans_fit.cluster)
View(result2)
result2.copy <- result2

result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==2] <- 0
result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==1] <- 2
result2$kmeans_fit.cluster[result2$kmeans_fit.cluster==0] <- 1
#now create a confusion table to see the differences in the two clusterings
table(result1$pam_fit.cluster, result2$kmeans_fit.cluster)
