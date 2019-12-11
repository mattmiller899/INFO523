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

#1970 and 2008 work nicely
df = total_data  %>%  group_by(country) %>% filter(year == 2008 )
df = drop_na(df)
df = subset(df, select = -year)

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
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

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


#Resuts suggest three distinct clusters with the following means:

#          res_electricity, women_years_school, life_expectancy,  gini coef,    co2_emissions, children_per_woman, gdp,             child_mortality
#Cluster1: Mean:5.556e+10   Mean:12.15           Mean:77.16       Mean:33.2    Mean:10.174    Mean:1.677          Mean:8.345e+11    Mean: 8.353
#Cluster2: Mean:3.094e+10   Mean:9.834           Mean:74.26       Mean:41.94   Mean:4.414     Mean:2.455          Mean:7.401e+11    Mean:22.45 
#Cluster3: Mean:7.972e+09   Mean:6.684           Mean:60.60       Mean:44.02   Mean:1.2321    Mean:4.308          Mean:2.604e+11    Mean: 82.21


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

#build a hierarchical cluster 
#method argument takes the agglomeration method to be used. This should be (an unambiguous abbreviation of)
# one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).

h <- hclust(gower_dist, method="ward.D2")

#plot the dendrogram, #hang decides where to start labels on axis. -0.1 is negative, so the x-axis labels will hang down from 0
plot(h, hang=-0.1)

clus2 <- cutree(h, 2)
table(result1$pam_fit.cluster, clus2)

#If we want three clusters
clus3 <- cutree(h, 3)
table(result1$pam_fit.cluster, clus3)

#When using the ward.D2 method to do hierarchical clustering, we find that the results of cutting the dendrogram into 3 partitions 
# very closely matches how the PAM clusters.  

clus4 <- cutree(h, 4)
table(result1$pam_fit.cluster, clus4)

