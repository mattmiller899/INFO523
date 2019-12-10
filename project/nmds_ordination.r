# Gapminder Data analysis
# Data Availible from https://www.gapminder.org/data/

install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')

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

# examlpe of how to drill down 
#drilled_down_data_ex = total_data  %>%  group_by(country) %>% filter(year > 1900 & adults_hiv > 0.00 & underweight_children > 0.00)

# Make Trimmed version of data by dropping all rows with NA's. 
data_trimmed = drop_na(total_data)

# make country a factor
data_trimmed$country <- as.factor(data_trimmed$country)

# make year as numeric
data_trimmed$year <- as.numeric(data_trimmed$year)

### trying to do stuff can ignore from here:
# data_trimmed = data_trimmed %>% 
#   unite(country_year, c("country", "year"))

#Function to perform Min max normalize data
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

##PCOA
#check out https://www.davidzeleny.net/anadat-r/doku.php/en:ordiagrams_examples
install.packages('vegan')
library(vegan)

#df =  filter(data_trimmed, grepl('c|d', country))
#1970 and 2008 work
df = total_data  %>%  group_by(country) %>% filter(year == 2008 )
df = drop_na(df)
#df = subset(df_sub, select = -c(country,year) )
df = subset(df, select = -year)

#normalize df columns
df$res_electricity <- normalize(df$res_electricity)
df$women_years_school <- normalize(df$women_years_school)
df$life_expectancy <- normalize(df$life_expectancy)
df$gini <- normalize(df$gini)
df$co2_emissions <- normalize(df$co2_emissions)
df$children_per_woman <- normalize(df$children_per_woman)
df$gdp <- normalize(df$gdp)
df$child_mortality <- normalize(df$child_mortality)

# my data #with data_trimmed remove country
my.dis <- vegdist(decostand(df[,-1],"hellinger")) 
#my.dis <- vegdist(wisconsin(df))

my.pcoa <- cmdscale(my.dis, eig = TRUE)
my.pcoa$species <- wascores(my.pcoa$points, df[,-1], expand = TRUE)
pl <- ordiplot(my.pcoa, type = "none")

orditorp(my.pcoa, 'sites', labels = df$country)

#points(pl, "sites", pch=21, col="red", bg="yellow")
text(pl, "species", col="blue", cex=0.9)

########################## testing ################################
### testing PCOA purposes
# Draw a plot for a non-vegan ordination (cmdscale).
data(dune)
dune.dis <- vegdist(wisconsin(dune))
dune.mds <- cmdscale(dune.dis, eig = TRUE)
dune.mds$species <- wascores(dune.mds$points, dune, expand = TRUE)
pl <- ordiplot(dune.mds, type = "none")
points(pl, "sites", pch=21, col="red", bg="yellow")
text(pl, "species", col="blue", cex=0.9)
# Default plot of the previous using identify to label selected points
## Not run: 
pl <- ordiplot(dune.mds)
identify(pl, "spec")
## End(Not run)


#### testing cca https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/cca

# Note about cca: The shotgun method is to use all environmental variables as constraints. 
# However, such exploratory problems are better analysed with unconstrained methods such as 
# correspondence analysis (decorana, corresp) or non-metric multidimensional scaling (metaMDS) 
# and environmental interpretation after analysis (envfit, ordisurf). CCA is a good choice if 
# the user has clear and strong a priori hypotheses on constraints and is not interested in 
# the major structure in the data set.

data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca)
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca)
## `Partialling out' and `negative components of variance'
cca(varespec ~ Ca, varechem)
cca(varespec ~ Ca + Condition(pH), varechem)
## RDA
data(dune)
data(dune.env)
dune.Manure <- rda(dune ~ Manure, dune.env)
plot(dune.Manure) 


## The recommended way of running NMDS (Minchin 1987)
data(dune)
# Global NMDS using monoMDS
sol <- metaMDS(dune)
sol
plot(sol, type="t")
## Start from previous best solution
sol <- metaMDS(dune, previous.best = sol)
## Local NMDS and stress 2 of monoMDS
sol2 <- metaMDS(dune, model = "local", stress=2)
sol2
plot(sol2, type="t")

## Use Arrhenius exponent 'z' as a binary dissimilarity measure
sol <- metaMDS(dune, distfun = betadiver, distance = "z")
sol


### decorana
# Detrended Correspondence Analysis and Basic Reciprocal Averaging

data(varespec)
vare.dca <- decorana(varespec)
vare.dca
summary(vare.dca)
plot(vare.dca)

### the detrending rationale:
gaussresp <- function(x,u) exp(-(x-u)^2/2)
x <- seq(0,6,length=15) ## The gradient
u <- seq(-2,8,len=23)   ## The optima
pack <- outer(x,u,gaussresp)
matplot(x, pack, type="l", main="Species packing")
opar <- par(mfrow=c(2,2))
plot(scores(prcomp(pack)), asp=1, type="b", main="PCA")
plot(scores(decorana(pack, ira=1)), asp=1, type="b", main="CA")
plot(scores(decorana(pack)), asp=1, type="b", main="DCA")
plot(scores(cca(pack ~ x), dis="sites"), asp=1, type="b", main="CCA")

#### corresp
###Simple Correspondence Analysis
# package ‘corresp’ is not available (for R version 3.6.0) Nevermind
#install.packages('corresp')
#library('corresp')
#(ct <- corresp(~ N + P, data = varechem))
#plot(ct)
#corresp(caith)
#biplot(corresp(caith, nf = 2))

### envfit
# Fits an Environmental Vector or Factor onto an Ordination

# could do this instead of having cca app, instead run PCOA 
# first then allow additional option to add the env fit from the
# ontology searched env parameters over top. 

par(mfrow=c(1,1))
data(varespec, varechem)
library(MASS)
ord <- metaMDS(varespec)
(fit <- envfit(ord, varechem, perm = 999))
scores(fit, "vectors")
plot(ord)
plot(fit)
plot(fit, p.max = 0.05, col = "red")
## Adding fitted arrows to CCA. We use "lc" scores, and hope
## that arrows are scaled similarly in cca and envfit plots
ord <- cca(varespec ~ Al + P + K, varechem)
plot(ord, type="p")
fit <- envfit(ord, varechem, perm = 999, display = "lc")
plot(fit, p.max = 0.05, col = "red")
## 'scaling' must be set similarly in envfit and in ordination plot
plot(ord, type = "p", scaling = "sites")
fit <- envfit(ord, varechem, perm = 0, display = "lc", scaling = "sites")
plot(fit, col = "red")
