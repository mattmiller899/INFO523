# Gapminder Data analysis
# Data Availible from https://www.gapminder.org/data/

install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')

# Set the working directory to this script's directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Import Datasets
# by Indicator name from https://www.gapminder.org/data/

# Adults with HIV (%, age 15-49)
Adults_HIV = read.csv("data/Adults_15-49_HIV.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

# Underweight children (added the fileEncoding, think its a Windows issue)
Underweight_children = read.csv("data/Underweight_children.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")


# Total GDP (PPP$, inflation-adjusted)
gdp_total_ppp = read.csv("data/gdp_total_ppp.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

# Population aged 40-59 years, female (%)
female_40_59_pop_percent = read.csv("data/female_40_59_pop_percent.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

### Make Long dataframes from data
Adults_HIV_long = Adults_HIV %>% gather(key = 'year', value = 'Adults_HIV', -country)
Underweight_children_long = Underweight_children %>% gather(key = 'year', value = 'Underweight_children', -country)
gdp_total_ppp_long = gdp_total_ppp %>% gather(key = 'year', value = 'gdp_total_ppp', -country)
female_40_59_pop_percent_long = female_40_59_pop_percent %>% gather(key = 'year', value = 'female_40_59_pop_percent', -country)


# Join csvs into full dataset
total_data = full_join(Adults_HIV_long, Underweight_children_long, by = c('country','year'))
total_data = full_join(gdp_total_ppp_long, total_data, by = c('country','year'))
total_data = full_join(female_40_59_pop_percent_long, total_data, by = c('country','year'))

# could also do with left_joins instead of full_joins to get rid of some NA's. 

## start working with the data
# more exaamlpes here https://tidyr.tidyverse.org/ https://uc-r.github.io/tidyr and 
# most importantly https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf 

# meaningless example just an example of how to get at the data
total_data  %>% group_by(country) %>% summarise(avg = mean(Adults_HIV, na.rm = TRUE))

# examlpe of how to drill down 
drilled_down_data_ex = total_data  %>%  group_by(country) %>% filter(year > 1900 & Adults_HIV > 0.00 & Underweight_children > 0.00)


# MATT'S CODE BELOW

# Filter data so that only data points with both attributes not having NA's are used

test_data = total_data %>% filter(Adults_HIV != "NA" & Underweight_children != "NA")
test_corr = cor(test_data$Adults_HIV, test_data$Underweight_children)

hiv_vs_weight_data = total_data %>% group_by(country) %>% filter(Adults_HIV != "NA" & Underweight_children != "NA")
hiv_vs_female_data = total_data %>% group_by(country) %>% filter(Adults_HIV != "NA" & female_40_59_pop_percent != "NA")
hiv_vs_gdp_data = total_data %>% group_by(country) %>% filter(Adults_HIV != "NA" & gdp_total_ppp != "NA")
weight_vs_female_data = total_data %>% group_by(country) %>% filter(Underweight_children!= "NA" & female_40_59_pop_percent != "NA")
weight_vs_gdp_data = total_data %>% group_by(country) %>% filter(Underweight_children != "NA" & gdp_total_ppp != "NA")
gdp_vs_female_data = total_data %>% group_by(country) %>% filter(female_40_59_pop_percent != "NA" & gdp_total_ppp != "NA")

#Covariances

hiv_vs_weight_cov = cov(hiv_vs_weight_data$Adults_HIV, hiv_vs_weight_data$Underweight_children)
hiv_vs_female_cov = cov(hiv_vs_female_data$female_40_59_pop_percent, hiv_vs_female_data$Adults_HIV)
hiv_vs_gdp_cov = cov(hiv_vs_gdp_data$Adults_HIV, hiv_vs_gdp_data$gdp_total_ppp)
weight_vs_gdp_cov = cov(weight_vs_gdp_data$Underweight_children, weight_vs_gdp_data$gdp_total_ppp)
weight_vs_female_cov = cov(weight_vs_female_data$female_40_59_pop_percent, weight_vs_female_data$Underweight_children)
gdp_vs_female_cov = cov(gdp_vs_female_data$gdp_total_ppp, gdp_vs_female_data$female_40_59_pop_percent)

hiv_vs_weight_cov
hiv_vs_female_cov
hiv_vs_gdp_cov
weight_vs_gdp_cov
weight_vs_female_cov
gdp_vs_female_cov


#Correlation coefficients

hiv_vs_weight_corr = cor(hiv_vs_weight_data$Adults_HIV, hiv_vs_weight_data$Underweight_children)
hiv_vs_female_corr = cor(hiv_vs_female_data$Adults_HIV, hiv_vs_female_data$female_40_59_pop_percent)
hiv_vs_gdp_corr = cor(hiv_vs_gdp_data$Adults_HIV, hiv_vs_gdp_data$gdp_total_ppp)
weight_vs_gdp_corr = cor(weight_vs_gdp_data$Underweight_children, weight_vs_gdp_data$gdp_total_ppp)
weight_vs_female_corr = cor(weight_vs_female_data$Underweight_children, weight_vs_female_data$female_40_59_pop_percent)
gdp_vs_female_corr = cor(gdp_vs_female_data$gdp_total_ppp, gdp_vs_female_data$female_40_59_pop_percent)

hiv_vs_weight_corr
hiv_vs_female_corr
hiv_vs_gdp_corr
weight_vs_gdp_corr
weight_vs_female_corr
gdp_vs_female_corr

# More additions from Kai

# Correlation in depth, examining hiv vs underweight children correlation by counrty
cor1 = drilled_down_data_ex %>%  group_by(country) %>% summarise(cor_pearson = cor(Adults_HIV, Underweight_children, method = 'pearson' )) %>% filter(!is.na(cor_pearson))

# discretization

#examine binning by time
#hiv_vs_gdp_pearson_corr
hiv_vs_gdp_pearson_corr_binned = transform(drilled_down_data_ex, bin = cut(as.numeric(year), 5))
hiv_vs_gdp_pearson_corr_binned = hiv_vs_gdp_pearson_corr_binned %>%  group_by(bin)  %>% summarise(cor_pearson = cor(Adults_HIV, gdp_total_ppp, method = 'pearson' )) %>% filter(!is.na(cor_pearson))

# examine binning by hiv infection rate
hiv_vs_gdp_pearson_corr_binned_hiv = transform(drilled_down_data_ex, bin = cut(Adults_HIV, 3))
hiv_vs_gdp_pearson_corr_binned_hiv = hiv_vs_gdp_pearson_corr_binned_hiv %>%  group_by( bin)  %>% summarise(cor_pearson = cor(Adults_HIV, gdp_total_ppp, method = 'pearson' )) %>% filter(!is.na(cor_pearson))



