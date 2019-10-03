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

