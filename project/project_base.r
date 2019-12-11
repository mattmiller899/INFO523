# Gapminder Data analysis
# Data Availible from https://www.gapminder.org/data/

install.packages('dplyr')
install.packages('tidyverse')

library('dplyr')
library('tidyverse')

# Set the working directory to this script's directory
# windows only
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
income_table  = income_per_person %>% gather(key = 'year', value = 'income_per_person', -country)
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

#Function to perform Min max normalize data
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

# Rule Learning

