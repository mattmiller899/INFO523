install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')

# Set the working directory to this script's directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Import Datasets
# by Indicator name from https://www.gapminder.org/data/

# Adults with HIV (%, age 15-49)
adults_hiv = read.csv("data/Adults_15-49_HIV.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

# Underweight children (added the fileEncoding, think its a Windows issue)
underweight_children = read.csv("data/Underweight_Children.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")


# Total GDP (PPP$, inflation-adjusted)
gdp_total_ppp = read.csv("data/gdp_total_ppp.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

# Population aged 40-59 years, female (%)
female_40_59_pop_percent = read.csv("data/female_40_59_pop_percent.csv", header = TRUE, check.names = FALSE, fileEncoding="UTF-8-BOM")

hiv_table = adults_hiv %>% gather(key = 'year', value = 'adults_hiv', -country)
underweight_table = underweight_children %>% gather(key = 'year', value = 'underweight_children', -country)
gdp_table = gdp_total_ppp %>% gather(key = 'year', value = 'gdp_total_ppp', -country)
female_table = female_40_59_pop_percent %>% gather(key = 'year', value = 'female_40_59_pop_percent', -country)

total_data = full_join(hiv_table, underweight_table, by = c('country','year'))
total_data = full_join(gdp_table, total_data, by = c('country','year'))
total_data = full_join(female_table, total_data, by = c('country','year'))

View(total_data)


