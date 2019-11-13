install.packages('dplyr')
library('dplyr')
install.packages('tidyverse')
library('tidyverse')
install.packages("arules")
library("arules")

# Set the working directory to this script's directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Import Datasets
# by Indicator name from https://www.gapminder.org/data/

# Adults with HIV (%, age 15-49)
adults_hiv = read.csv("data/Adults_15-49_HIV.csv", header = TRUE, check.names = FALSE, encoding="UTF-8-BOM")

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

#Descretize data

discr = function(x) cut(x, breaks=quantile(x, na.rm=TRUE), labels=c("low", "medLow", "medHigh", "high"))
filtered_total_data = total_data %>% filter(!is.na(adults_hiv) & !is.na(underweight_children) & !is.na(female_40_59_pop_percent) & !is.na(gdp_total_ppp))
#Not including Year because it clogs stuff up
disc_total_data = select(filtered_total_data, -c("year", "country"))  %>% mutate_all(funs(discr)) %>% bind_cols(select(filtered_total_data, c("country"))) 
summary(disc_total_data)


trans_total_data = as(disc_total_data, "transactions")
inspect(trans_total_data[1:4])
summary(trans_total_data)
itemFrequencyPlot(trans_total_data, support=0.2, cex.names=0.5)
colnames(trans_total_data)
summary(trans_total_data)
apr = apriori(trans_total_data, parameter=list(support=0.1, confidence=0.4))
inspect(apr)
inspect(subset(apr, subset=rhs %in% "adults_hiv=high"))
inspect(subset(apr, subset=is.maximal(apr)))
freq_itemsets = apriori(trans_total_data, parameter=list(target="frequent itemsets", support=0.2, confidence=0.5))
inspect(freq_itemsets)
closed = freq_itemsets[is.closed(freq_itemsets)]
inspect(closed)
summary(closed)
maximal = freq_itemsets[is.maximal(freq_itemsets)]
inspect(maximal)
summary(maximal)


#females + gdp + underweight
fgu_data = full_join(female_table, underweight_table, by = c('country','year'))
fgu_data = full_join(fgu_data, gdp_table, by = c('country', 'year'))
filtered_fgu_data = fgu_data %>% filter(!is.na(underweight_children) & !is.na(female_40_59_pop_percent) & !is.na(gdp_total_ppp))
#Not including Year because it clogs stuff up
disc_fgu_data = select(filtered_fgu_data, -c("year", "country"))  %>% mutate_all(funs(discr)) %>% bind_cols(select(filtered_fgu_data, c("country"))) 
summary(disc_fgu_data)
trans_fgu_data = as(disc_fgu_data, "transactions")
fgu_apr = apriori(trans_fgu_data, parameter=list(support=0.1, confidence=0.4))
inspect(fgu_apr)
