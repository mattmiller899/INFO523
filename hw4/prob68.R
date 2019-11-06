install.packages("arules")
library("arules")

a_list <- list(
  c("a","b","c"),
  c("a","b"),
  c("a","b","d"),
  c("c","e"),
  c("a","b","d","e")
)
## set transaction names
names(a_list) <- paste("Tr",c(1:5), sep = "")
a_list
## coerce into transactions
trans1 <- as(a_list, "transactions")
## analyze transactions
trans1
summary(trans1)

category_list = list(
  c(c("Crab"), c("Milk"), c("Cheese"), c("Bread")),
  c(c("Cheese"), c("Milk"), c("Apple"), c("Pie"), c("Bread")),
  c(c("Apple"), c("Milk"), c("Bread"), c("Pie")),
  c(c("Bread"), c("Milk"), c("Cheese"))
)

brand_list = list(
  c(c("King's-Crab"), c("Sunset-Milk"), c("Dairyland-Cheese"), c("Best-Bread"), c("Westcoast-Apple"), c("Dairyland-Milk"), c("Wonder-Bread"), c("Tasty-Pie")),
  c(c("Best-Cheese"), c("Dairyland-Milk"), c("Goldenfarm-Apple"), c("Tasty-Pie"), c("Wonder-Bread")),
  c(c("Wonder-Bread"), c("Sunset-Milk"), c("Dairyland-Cheese"))
)
#names(brand_list) = c("1", "2", "1", "3")
names(brand_list) = c("1", "2", "3")
names(category_list) = c("T100", "T200", "T300", "T400")

brand_trans = as(brand_list, "transactions")
cat_trans = as(category_list, "transactions")

freqsets_b = apriori(brand_trans, parameter=list(target="frequent itemsets", support=0.6, confidence=0.8))
apri_c = apriori(cat_trans, parameter=list(support=0.6, confidence=0.8))

inspect(subset(apri_c, subset=is.maximal(apri_c)))
inspect(subset(freqsets_b, subset=is.maximal(freqsets_b)))
