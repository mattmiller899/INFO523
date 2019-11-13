(state_table <- data.frame(key=c("CA", "NY", "WA", "ON", "QU"),
                           name=c("California", "new York", "Washington", "Ontario", "Quebec"),
                           country=c("USA", "USA", "USA", "Canada", "Canada")))
(month_table <- data.frame(key=1:12,
                          desc=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                          quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4")))
(prod_table <- data.frame(key=c("Printer", "Tablet", "Laptop"),
                          price=c(225, 570, 1120)))

gen_sales <- function(no_of_recs) {
  loc <- sample(state_table$key, no_of_recs, 
                replace=T, prob=c(2,2,1,1,1))
  time_month <- sample(month_table$key, no_of_recs, replace=T)
  time_year <- sample(c(2012, 2013), no_of_recs, replace=T)
  prod <- sample(prod_table$key, no_of_recs, replace=T, prob=c(1, 3, 2))
  unit <- sample(c(1,2), no_of_recs, replace=T, prob=c(10, 3)) #if the chance for '1' to be sampled is 10, then the chance for '2' to be sampled is 3.
  amount <- unit*prod_table[prod,]$price
  sales <- data.frame(month=time_month,
                      year=time_year,
                      loc=loc,
                      prod=prod,
                      unit=unit,
                      amount=amount)
        # Sort the records by time order
  sales <- sales[order(sales$year, sales$month),]
  row.names(sales) <- NULL
  return(sales)
}

(sales_fact <- gen_sales(500))
View(sales_fact)
head(sales_fact)

(revenue_cube <- tapply(sales_fact$amount, 
                        sales_fact[,c("prod", "month", "year", "loc")],
                        FUN=function(x){return(sum(x))}))

#Slice
revenue_cube[, , , "CA"]

#Dice
revenue_cube[c("Laptop", "Tablet"), c("1", "2", "3"), , c("CA", "NY")]

#Rollup
apply(revenue_cube, c("year", "prod"), FUN=function(x) sum(x, na.rm=TRUE))

#Drilldown
apply(revenue_cube, c("year", "month", "prod"), FUN=function(x) sum(x, na.rm=TRUE))

#Pivot
apply(revenue_cube, c("year", "month"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("month", "year"), FUN=function(x) sum(x, na.rm=TRUE))

#rollup and dice
(rollup <-apply(revenue_cube, c("loc", "prod", "year"), FUN=function(x) sum(x, na.rm=TRUE)))
dimnames(rollup)

#Dice loc and year
(rollup["CA",, "2012"])

#Compare and contrast
revenue_cube[, ,"2012" , "CA"]

#Apex cuboid
sum(apply(revenue_cube, c("year"), FUN=function(x) sum(x, na.rm=TRUE)))

#1D cuboids
apply(revenue_cube, c("year"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("prod"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("month"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("loc"), FUN=function(x) sum(x, na.rm=TRUE))

#2d cuboids
apply(revenue_cube, c("year", "prod"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("year", "month"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("year", "loc"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("prod", "month"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("prod", "loc"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("month", "loc"), FUN=function(x) sum(x, na.rm=TRUE))

#3d cuboids (HW ANSWERS HERE)
apply(revenue_cube, c("year", "prod", "month"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("year", "prod", "loc"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("year", "month", "loc"), FUN=function(x) sum(x, na.rm=TRUE))
apply(revenue_cube, c("month", "prod", "loc"), FUN=function(x) sum(x, na.rm=TRUE))


