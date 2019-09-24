nstall.packages("adephylo")
library("adephylo")
data(carni70)
carni70$tab

carni = carni70$tab
mean(carni$size)
median(carni$size)
var(carni$size)
sd(carni$size)
range(carni$size)
max(carni$size)
min(carni$size)
IQR(carni$size)
quantile(carni$size)

nas = apply(carni, 1, function(x) sum(is.na(x)))
nas

cat("dataset contains ", sum(nas), " NA values.\n")
cat("dataset contains ", sum(!complete.cases(carni)), " out of ", nrow(carni), "incomplete rows. \n")
summary(carni)

library("dplyr")

aggregate(x=carni, by=list(Range=carni$range), FUN="quantile")
aggregate(x=carni[-5], by=list(Range=carni$range), FUN="quantile")


pdf("myplot.pdf")
plot(sin(seq(0,10, by=0.1)), type="l")
dev.off()

library(ggplot2)

freqOcc = table(carni$size)
barplot(freqOcc, main = "Frequency of sizes")

ggplot(carni, aes(x=size)) +
  geom_bar() +
  ggtitle("Frequency of sizes")
ggplot()


theme_update(plot.title=element_text(hjust=0.5))
ggplot(carni, aes(x=size)) +
  geom_bar() +
  ggtitle("Frequency of sizes")
