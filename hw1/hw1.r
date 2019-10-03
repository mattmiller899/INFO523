#HW 1

prob22 = c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)

#2.2 a
mean(prob22)
median(prob22)

#2.2 b
Mode = function(x, na.rm=FALSE){
  if(na.rm) x=x[!is.na(x)]
  ux = unique (x)
  print(ux)
  return (ux[which.max(tabulate(match(x, ux)))])
}
Mode(prob22)

#2.2 c
mean(c(max(prob22), min(prob22)))

#2.2 d
quantile(prob22)

#2.2 e
summary(prob22)

#2.2 f
boxplot(prob22)

#2.2 g
prob22q1 = quantile(prob22)[2]
qqplot(prob22)

#2.3
freqs = c(200, 450, 300, 1500, 700, 44)
freq_range = c(1:sum(freqs))
freq_range_median = median(freq_range)
get_median_interval = function(x, median) {
  count = 1
  cum_sum = 0
  median_interval = 0
  while (cum_sum < median) {
    median_interval = freqs[count]
    cum_sum = cum_sum + freqs[count]
    count = count + 1
  }
  return (c(median_interval, count))
}

v = get_median_interval(freqs, freq_range_median)
freq_median_interval = v[1]
ind = v[2] - 1
l1 = 20
n = sum(freqs)
lower_freq_sum = sum(freqs[1:ind-1])
width = 50-20
med = l1 + ((n/2 - lower_freq_sum)/freq_median_interval) * width
print(med)

#2.4 a
ages = c(23, 23, 27, 27, 39, 41, 47, 49, 50, 52, 54, 54, 56, 57, 58, 58, 60, 61)
fats = c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4, 27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2, 34.1, 32.9, 41.2, 35.7)
age_mean = mean(ages)
age_median = median(ages)
age_sd = sd(ages)
fat_mean = mean(fats)
fat_median = median(fats)
fat_sd = sd(fats)

#2.4 b
boxplot(ages, main="Ages boxplot")
boxplot(fats, main="% fat boxplot")

#2.4 c
plot(ages, fats, xlab="ages", ylab="% fat", main="Age vs % fat scatter plot")
qqplot(ages, fats, main = "Age vs % fat Q-Q plot")
qqline(fats)

#2.8 a
install.packages("ecodist")
library("ecodist")
install.packages("lsa")
library("lsa")
a1 = c(1.5, 2, 1.6, 1.2, 1.5)
a2 = c(1.7, 1.9, 1.8, 1.5, 1.0)
df = data.frame(a1, a2)
x = data.frame(1.4, 1.6)

euc_dists = c()
man_dists = c()
sup_dists = c()
cos_dists = c()

for (row in 1:nrow(df)) {
  samp = df[row, 1:2]
  print(samp)
  xs = c(samp[1,1], x[1,1])
  ys = c(samp[1,2], x[1,2])
  tmp_df = data.frame(xs, ys)
  euc_dists = append(euc_dists, distance(tmp_df, "euclidean"))
  man_dists = append(man_dists, distance(tmp_df, "manhattan"))
  cos_dists = append(cos_dists, cosine(as.numeric(x), as.numeric(samp)))
  sup_dists = append(sup_dists, dist(tmp_df, "maximum"))
}
print(euc_dists)
print(man_dists)
print(sup_dists)
print(cos_dists)

#2.8 b
normalize <- function(x) {
  norm_val = sqrt(x[1,1]^2 + x[1,2]^2)
  return (x/norm_val)
}

norm_x = normalize(x)
norm_euc_dists = c()

for (row in 1:nrow(df)) {
  norm_samp = normalize(df[row, 1:2])
  xs = c(norm_samp[1,1], norm_x[1,1])
  ys = c(norm_samp[1,2], norm_x[1,2])
  tmp_df = data.frame(xs, ys)
  norm_euc_dists = append(norm_euc_dists, distance(tmp_df, "euclidean"))
}
print(norm_euc_dists)
