
#3.3a
prob33 = c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70)
bins = c()
print(bins)
for (i in c(0:((length(prob33)/3)-1))) {
  print(i)
  tmp_mean = mean(prob33[(i*3+1):((i*3)+3)])
  bins = c(bins, tmp_mean, tmp_mean, tmp_mean)
}
print(bins)

#3.6

prob3.6 = c(200, 300, 400, 600, 1000)

# (a) min-max normalization by setting min = 0 and max = 1

# write a function which does the min-max normalization
# source: https://stackoverflow.com/questions/44050028/min-max-scaling-normalization-in-r-for-train-and-test-data#44050950
normalize = function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

prob3.6_normalized =  normalize(prob3.6)
prob3.6_normalized

# (b) z-score normalization
# source https://stackoverflow.com/questions/6263400/can-i-calculate-z-score-with-r
z_score =  function(x)
{
  (x-mean(x))/sd(x)
}

prob3.6_z_score = z_score(prob3.6)
prob3.6_z_score

# (c) z-score normalization using the mean absolute deviation

# install DescTools to get the Mean Absolute Deviation function
install.packages('DescTools')
library('DescTools')

z_score_Mean_AD =  function(x)
{
  (x-mean(x))/MeanAD(x, FUN = mean, na.rm = FALSE)
}

prob3.6_z_score_Mean_AD = z_score_Mean_AD(prob3.6)
prob3.6_z_score_Mean_AD

# (d) normalization by decimal scaling

decimal_scale = function(x,j)
{
  (x/10^(j))
}

prob3.6_decimal_scale = decimal_scale(prob3.6, 4)
prob3.6_decimal_scale

#3.8a
ages = c(23, 23, 27, 27, 39, 41, 47, 49, 50, 52, 54, 54, 56, 57, 58, 58, 60, 61)
fats = c(9.5, 26.5, 7.8, 17.8, 31.4, 25.9, 27.4, 27.2, 31.2, 34.6, 42.5, 28.8, 33.4, 30.2, 34.1, 32.9, 41.2, 35.7)

my_znorm = function(x) {
  return ((x - mean(x)) / sd(x))
}

norm_ages = my_znorm(ages)
norm_fats = my_znorm(fats)
norm_ages
norm_fats

#3.8
corr_coef = cor(ages, fats)
covar = cov(ages, fats)
corr_coef
covar

#3.11a
prob311 = prob33
minval = min(prob311)

breakpoints = seq(0, 100, by=10)
hist(prob311, breakpoints, TRUE)

