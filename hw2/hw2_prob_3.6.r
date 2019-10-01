# Problem 3.6

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
