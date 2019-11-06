prob814_m1 = c(30.5, 32.2, 20.7, 20.6, 31.0, 41.0, 27.7, 26.0, 21.5, 26.0)
prob814_m2 = c(22.4, 14.5, 22.4, 19.6, 20.7, 20.4, 22.1, 19.4, 16.2, 35.0)

diffs = prob814_m1 - prob814_m2
mean_diffs = mean(diffs)
sd_diffs = sd(diffs)
n = 10

t_score = mean_diffs / (sd_diffs / sqrt(n))

t.test(prob814_m1, prob814_m2, paired=TRUE)
