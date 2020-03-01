mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

data <- read.csv(file = 'database.csv')

age = data[, 5]
overall = data[, 11]
potential = data[, 12]

age_mode = mode(age)
age_median = median(age)
age_mean = mean(age)
age_sd = sd(age)

overall_mode = mode(overall)
overall_median = median(overall)
overall_mean = mean(overall)
overall_sd = sd(overall)
