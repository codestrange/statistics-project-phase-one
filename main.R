mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

data <- read.csv(file = 'database.csv')

age = data[, 5]
overall = data[, 11]
potential = data[, 12]
