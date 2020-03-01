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

print('Moda de la Edad')
print(age_mode)
print('Mediana de la Edad')
print(age_median)
print('Media de la Edad')
print(age_mean)
print('Desviación estandar de la Edad')
print(age_sd)

overall_mode = mode(overall)
overall_median = median(overall)
overall_mean = mean(overall)
overall_sd = sd(overall)

potential_mode = mode(potential)
potential_median = median(potential)
potential_mean = mean(potential)
potential_sd = sd(potential)
