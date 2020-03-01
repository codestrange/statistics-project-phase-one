mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_sample <- function(database, n, replace=FALSE) {
    database[sample(1:length(database), n, replace = replace),]
}

get_params <- function(database, name) {
    x_mode = mode(age)
    x_median = median(age)
    x_mean = mean(age)
    x_sd = sd(age)

    print(paste('Moda -', name))
    print(x_mode)
    print(paste('Mediana -', name))
    print(x_median)
    print(paste('Media -', name))
    print(x_mean)
    print(paste('Desviación estandar -', name))
    print(x_sd)

    c(x_mode, x_median, x_mean, x_sd)
}

database <- read.csv(file = 'database.csv')

age = database[, 5]
overall = database[, 11]
potential = database[, 12]

# 1a

age_params = get_params(age, 'Edad')
overall_params = get_params(overall, 'Valoración General')
potential_params = get_params(potential, 'Potencial')

# 1b

boxplot(x=age, xlab='Edad')
boxplot(x=overall, xlab='Valoración General')
boxplot(x=potential, xlab='Potencial')

