mode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

var_interval = function(data, conf.level = 0.95) {
    df = length(data) - 1
    chilower = qchisq((1 - conf.level)/2, df)
    chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
    v = var(data)
    c(df * v/chiupper, df * v/chilower)
}

get_sample <- function(database, n, replace=FALSE) {
    database[sample(1:length(database[,1]), n, replace = replace),]
}

get_params <- function(database, name) {
    x_mode = mode(database)
    x_median = median(database)
    x_mean = mean(database)
    x_sd = sd(database)

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

age = database$age
overall = database$overall
potential = database$potential

# 1a

age_params = get_params(age, 'Edad')
overall_params = get_params(overall, 'Valoración General')
potential_params = get_params(potential, 'Potencial')

# 1b

boxplot(x=age, xlab='Edad')
boxplot(x=overall, xlab='Valoración General')
boxplot(x=potential, xlab='Potencial')

# 2

database500 = get_sample(database, 500)

database100r = get_sample(database500, 100, TRUE)
database50r = get_sample(database500, 50, TRUE)
database30r = get_sample(database500, 30, TRUE)
database20r = get_sample(database500, 20, TRUE)

database100 = get_sample(database500, 100)
database50 = get_sample(database500, 50)
database30 = get_sample(database500, 30)
database20 = get_sample(database500, 20)

# 2a 2b

age500 = database500$age
overall500 = database500$overall
potential500 = database500$potential

age500_params = get_params(age500, 'Edad')
overall500_params = get_params(overall500, 'Valoración General')
potential500_params = get_params(potential500, 'Potencial')

age100r = database100r$age
overall100r = database100r$overall
potential100r = database100r$potential

age100r_params = get_params(age100r, 'Edad')
overall100r_params = get_params(overall100r, 'Valoración General')
potential100r_params = get_params(potential100r, 'Potencial')

age50r = database50r$age
overall50r = database50r$overall
potential50r = database50r$potential

age50r_params = get_params(age50r, 'Edad')
overall50r_params = get_params(overall50r, 'Valoración General')
potential50r_params = get_params(potential50r, 'Potencial')

age30r = database30r$age
overall30r = database30r$overall
potential30r = database30r$potential

age30r_params = get_params(age30r, 'Edad')
overall30r_params = get_params(overall30r, 'Valoración General')
potential30r_params = get_params(potential30r, 'Potencial')

age20r = database20r$age
overall20r = database20r$overall
potential20r = database20r$potential

age20r_params = get_params(age20r, 'Edad')
overall20r_params = get_params(overall20r, 'Valoración General')
potential20r_params = get_params(potential20r, 'Potencial')

age100 = database100$age
overall100 = database100$overall
potential100 = database100$potential

age100_params = get_params(age100, 'Edad')
overall100_params = get_params(overall100, 'Valoración General')
potential100_params = get_params(potential100, 'Potencial')

age50 = database50$age
overall50 = database50$overall
potential50 = database50$potential

age50_params = get_params(age50, 'Edad')
overall50_params = get_params(overall50, 'Valoración General')
potential50_params = get_params(potential50, 'Potencial')

age30 = database30$age
overall30 = database30$overall
potential30 = database30$potential

age30_params = get_params(age30, 'Edad')
overall30_params = get_params(overall30, 'Valoración General')
potential30_params = get_params(potential30, 'Potencial')

age20 = database20$age
overall20 = database20$overall
potential20 = database20$potential

age20_params = get_params(age20, 'Edad')
overall20_params = get_params(overall20, 'Valoración General')
potential20_params = get_params(potential20, 'Potencial')

# 2c

boxplot(x=age500, xlab='Edad Población de 500')
boxplot(x=overall500, xlab='Valoración General Población de 500')
boxplot(x=potential500, xlab='Potencial Población de 500')

boxplot(x=age100r, xlab='Edad Muestra de 100 con remplazo')
boxplot(x=overall100r, xlab='Valoración General Muestra de 100 con remplazo')
boxplot(x=potential100r, xlab='Potencial Muestra de 100 con remplazo')

boxplot(x=age50r, xlab='Edad Muestra de 50 con remplazo')
boxplot(x=overall50r, xlab='Valoración General Muestra de 50 con remplazo')
boxplot(x=potential50r, xlab='Potencial Muestra de 50 con remplazo')

boxplot(x=age30r, xlab='Edad Muestra de 30 con remplazo')
boxplot(x=overall30r, xlab='Valoración General Muestra de 30 con remplazo')
boxplot(x=potential30r, xlab='Potencial Muestra de 30 con remplazo')

boxplot(x=age20r, xlab='Edad Muestra de 20 con remplazo')
boxplot(x=overall20r, xlab='Valoración General Muestra de 20 con remplazo')
boxplot(x=potential20r, xlab='Potencial Muestra de 20 con remplazo')

boxplot(x=age100, xlab='Edad Muestra de 100 sin remplazo')
boxplot(x=overall100, xlab='Valoración General Muestra de 100 sin remplazo')
boxplot(x=potential100, xlab='Potencial Muestra de 100 sin remplazo')

boxplot(x=age50, xlab='Edad Muestra de 50 sin remplazo')
boxplot(x=overall50, xlab='Valoración General Muestra de 50 sin remplazo')
boxplot(x=potential50, xlab='Potencial Muestra de 50 sin remplazo')

boxplot(x=age30, xlab='Edad Muestra de 30 sin remplazo')
boxplot(x=overall30, xlab='Valoración General Muestra de 30 sin remplazo')
boxplot(x=potential30, xlab='Potencial Muestra de 30 sin remplazo')

boxplot(x=age20, xlab='Edad Muestra de 20 sin remplazo')
boxplot(x=overall20, xlab='Valoración General Muestra de 20 sin remplazo')
boxplot(x=potential20, xlab='Potencial Muestra de 20 sin remplazo')

# 2d

print('Intervalo de confianza para la varianza de la Edad de la muestra de 100 con remplazo')
print(var_interval(age100r))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 100 sin remplazo')
print(var_interval(age100))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 50 con remplazo')
print(var_interval(age50r))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 50 sin remplazo')
print(var_interval(age50))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 30 con remplazo')
print(var_interval(age30r))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 30 sin remplazo')
print(var_interval(age30))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 20 con remplazo')
print(var_interval(age20r))

print('Intervalo de confianza para la varianza de la Edad de la muestra de 20 sin remplazo')
print(var_interval(age20))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 100 con remplazo')
print(var_interval(overall100r))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 100 sin remplazo')
print(var_interval(overall100))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 50 con remplazo')
print(var_interval(overall50r))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 50 sin remplazo')
print(var_interval(overall50))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 30 con remplazo')
print(var_interval(overall30r))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 30 sin remplazo')
print(var_interval(overall30))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 20 con remplazo')
print(var_interval(overall20r))

print('Intervalo de confianza para la varianza de la Valoración General de la muestra de 20 sin remplazo')
print(var_interval(overall20))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 100 con remplazo')
print(var_interval(potential100r))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 100 sin remplazo')
print(var_interval(potential100))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 50 con remplazo')
print(var_interval(potential50r))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 50 sin remplazo')
print(var_interval(potential50))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 30 con remplazo')
print(var_interval(potential30r))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 30 sin remplazo')
print(var_interval(potential30))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 20 con remplazo')
print(var_interval(potential20r))

print('Intervalo de confianza para la varianza del Potencial de la muestra de 20 sin remplazo')
print(var_interval(potential20))

print('Intervalo de confianza para la media de la Edad de la muestra de 100 con remplazo')
print(t.test(age100r)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 100 sin remplazo')
print(t.test(age100)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 50 con remplazo')
print(t.test(age50r)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 50 sin remplazo')
print(t.test(age50)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 30 con remplazo')
print(t.test(age30r)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 30 sin remplazo')
print(t.test(age30)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 20 con remplazo')
print(t.test(age20r)$conf.int)

print('Intervalo de confianza para la media de la Edad de la muestra de 20 sin remplazo')
print(t.test(age20)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 100 con remplazo')
print(t.test(overall100r)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 100 sin remplazo')
print(t.test(overall100)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 50 con remplazo')
print(t.test(overall50r)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 50 sin remplazo')
print(t.test(overall50)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 30 con remplazo')
print(t.test(overall30r)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 30 sin remplazo')
print(t.test(overall30)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 20 con remplazo')
print(t.test(overall20r)$conf.int)

print('Intervalo de confianza para la media de la Valoración General de la muestra de 20 sin remplazo')
print(t.test(overall20)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 100 con remplazo')
print(t.test(potential100r)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 100 sin remplazo')
print(t.test(potential100)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 50 con remplazo')
print(t.test(potential50r)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 50 sin remplazo')
print(t.test(potential50)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 30 con remplazo')
print(t.test(potential30r)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 30 sin remplazo')
print(t.test(potential30)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 20 con remplazo')
print(t.test(potential20r)$conf.int)

print('Intervalo de confianza para la media del Potencial de la muestra de 20 sin remplazo')
print(t.test(potential20)$conf.int)

# 3

databaseFCB = database[database$club == 'FC Barcelona', ]

databaseRM = database[database$club == 'Real Madrid', ]

print('Prueba de Hipótesis de la varianza del potencial')
print(var.test(databaseFCB$potential, databaseRM$potential))

print('Prueba de Hipótesis de la media del potencial')
print(t.test(databaseFCB$potential, databaseRM$potential, alternative='greater', var.equal=TRUE))
