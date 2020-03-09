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

get_params <- function(database, name=NULL) {
    x_mode = mode(database)
    x_median = median(database)
    x_mean = mean(database)
    x_sd = sd(database)
    c(x_mode, x_median, x_mean, x_sd)
}

calculate_params <- function(database, title, view=FALSE) {
    age = database$age
    overall = database$overall
    potential = database$potential

    table = do.call(rbind, Map(data.frame, Edad=age, Valoración=overall, Potencial=potential))
    table1 = data.frame(cor(table))

    age_params = get_params(age)
    overall_params = get_params(overall)
    potential_params = get_params(potential)

    names = c('Moda', 'Mediana', 'Media', 'Desviacion Estandar')
    table2 = do.call(rbind, Map(data.frame, Medidas=names, Edad=age_params, Valoración=overall_params, Potencial=potential_params))

    names = c('Edad Correlación', 'Valoración Correlación', 'Potencial Correlación')
    table3 = do.call(rbind, Map(data.frame, Medidas=names, Edad=table1$Edad, Valoración=table1$Valoración, Potencial=table1$Potencial))
    result = rbind(table2, table3)[,2:4]

    if (view) {
        View(result, title=title)
    }

    return(result)
}

view = FALSE

database <- read.csv(file = 'database.csv')

# 1

age = database$age
overall = database$overall
potential = database$potential

# 1a

age_params = get_params(age, 'Edad')
overall_params = get_params(overall, 'Valoración')
potential_params = get_params(potential, 'Potencial')

# 1b

boxplot(x=age, xlab='Edad')
boxplot(x=overall, xlab='Valoración')
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

# 2a

params100r = calculate_params(database100r, 'database100r', view=view)
params50r = calculate_params(database50r, 'database50r', view=view)
params30r = calculate_params(database30r, 'database30r', view=view)
params20r = calculate_params(database20r, 'database20r', view=view)
params100 = calculate_params(database100, 'database100', view=view)
params50 = calculate_params(database50, 'database50', view=view)
params30 = calculate_params(database30, 'database30', view=view)
params20 = calculate_params(database20, 'database20', view=view)

# 2b

params500 = calculate_params(database500, 'database500', view=view)

# 2c

tableAge = do.call(rbind, Map(data.frame, database500=database500$age, database100r=database100r$age, database100=database100$age, database50r=database50r$age, database50=database50$age, database30r=database30r$age, database30=database30$age, database20r=database20r$age, database20=database20$age))
tableOverall = do.call(rbind, Map(data.frame, database500=database500$overall, database100r=database100r$overall, database100=database100$overall, database50r=database50r$overall, database50=database50$overall, database30r=database30r$overall, database30=database30$overall, database20r=database20r$overall, database20=database20$overall))
tablePotential = do.call(rbind, Map(data.frame, database500=database500$potential, database100r=database100r$potential, database100=database100$potential, database50r=database50r$potential, database50=database50$potential, database30r=database30r$potential, database30=database30$potential, database20r=database20r$potential, database20=database20$potential))

boxplot(x=tableAge, xlab='Edad', names=c('Muestra 500', 'Muestra 100 (remp)', 'Muestra 100', 'Muestra 50 (remp)', 'Muestra 50', 'Muestra 30 (remp)', 'Muestra 30', 'Muestra 20 (remp)', 'Muestra 20'))
boxplot(x=tableOverall, xlab='Valoración', names=c('Muestra 500', 'Muestra 100 (remp)', 'Muestra 100', 'Muestra 50 (remp)', 'Muestra 50', 'Muestra 30 (remp)', 'Muestra 30', 'Muestra 20 (remp)', 'Muestra 20'))
boxplot(x=tablePotential, xlab='Potencial', names=c('Muestra 500', 'Muestra 100 (remp)', 'Muestra 100', 'Muestra 50 (remp)', 'Muestra 50', 'Muestra 30 (remp)', 'Muestra 30', 'Muestra 20 (remp)', 'Muestra 20'))

# 2d

age100r = database100r$age
age100 = database100$age
age50r = database50r$age
age50 = database50$age
age30r = database30r$age
age30 = database30$age
age20r = database20r$age
age20 = database20$age

overall100r = database100r$overall
overall100 = database100$overall
overall50r = database50r$overall
overall50 = database50$overall
overall30r = database30r$overall
overall30 = database30$overall
overall20r = database20r$overall
overall20 = database20$overall

potential100r = database100r$potential
potential100 = database100$potential
potential50r = database50r$potential
potential50 = database50$potential
potential30r = database30r$potential
potential30 = database30$potential
potential20r = database20r$potential
potential20 = database20$potential

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
