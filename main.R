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

print('Moda de la Valoración General')
print(overall_mode)
print('Mediana de la Valoración General')
print(overall_median)
print('Media de la Valoración General')
print(overall_mean)
print('Desviación estandar de la Valoración General')
print(overall_sd)

potential_mode = mode(potential)
potential_median = median(potential)
potential_mean = mean(potential)
potential_sd = sd(potential)

print('Moda del Potencial')
print(potential_mode)
print('Mediana del Potencial')
print(potential_median)
print('Media del Potencial')
print(potential_mean)
print('Desviación estandar del Potencial')
print(potential_sd)

print('Se selecionaron la edad, la valoración general y el potencial, porque consideramos que son las principales características por las cual un jugador puede ser evaluado.')
