valores = c(85.3, 84.3, 79.5, 82.5, 80.2, 84.6, 79.2, 70.9, 78.6, 86.2, 74.0, 83.7)
valores
# calculo da media utilizando a função 'mean'
media <- mean(valores)
media
# calculo da mediana utilizando a função 'median'
mediana <- median(valores)
mediana
# calculo do desvio padrão utilizando a função 'sd'
desviopadrao <- sd(valores)
desviopadrao

### Agora calculos manuais

# calculo da média
# primeiro, determinando "n", número de elementos no vetor
n = length(valores)
media <- sum(valores)/n
media

# calculo da mediana
ordenados <- sort(valores)
ordenados
n
# como "n" é par pegamos os dois valores centrais
v1 = ordenados[n/2]
v2 = ordenados[n/2 + 1]
mediana = (v1 + v2)/2
mediana
# calculo do desvio padrão manaualmente
# primeira etapa: calculo das diferenças
diferencas = media - valores
diferencas
# terceira etapa: quadrado das diferenças
qddiff = diferencas^2
qddiff
# quarta etapa: somar os quadrados e dividir por n - 1
somasq = sum(qddiff)/(n - 1)
somasq
# tirar a raiz quadrada
desviopadrao = sqrt(somasq)
desviopadrao
