# Rodrigo Ribeiro da Silva
# TIA: 71622853
# Data: 18/02/2017

valores = c(28, 33, 27, 30, 31, 30, 33, 30, 33, 29, 27, 33, 31, 27, 31, 28, 27, 29, 31, 24, 31, 33, 30, 32, 30, 33, 27, 33, 31, 33, 23, 29, 30, 24, 28, 34, 30, 30, 18, 17, 18, 15, 16, 17, 17, 18, 19, 19, 20, 29)

# a) Construir a tabela com a distribuição de frequência;
# b) Calcular a média;
# c) Moda;
# d) Mediana;
# e) O coeficiente de variação;
# f) Determinar a curtose.

### Respostas
# a)
ordenados <- sort(valores)
ordenados.freq <- table(ordenados)
ordenados.freq
# R:
# 15 16 17 18 19 20 23 24 27 28 29 30 31 32 33 34 
#  1  1  3  3  2  1  1  2  5  3  4  8  6  1  8  1

# b)
media <- mean(ordenados)
media
# R: [1] 27.12

# c)
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode <- getmode(ordenados)
mode
# R: [1] 30

# d)
mediana <- median(ordenados)
mediana
# R: [1] 29

# e)
desviopadrao <- sd(ordenados)
desviopadrao
coeficientevariacao <- (desviopadrao/media)
coeficientevariacao
# R: [1] 0.2085382

# f)
ordenados2=ordenados[2:(length(ordenados)-1)]
trimedia <- mean(ordenados2)
trimedia
q1 <- round((length(ordenados)+1)/4)
q1
q3 <- round(3*((length(ordenados)+1)/4))
q3
percent90 <- quantile(ordenados, c(.90))
percent90
percent10 <- quantile(ordenados, c(.10))
percent10
# Assimetria
assimetria <- (1/(length(ordenados))) * (sum((ordenados-trimedia)^3))/(4.32^3)
assimetria
# [1] -1.979038
# Coeficiente percentilico de curtose
k <- 0.263-(((q3-q1)/2)/(percent90-percent10))
k
# -0.5648146
# Coeficiente momento de curtose
curtose <- (1/(length(ordenados))) * (sum((ordenados-trimedia)^4))/(4.32^4) - 3
curtose
# R: [1] 3.806775
# Leptocúrtica

# Um candidato, com pouco estudo, respondeu a 2 anúncios de
# oferta de emprego. As empresas são do mesmo ramo, ou seja,
# o mesmo serviço em ambas. O candidado obteve a seguinte
# informação sobre os salários das duas empresas:
# 					Empresa A	Empresa B
# Media				445			475
# Mediana			400			350
# Desvio padrao		160			190
#
# Qual das empresas vc aconselharia o candidado a escolher?
# Explique porquê.
# 
# R: Empresa A pois:
# Um baixo desvio padrão indica que os dados tendem a estar próximos da média;
# Um desvio padrão alto indica que os dados estão espalhados por uma gama de valores.
