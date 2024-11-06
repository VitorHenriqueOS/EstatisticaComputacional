media <- mean(sample(1:6,500,replace = TRUE))

resultados <- c()
for (i in 1:1000) {
  resultados[i] <- mean(sample(1:6,500,replace = TRUE))
}
hist(resultados)

dados <- data.frame(resultados)

dados |>
  ggplot(aes(x = resultados)) +
  geom_density(fill = "lightblue")+
  theme_minimal()

#-------------------------------------------------------------------------------

populacao <- sample(0:1, 100000, replace = TRUE, prob = c(0.9,0.1))
valor_real <- mean(populacao)
amostra <- sample(populacao, 500)
media <- mean(amostra)
media

parte_inferior <- media - 1.96* (sqrt(media * (1-media)) / sqrt(500))
parte_superior <- media + 1.96* (sqrt(media * (1-media)) / sqrt(500))

inferiores <- c()
superiores <- c()

for (k in 1:100){
  amostra <- sample(populacao, 500)
  media <- mean(amostra)
  inferiores[k] <- media - 1.96* (sqrt(media * (1-media)) / sqrt(500))
  superiores[k] <- media + 1.96* (sqrt(media * (1-media)) / sqrt(500))
}

intervalos <- data.frame(inferiores, superiores, contador = 1:100)

intervalos |>
  ggplot()+
  geom_segment(aes(x = inferiores, xend = superiores, y = contador, yend = contador)) +
  geom_vline(xintercept = valor_real, col = "red") +
  theme_minimal()

categoria <- ifelse(intervalos$inferiores > valor_real | intervalos$superiores < valor_real, 0, 1)

intervalos$categoria <- as.factor(categoria)

intervalos |>
  ggplot()+
  geom_segment(aes(x = inferiores, xend = superiores, y = contador, yend = contador, col = categoria)) +
  geom_vline(xintercept = valor_real, col = "red") +
  theme_minimal()

