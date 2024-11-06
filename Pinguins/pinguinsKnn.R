data(penguins)
pinguins <- penguins
str(pinguins)
pinguins <- pinguins[,-c(2,7,8)]

pinguins <- na.omit(pinguins)
n <- round(0.8*nrow(pinguins))


indice_treinamento <- sample(1:nrow(pinguins),n)
treinamento <- pinguins[indice_treinamento,]
teste <- pinguins[-indice_treinamento,]


treino_padronizado <- scale(treinamento[,-1])
teste_padronizado <- scale(teste[,-1])

classe_treino <- treinamento$species
classe_teste <- teste$species

resultado <- c()
for(i in 1:10){
  modelo <- knn(treino_padronizado,teste_padronizado,classe_treino,classe_teste, k = i)
  resultado[i] <- mean(modelo == teste$species)
}

df <- data.frame(k = 1:10,resultado)

ggplot(df, aes(x = k, y = resultado))+
  geom_line()+
  theme_minimal()

modelo1 <- knn(treino_padronizado,teste_padronizado,classe_treino,classe_teste, k = 2)
mean(modelo1 == teste$species)

#Validação cruzada