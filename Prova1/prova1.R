library(rpart)
library(rpart.plot)
# Exercicio 1
# A
# 1 ate 13 azuis, 14 ate 26 vermelhas, 27 ate 39 brancas, 40 ate 52 marrons
sorteio <- c()
proporcao <- c()
for(j in 1:100000){
  for(i in 3){
    sorteio <- sample(1:52,3)
  }
  if(sorteio[1] <= 13 & sorteio[2] <= 13 & sorteio[3] > 13)
    proporcao[j] = TRUE
  else if(sorteio[1] <= 13 & sorteio[3] <= 13 & sorteio[2] > 13)
    proporcao[j] = TRUE
  else if(sorteio[3] <= 13 & sorteio[2] <= 13 & sorteio[1] > 13)
    proporcao[j] = TRUE
  else
    proporcao[j] = FALSE
}
mean(proporcao)
#

a1 <- function(n){
  sorteio <- c()
  proporcao <- c()
  for(j in 1:n){
    for(i in 3){
      sorteio <- sample(1:52,3)
    }
    if(sorteio[1] <= 13 & sorteio[2] <= 13 & sorteio[3] > 13)
      proporcao[j] = TRUE
    else if(sorteio[1] <= 13 & sorteio[3] <= 13 & sorteio[2] > 13)
      proporcao[j] = TRUE
    else if(sorteio[3] <= 13 & sorteio[2] <= 13 & sorteio[1] > 13)
      proporcao[j] = TRUE
    else
      proporcao[j] = FALSE
  }
  mean(proporcao)
}

# B
proporcao <- c()
for(i in 1:100000){
  qtdSorteio <- 0
  qtd <- 0
  while (qtd != 4) {
    qtdSorteio <- qtdSorteio + 1
    so <- sample(1:52,1,TRUE)
    if(so == 7 | so == 20 | so == 34 | so == 46) qtd <- qtd + 1
  }
  proporcao[i] <- qtdSorteio
}
mean(proporcao)
#

#Exercicio 2

dados <- read.table("churn.txt",TRUE,";")
dados <- dados[sample(nrow(dados)),]
dados$Geography <- as.factor(dados$Geography)
dados <- dados[,-c(1,2,3)]

n <- round(0.75*nrow(dados))
treinamento <- dados[1:n,]
teste <- dados[-(1:n),]

Arvore <- rpart(formula = Exited ~., data = treinamento, method = "class")
rpart.plot(Arvore)

previsao <- predict(Arvore, newdata = teste, type = "class")
mean(previsao == dados$Exited)

France <- dados[dados$Geography == "France",]
Spain <- dados[dados$Geography == "Spain",]
Germany <- dados[dados$Geography == "Germany",]

#Farnça
n <- round(0.75*nrow(France))
France <- France[,-c(1,2,3)]
treinamentoF <- France[1:n,]
testeF <- France[-(1:n),]
ArvoreF <- rpart(formula = Exited ~., data = treinamentoF, method = "class")
rpart.plot(ArvoreF)
previsaoF <- predict(ArvoreF, newdata = testeF, type = "class")
mean(previsaoF == France$Exited)

#Espanha

n <- round(0.75*nrow(Spain))
Spain <- Spain[,-c(1,2,3)]
treinamentoS <- Spain[1:n,]
testeS <- Spain[-(1:n),]
ArvoreS <- rpart(formula = Exited ~., data = treinamentoS, method = "class")
rpart.plot(ArvoreS)

previsaoS <- predict(ArvoreS, newdata = testeS, type = "class")
mean(previsaoS == Spain$Exited)

#Alemanha

n <- round(0.75*nrow(Germany))

Germany <- Germany[,-c(1,2,3)]

treinamentoG <- Germany[1:n,]

testeG <- Germany[-(1:n),]

ArvoreG <- rpart(formula = Exited ~., data = treinamentoG, method = "class")

rpart.plot(ArvoreG)

previsao <- predict(ArvoreG, newdata = testeG, type = "class")

mean(previsaoG == Germany$Exited)

# A previsao da França é levemente maior que os demais paises. Sim mas ficam entre 75% e 80%