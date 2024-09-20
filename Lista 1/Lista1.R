library(ggplot2)
library(rpart)
library(rpart.plot)

#Exercico 1
a <- c(10:30)
a
b <- c(30:10)
b
c <- c(a,b)
c

#Exercicio 2
?rep
?seq
a2 <- rep(seq(2,8,2),10)
a2
b2 <- c(rep(c(2, 4, 6, 8), 10), 2)
b2

#Exercicio 3
n <- 20:30
a3 <- sum(n^2 + 4*n)
a3
n <- 10:20
b3 <- sum((3*n) / (n + 2*n/n^2))
b3

#Exercicio 4 falta letra c
sorteio <- sample(1:100,40,replace = TRUE)
sorteio
a4 <- sum(sorteio %% 2 == 0)
a4
b4 <- sum(sorteio > 70)
b4
c4 <-  which(sorteio %% 2 != 0)
c4


#Exercicio 5 
exercico5 <- function(){
  qtd4 <- 0
  lancamentos <- 0
  while(qtd4 != 2){
    if(sample(1:6,1) == 4){
      qtd4 <- qtd4 + 1
    }
    lancamentos <- lancamentos + 1
  }
  lancamentos
}
exercico5()

#Exercicio 6
quantidades <- c()
for(i in 1:10000){
  quantidades[i] <- exercico5()
}
mean(quantidades)

#Exercicio 7
fibonacci <- function(n){
  if(n < 3){
    print("n menor que 3")
    return()
  }
  fib <- c()
  fib[1] = 1
  fib[2] = 2
  
  for(i in 3:n){
    fib[i] <- fib[i-1] + fib[i-2]
  }
  fib
}
fibonacci(5)

#Exercico 8
participantes <- c("Michel scott","Dwight Schrute","Jim Halpert","Kevin Malone","Creed Bratton")
sorteioAmigoOculto <- function(participantesSorteio){
  amigo_secreto <- c(sample(participantesSorteio))
  for(i in length(participantesSorteio)){
    if(participantesSorteio[i] == amigo_secreto[i]) return(0)
    return(1)
  }
}
resultadoSorteio <- sorteioAmigoOculto(participantes)
resultadoSorteio <- c()
for (i in 1:10000) {
  resultadoSorteio[i] <- sorteioAmigoOculto(participantes)
}
mean(resultadoSorteio)

#Exercicio 9
Crabs <- function(){
  somaInicial <- sum(sample(1:6,2,TRUE))
  if(somaInicial == 7) return(1)
  if(sum(somaInicial == c(2,3,12)) == 1) return(0)
  while(TRUE){
    proximaSoma <- sum(sample(1:6,2,TRUE))
    if(proximaSoma == 7) return(0)
    if(proximaSoma == somaInicial) return(1)
  }
}
resultasosCrab <- c()
for(i in 1:100000){
  resultasosCrab[i] <- Crabs()
}
mean(resultasosCrab)

#Exercicio 10
Passeio <- function(l){
  if(l < 0 | l >= 20) return(print("Valor de l invalido"))
  while(TRUE){
    moeda = sample(1:2,1)
    if(moeda == 1){
      l = l + 1
    } else {
      l = l - 1
    }
    if(l == 0) return(l)
    if(l == 20) return(1)
  }
}

passeios <- c()
P10mil <- function(l){
  for(i in 1:10000)
    passeios[i] <- Passeio(l)
  mean(passeios)
}

passeiosPlot <- c()
for(i in 1:19){
  passeiosPlot[i] <- P10mil(i)
}
quantidadeL <- c(1:19)
Pplot <- data.frame(passeiosPlot,quantidadeL)
Pplot |>
  ggplot(aes(x = quantidadeL, y = passeiosPlot))+
  geom_col(fill = "blue")+
  theme_minimal()

# Exercicio 11
# a) 
Exercicio11a <- function(n){
  Posicao <- c(0, 0)
  # 1-L 2-R 3-U 4-D 
  deslocamento <- sample(1:4, n, TRUE)
  for(i in 1:n){
    switch (deslocamento[i],
            "1" = Posicao[1] <- Posicao[1] - 1, # Esquerda
            "2" = Posicao[1] <- Posicao[1] + 1, # Direita
            "3" = Posicao[2] <- Posicao[2] + 1, # Cima
            "4" = Posicao[2] <- Posicao[2] - 1  # Baixo
    )
  }
  return(Posicao)
}
Exercicio11a(8)

# b) 
resultadosLink <- c() # Armazenar os resultados
for(i in 1:10000){
  P <- Exercicio11a(8) # P é a posição final após 8 movimentos
  if(P[1] == 0 & P[2] == 0)  # Verifica se voltou para (0,0)
    resultadosLink[i] <- 1
  else
    resultadosLink[i] <- 0
}

# Calcular a média de vezes que voltou para (0,0)
mean(resultadosLink)

# c) 
Exercicio11c <- function(n){
  if(n %% 2 == 1) {
    return("Numero nao pode ser impar.") # Retorna erro para números ímpares
  }
  
  resultadosLinkC <- c() # Inicializa vetor para armazenar resultados
  for(i in 1:10000){
    Pc <- Exercicio11a(n) # Pc é a posição final após n movimentos
    if(Pc[1] == 0 & Pc[2] == 0)  # Verifica se voltou para (0,0)
      resultadosLinkC[i] <- 1
    else
      resultadosLinkC[i] <- 0
  }
  return(mean(resultadosLinkC))
}
Exercicio11c(44)

# Exercicio 12 1- Cara 2- Coroa
Exercicio12 <- function(steven,garnet){
  CC <- sample(0:1,3,TRUE)
  if(sum(steven == CC) == 3) return(print("steven"))
  if(sum(garnet == CC) == 3) return(print("garnet"))
  
  while(TRUE){
      CC[1] <- CC[2]
      CC[2] <- CC[3]
      CC[3] <- sample(0:1,1)
      if(sum(steven == CC) == 3) return(print("steven"))
      if(sum(garnet == CC) == 3) return(print("garnet"))
  }
}
Exercicio12(c(0,1,0),c(0,0,1))

# Exercicio 13
dados <- read.table("dados.txt",TRUE,";")
str(dados)

# a)
dados |>
  ggplot(aes(x = Genero))+
  geom_bar(fill = "darkred") +
  theme_minimal()
# Pelo gráfico fica evidente que a maioria das vitimas eram mulheres, sendo o numero de mortes mais de 3x o número de homens

# b)
dados |>
  ggplot(aes(x = Idade))+
  geom_histogram(bins = 8, color = "black", fill = "#f1a098")+
  labs(title = "Histograma da Idade", x = "Idade", y = "Frequência") +
  theme_minimal()+
  facet_wrap(~ Genero)
# A maioria das vitimas eram pessoa acima dos 40 anos e grande parte cerca de 80 anos

# c)
dados |>
  ggplot(aes(y = Idade, x = Genero))+
  geom_boxplot()+
  labs(title = "Boxplot da Idade por Gênero", x = "Gênero", y = "Idade") +
  theme_minimal()
# A mediana das idade de homens é maior do que das mulheres, os quartis parecem ser de tamanhos iguais

# d)
dados |>
  ggplot(aes(x = LocalDaMorte))+
  geom_bar(fill = "red")+
  labs(title = "Gráfico por local da morte", x = "Local", y = "Frequência")
  theme_minimal()
  
#A frequência de mortes em casas de repouso e de hospitais é baixa.A maioria das mortes ocorre na própria casa, com uma frequência significativamente maior do que nos outros dois locais.
  
# e)
dados |>
  ggplot(aes(x = AnoDaMorte))+
  geom_bar(fill = "red")+
  theme_minimal()
# A primeira morte ocrreu em 1975, até os anos de 1982 as mortes eram menores, após esse tempo a quantidade começou a cescer. Depois dos anos de 1992 a quantidade cresceu ainda mais chengando no seu apice perto dos anos 2000

# f)
# alvo principal mulheres, cujas mortes representaram mais de três vezes o número de homens. A maioria de suas vítimas era composta por pessoas com mais de 40 anos, sendo a faixa etária mais atingida próxima aos 80 anos. A mediana de idade dos homens foi maior que a das mulheres, e os quartis dos dados de idade pareceram ser equilibrados em tamanho. As mortes em casas de repouso e hospitais foram relativamente baixas, com a maioria das vítimas morrendo em suas próprias casas. O padrão temporal mostra que as mortes ocorreram em 1975, com um aumento gradual até 1982, seguido de um crescimento acentuado a partir de 1992, atingindo seu pico por volta dos anos 2000. O perfil das vítimas, portanto, indica um predomínio de mulheres idosas , fale geralmentecendo em suas residências.

# Exercicio 14
primatas <- read.table("primatas.txt",TRUE,":")
str(primatas)
summary(primatas)
# b)
primatas |>
  ggplot(aes(x = especie,fill = genero))+
  geom_bar()+
  theme_minimal()
# c)
primatas |>
  ggplot(aes(x = peso,y = altura,fill = genero))+
  geom_col()+
  facet_wrap(~ especie)+
  theme_minimal()
# d)
primatas |>
  ggplot(aes(x = peso,y = altura,col = especie))+
  geom_point()+
  facet_wrap(~ genero)+
  theme_minimal()
?geom_point
# e)
# Em ambas as espécies, as fêmeas têm um peso menor em comparação aos machos. Nos chimpanzés, o peso das fêmeas é mais concentrado entre 40 e 50 kg, enquanto os machos variam mais, entre 50 e 60 kg. Em termos de altura, os machos são geralmente mais altos, enquanto as fêmeas são mais baixas, mas há alguma sobreposição entre as duas distribuições. Assim como nos bonobos, os machos chimpanzés tendem a ser mais altos que as fêmeas, embora a diferença de altura entre os gêneros seja perceptível.Os chimpanzés, em geral, parecem ser mais pesados que os bonobos, especialmente entre os machos, que podem ultrapassar os 60 kg. Já os bonobos machos raramente passam de 50 kg. Em termos de altura, os chimpanzés machos e fêmeas também são ligeiramente mais altos do que os bonobos de ambos os gêneros.

n <- round(0.8*nrow(primatas))
set.seed(1910)
primatas <- primatas[sample(nrow(primatas)),]
treinamento <- primatas[1:n,]
teste <- primatas[-(1:n),]
Arvore <- rpart(especie~., data = treinamento, method = "class")
rpart.plot(Arvore)
previsao <- predict(Arvore, newdata = teste, type = "class")
mean(previsao == teste$especie)
