#Exercico 1
a <- c(10:30)
b <- c(30:10)
c <- c(a,b)

#Exercicio 2 falta letra b
?rep
?seq
a2 <- rep(seq(2,8,2),10)


#Exercicio 3


#Exercicio 4 falta letra c
sorteio <- sample(1:100,40,replace = TRUE)
sum(sorteio %% 2 == 0)
sum(sorteio > 70)

#Exercicio 5 tornar uma funcao
qtd4 <- 0
lancamentos <- 0
while(qtd4 != 2){
  if(sample(1:6,1) == 4){
    qtd4 <- qtd4 + 1
  }
  lancamentos <- lancamentos + 1
}
