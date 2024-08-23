set.seed(1711)
iris <- iris[sample(nrow(iris)),]
iris
n <- round(0.8*nrow(iris))

treinamento <- iris[1:n,]
teste <- iris[-(1:n),]

ggplot(iris, aes(x = Petal.Length, y =Petal.Width,col = Species))+
  geom_point(size = 2, alpha = 0.5)+
  theme_minimal()

ggplot(data = treinamento,mapping = aes(x = Species))+
  geom_bar()


ggplot(treinamento, aes(x = Petal.Length))+
  geom_histogram(bins = 20, fill = "darkgrey")+
  theme_minimal()

ggplot(treinamento, aes(y = Petal.Length))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(treinamento, aes(y = Petal.Width))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(treinamento, aes(y = Sepal.Length))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(treinamento, aes(y = Sepal.Width))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(treinamento, aes(x = Petal.Length, y =Petal.Width,color = Species))+
  geom_point(size = 2, alpha = 0.5)

ggplot(treinamento, aes(x = Sepal.Length, y =Sepal.Width,color = Species))+
  geom_point(size = 2)

resultados <- c()

for (i in 1:nrow(teste)) {
  if(teste$Petal.Length[i] < 2.25){
    resultados[i] <- "setosa"
  }else{
    if(teste$Petal.Width[i] < 1.25){
      resultados[i] <- "versicolor"
    }
    else{
      resultados[i] <- "virginica"
    }
  }
}

mean(teste$Species == resultados)
resultados
