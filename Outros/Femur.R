library(ggplot2)
data(iris)
iris |>
  ggplot(aes(x= Petal.Length, y = Petal.Width)) +
  geom_point() +
  theme_minimal()
cor(iris$Petal.Length, iris$Petal.Width)
cor(iris[,-5]) 

femur <- read.csv("/cloud/project/femur.csv")
femur <- femur[,-1]

femur |>
  ggplot(aes(x = femur, y = altura, colour = genero)) +
  geom_point() +
  facet_wrap(~genero) +
  theme_minimal()

homens <- femur[femur$genero == "Male",]
mulheres <- femur[femur$genero == "Female",]

cor(homens$altura, homens$femur)
cor(mulheres$altura, mulheres$femur)

mean(homens$altura)

homens |>
  ggplot(aes(x = femur, y = altura))+
  geom_point(col = "blue") +
  theme_minimal()

mulheres |>
  ggplot(aes(x = femur, y = altura))+
  geom_point(col = "red") +
  theme_minimal()

modeloLinearHomens <- lm(data = homens, formula = altura ~ femur)
modeloLinearHomens

summary(homens$femur)
