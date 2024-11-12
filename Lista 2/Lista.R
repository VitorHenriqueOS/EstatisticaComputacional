library(rpart)
library(rpart.plot)
library(randomForest)
library(ggplot2)

diabetes <- read.csv("diabetes.txt", sep=";")
diabetes <- diabetes[,-1]
diabetes$Diabetic <- as.factor(diabetes$Diabetic)

set.seed(1900)
diabetes <- diabetes[sample(nrow(diabetes)),]
n = round(0.8*nrow(diabetes))
treinamento <- diabetes[1:n,]
teste <- diabetes[-(1:n),]

# a)

# b)
treinamento |>
  rpart(formula = Diabetic~., method = "class") |>
  rpart.plot()

modeloArvore <- rpart(data = treinamento,formula =  Diabetic~., method =  "class")
previsao <- predict(modeloArvore,newdata = teste, type = "class")
mean(previsao == teste$Diabetic)

Diabete <- function(paciente) {
  if (paciente$Pregnancies < 2) return(0)
  if (paciente$BMI < 22) return(0)
  else {
    if (paciente$SerumInsulin < 52) {
      if (paciente$Age < 36) return(0)
      else return(1)
    } else {
      if(paciente$Age < 36){
        if(paciente$PlasmaGlucose < 96){
          if(paciente$BMI >= 33) return(0)
          else return(1)
        }
        else{
          if(paciente$Age >= 24){
            if(paciente$Age < 27) return(0)
            else return(1)
          }
          else return(1)
        }
      } else return(1)
    }
  }
}


resultados <- c()
for (i in 1:nrow(teste)) {
  resultados[i] <- Diabete(teste[i,])
}
mean(resultados == teste$Diabetic)


#c)
modeloRF <- randomForest(Diabetic ~ ., data = treinamento, ntree = 500)
previsaoRF <- predict(modeloRF, newdata = teste)

mean(previsaoRF == teste$Diabetic)

#

#---------------------------Exercicio 2

cerebelo <- read.csv("cerebelo.csv",header = TRUE)

# a)
cerebelo |>
  ggplot(aes(x = Body_g, y = Cerebellum_g))+
  geom_point()+
  theme_minimal()

cerebelo |>
  ggplot(aes(y = Log_body, x = Log_cerebellum))+
  geom_point()+
  theme_minimal()

# No primeiro grafico a dispersão se mostra pouca tendo um agrupamento em uam parte, no segundo grafico a dispersão é maior mostrando que pode haver uma possivel reta que correlaciona os pontos.

# b)
cor(cerebelo$Cerebellum_g, cerebelo$Body_g)

# c)
cor(cerebelo$Log_cerebellum, cerebelo$Log_body)

# d) quando em bases diferente o coeficiente de correlação mostra qu ano existe, mas comparando em base logarítima chega-se ao resultado de que há uma forte correlacao entre as variaveis

# e)

EqReta <- lm(data = cerebelo, formula = Log_body ~ Log_cerebellum)
summary(EqReta)
#H0: coeficiente de correlacao é zero
#Ha: coeficiente de correlacao não é zero

# como  p-valor é menor que 0.05, rejeitamos a hipotese nula e concluimos que o coeficiente é diferente de zero

cerebelo |>
  ggplot(aes(x = Log_body, y = Log_cerebellum))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method = "lm")

# f)
#H0: os residuos seguem uma distribuicao normal
#Ha: os residuos não seguem uma distribuicao normal
hist(EqReta$residuals)
shapiro.test(EqReta$residuals)
# como o p-valor > 0.05, não rejeitamos a hipotese nula. Os residuos seguem uma distribuição normal.

# g)


#---------------------------Exercicio 3

olive <- read.table("olive.txt", header = TRUE, sep = ",")

dados_padronizados <- scale(olive[,-1])

#-----------k = 3
modelo_Kmeans <- kmeans(dados_padronizados, centers = 3, nstart = 20)
cluster_k3 <- modelo_Kmeans$cluster

olive$cluster_k3 <- cluster_k3

olive |>
  ggplot(aes(x = cluster_k3, fill = region))+
  geom_bar()


#------------k = 4
modelo_Kmeans <- kmeans(dados_padronizados, centers = 4, nstart = 20)
cluster_k4 <- modelo_Kmeans$cluster

olive$cluster_k4 <- cluster_k4

olive |>
  ggplot(aes(x = cluster_k4, fill = region))+
  geom_bar()


#------------k = 5
modelo_Kmeans <- kmeans(dados_padronizados, centers = 5, nstart = 20)
cluster_k5 <- modelo_Kmeans$cluster

olive$cluster_k5 <- cluster_k5

olive |>
  ggplot(aes(x = cluster_k5, fill = region))+
  geom_bar()






