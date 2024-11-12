library(rpart)
library(rpart.plot)
library(ggplot2)
library(randomForest)

diabetes <- read.csv("diabetes.txt", sep=";")

set.seed(1900)
diabetes <- diabetes[sample(nrow(diabetes)),]
n = round(0.8*nrow(diabetes))
treinamento <- diabetes[1:n,]
teste <- diabetes[-(1:n),]

# a)

  
cor(diabetes)
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
# Acuracia do modelo

#c)
?randomForest
