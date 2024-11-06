femur <- read.csv("/cloud/project/femur.csv")
femur <- femur[,-1]

homens <- femur[femur$genero == "Male",]
mulheres <- femur[femur$genero == "Female",]

cor(homens$altura, homens$femur)

homens |>
  ggplot(aes(x = femur, y = altura)) +
  geom_point()+
  geom_smooth(method = "lm")

cor(mulheres$altura, mulheres$femur)
mulheres |>
  ggplot(aes(x = femur, y = altura)) +
  geom_point()+
  geom_smooth(method = "lm")

#Passo 1: Analisar a correlacao

cor.test(homens$altura,homens$femur)

#H0: coeficiente de correlacao é zero
#H1: coeficiente de correlação não é zero

#Passo 2: Determinar a reta de regressao e verificar se e um bom modelo

modelo <- lm(data = homens, formula = altura ~ femur)
modelo
summary(modelo)

#Passo 3: Analisar os residuos, isto é, analisar os erros

hist(modelo$residuals)

# Vamos realizar um teste de hipotese para analizar se os residuos segue uma distribuicao normal
#H0: os residuos seguem uma distribuição normal
#H1: os residuos nao seguem uma distribuição normal

shapiro.test(modelo$residuals) # se menor que 5% rejeita H0

