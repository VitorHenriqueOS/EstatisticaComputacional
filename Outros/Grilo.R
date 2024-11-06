grilo <- read.table("grilo.txt", header = TRUE, ",")
summary(grilo)
str(grilo)

grilo |>
  ggplot(aes(x = frequencia)) +
  geom_histogram(bins = 10) +
  theme_minimal() 

cor(grilo)

grilo |>
  ggplot(aes(x = temperatura, y = frequencia)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = "lm")

modeloLinear <- lm(formula = frequencia ~ temperatura, data = grilo)
modeloLinear

summary(grilo$temperatura)

t <- data.frame(temperatura = c(21, 23.6, 30.9, 29))
predict(modeloLinear, newdata = t)

#

library(palmerpenguins)
dados <- penguins

dados <- na.omit(dados)
str(dados)
summary(dados)
cor(dados[,-c(1,2,7,8)])

dados |>
  ggplot(aes(y = body_mass_g, x = flipper_length_mm, color = species)) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = "lm")

dados |>
  filter(species == "Gentoo")|>
  select(flipper_length_mm, body_mass_g) |>
  cor()

modelo2 <- lm(formula = flipper_length_mm  ~ body_mass_g  + bill_length_mm, data = dados)
modelo2
summary(modelo2)

