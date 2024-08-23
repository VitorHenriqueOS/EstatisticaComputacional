titanic <- read.table(file = "titanic.txt", header = TRUE, sep = ",")
titanic <- titanic[,-c(1,9:12)]

titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)

summary(titanic)
str(titanic)

ggplot(data = titanic, aes(x = Survived))+
  geom_bar(fill = "blue")+
  theme_minimal()

ggplot(data = titanic, aes(x = Survived, fill = Sex))+
  geom_bar()+
  scale_fill_manual(values = c("male" = "#ff7f00", "female" = "#984ea3"))+
  theme_minimal()

ggplot(data = titanic, aes(x = Sex, fill = Survived))+
  geom_bar()+
  scale_fill_manual(values = c("0" = "#e41a1c", "1" = "#377eb8"))+
  theme_minimal()

ggplot(data = titanic, aes(x = Sex, fill = Survived))+
  geom_bar()+
  labs(title = "Analise de sobrevivencia de homens e mulheres por classe",x = "Sexo", y = "Sobrevivencia", fill = "Sobreviveu")+
  scale_fill_manual(values = c("0" = "#e41a1c", "1" = "#377eb8"),labels = c("0" = "Não", "1" = "Sim"))+
  facet_wrap(~Pclass)+
  scale_x_discrete(labels = c("female" = "mulher", "male" = "homen"))+
  theme_minimal()

