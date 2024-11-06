library(jpeg)

imagem <- readJPEG("imagem.jpeg")
str(imagem) # altura x largura x rgb

R <- as.factor(imagem[,,1])
G <- as.factor(imagem[,,2])
B <- as.factor(imagem[,,3])

x <- rep(1:259, each = 148)
y <- rep(148:1, times = 259)

dados <- data.frame(x,y,R,G,B)
head(dados)

clusterizacao <- kmeans(x = dados[,3:5], centers = 3, nstart = 20)

clusterizacao$centers
cores <- rgb(clusterizacao$centers)
cluster <- as.factor(clusterizacao$cluster)
dados $cluster <- cluster
head(dados)

dados |>
  ggplot(aes(x = x, y = y, col = cluster))+
  geom_point()+
  scale_color_manual(values = cores)+
  theme_void()
  
