library(rvest)
library(dplyr)
library(ggplot2)
library(stringr)
library(geobr)

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetiza%C3%A7%C3%A3o"
html <- read_html(url)

html_element(html, "h1")
html_text2(html_element(html, "h1"))

html |>
  html_element("h1") |>
  html_text2()

tabelas <- html |>
  html_elements("table") |>
  html_table()

alfabetizacao <- tabelas[[3]]

alfabetizacao <- alfabetizacao[,c(2,3)]
names(alfabetizacao) <- c("estado", "taxa")

str_replace_all(string = "2ped3ro145",pattern = "\\d",replacement = "")

parte1 <- str_replace_all(alfabetizacao$taxa, ",",".")
parte2 <- str_replace_all(parte1, "%","")
parte_final <- as.numeric(parte2)
parte_final <- parte_final/100

alfabetizacao$taxa <- parte_final

minas <- read_state(code_state = "MG")

ggplot(minas)+
  geom_sf(fill = "darkorange")+
  theme_void()

municipioMg <- read_municipality(code_muni = "MG")

ggplot(municipioMg)+
  geom_sf()+
  theme_void()

ggplot(read_state())+
  geom_sf()+
  theme_void()

estados <- read_state()
estados <- estados[order(estados$name_state),]

alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]
estados$taxa <- alfabetizacao$taxa

ggplot(estados,aes(fill = taxa))+
  geom_sf()+
  scale_fill_gradient(high = "#132B43", low = "#56B1F7")+
  theme_void()







?scale_fill_gradient
