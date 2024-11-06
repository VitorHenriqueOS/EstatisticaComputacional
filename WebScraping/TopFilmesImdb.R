library(dplyr)
library(rvest)
library(stringr)

url <- "https://www.bosshunting.com.au/entertainment/movies/best-movies-imdb/"
html <- read_html(url)

filmes <- html |>
  html_elements("ol.wp-block-list>li") |>
  html_text2()

filmes[98] <- str_replace(filmes[98],"1941$","8.3")
filmes[98]

anos <- unlist(str_extract_all(filmes, "\\(\\d+\\)"))
anos <- unlist(str_extract_all(anos, "\\d+"))
anos <- as.numeric(anos)
anos

nota <- unlist(str_extract_all(filmes, "\\d\\.\\d$"))
nota

diretores <- unlist(str_extract_all(filmes,"dir\\. .+"))
diretores <- str_remove_all(diretores,"dir\\.")
diretores <- str_remove_all(diretores,"\\d+")
diretores <- str_remove_all(diretores,"\\—")
diretores <- str_remove_all(diretores,"\\.+$")
diretores

titulos <- str_remove_all(filmes,"\\(\\d+\\)")
titulos <- str_remove_all(titulos, " dir\\. .+")
titulos

posicao <- c(1:100)
top_filmes <- data.frame(posicao,titulos,anos,diretores,nota)
write.csv(top_filmes,file = "Top_Filmes_Imdb", row.names = FALSE)
