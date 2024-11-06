library(dplyr)
library(rvest)
library(stringr)

url <- "https://www.timeout.com/film/best-horror-films"
html <- read_html(url)

nomes <- html |>
  html_elements("h3._h3_cuogz_1") |>
  html_text2()

posicao <- str_extract_all(nomes, "^\\d+")
posicao <- unlist(posicao)
posicao <- as.numeric(posicao)

anos <- unlist(str_extract_all(nomes, "\\(\\d+\\)$"))
anos <- unlist(str_extract_all(anos, "\\d+"))
anos <- as.numeric(anos)

titulos <- str_remove_all(nomes, "^\\d+\\.\\s+")
titulos <- str_remove_all(titulos, "\\s+\\(\\d+\\)$")

filmes_horror <- data.frame(posicao,titulos,anos)

write.csv(filmes_horror,file = "filmes_horror.csv", row.names = FALSE)

