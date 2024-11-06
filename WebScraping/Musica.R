library(rvest)
library(dplyr)
library(tidytext)
library(ggplot2)

url <- "https://www.letras.mus.br/lady-gaga/die-with-a-smile-feat-bruno-mars/"

titulo <- html <- read_html(url)
html |>
  html_element("h1") |>
  html_text2()

letra <- html |>
  html_elements("div.lyric") |>
  html_elements("p") |>
  html_text2() |>
  paste(collapse = " ")

letra <- data.frame(letra)

letra |>
  unnest_tokens(output = word, input = letra) |>
  count(word, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(y = word, x = n)) +
  geom_col() 
  
 
