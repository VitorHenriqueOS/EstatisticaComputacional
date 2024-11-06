library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stopwords)
library(rvest)
library(stringr)
library(janeaustenr)

url <- "https://www.bbc.com/portuguese/articles/c8dj6prn6lvo"
html <- read_html(url)
texto <- html |>
  html_elements("p.bbc-hhl7in") |>
  html_text2() |>
  paste(collapse = " ")

conjunto <- data.frame(texto)

conjunto |> 
  unnest_tokens(output = palavra, input = texto) |>
  count(palavra, sort = TRUE) |>
  top_n(10)

stopwords_br <- data.frame(palavra = stopwords("pt"))

conjunto |>
  unnest_tokens(output = palavra, input = texto) |>
  anti_join(stopwords_br) |>
  count(palavra, sort = TRUE) |>
  top_n(10) |>
  mutate(palavra = reorder(palavra,n)) |>
  ggplot(aes(y = palavra, x = n)) +
  geom_col(fill = "orange") +
  theme_minimal()





livro <- prideprejudice  

#stopwords_en <- data.frame(palavra = c(stopwords("en"),"mr","mrs"))

stopwords_en <- data.frame(palavra = stopwords("en"))

conjunto2 <- data.frame(livro)
conjunto2 |>
  unnest_tokens(output = palavra, input = livro) |>
  anti_join(stopwords_en) |>
  count(palavra, sort = TRUE) |>
  top_n(10) |>
  mutate(palavra = reorder(palavra,n)) |>
  ggplot(aes(y = palavra, x = n)) +
  geom_col() +
  theme_minimal()
  
sentimentos <- get_sentiments("bing")

capitulos <- str_detect(conjunto2$livro, "^Chapter \\d+")
capitulos <- cumsum(capitulos)


conjunto2 |>
  mutate(capitulo = capitulos) |>
  unnest_tokens(word, livro) |>
  inner_join(sentimentos) |>
  count(capitulo, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total =  positive - negative) |>
  ggplot(aes(x = capitulo, y = total, fill = total > 0)) +
  geom_col() +
  theme_minimal()


