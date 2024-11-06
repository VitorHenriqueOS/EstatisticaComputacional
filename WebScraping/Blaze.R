library(httr)
library(dplyr)
library(jsonlite)

url <- "https://blaze1.space/api/roulette_games/history?startDate=2024-08-14T16:49:39.981Z&endDate=2024-09-13T16:49:39.982Z&page=1"
GET(url)

dados <- content(GET(url),"text")

dados <- fromJSON(dados)
