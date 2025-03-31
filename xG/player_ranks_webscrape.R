library(rvest)
library(httr)
library(tidyverse)

# https://www.fifaindex.com/players/?gender=0&league=1&order=desc
readRenviron("data/.Renviron")
cookie <- Sys.getenv("cookie")
user_agent <- Sys.getenv("agent")

url <-paste0("https://www.fifaindex.com/players/fifa24_599/?page=","&gender=0&league=1&order=desc")
headers <- c(
  "User-Agent" = user_agent,
  "Cookie" = cookie,
  "Accept-Language" = "en-US,en;q=0.9",
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Referer" = "https://www.fifaindex.com/",
  "Connection" = "keep-alive"
)

response <- GET(url, add_headers(.headers = headers))
response$status_code
page <- read_html(response)

last_page_href <- page %>%
  html_node("a.page-link:contains('Last')") %>%
    html_attr("href")
last_page <- str_extract(last_page_href, "(?<=page=)\\d+") %>% as.numeric() # only take the page number

# get all rows (each player)
rows <- html_elements(page, "tr[data-playerid]")

players_df <- NULL

player_ids <- c()
names <- c()
nationalities <- c()
overalls <- c()
potentials <- c()
positions <- c()
ages <- c()
teams <- c()

for (i in seq_along(rows)) {
  row <- rows[[i]]
  
  player_ids[i] <- html_attr(row, "data-playerid")
  names[i] <- html_text(html_element(row, 'td[data-title="Name"] a'))
  nationalities[i] <- html_attr(html_element(row, 'td[data-title="Nationality"] a'), "title")
  
  ovr_pot <- html_elements(row, 'td[data-title="OVR / POT"] span')
  overalls[i] <- html_text(ovr_pot[1])
  potentials[i] <- html_text(ovr_pot[2])
  
  positions[i] <- row %>%
    html_elements('td[data-title="Preferred Positions"] a') %>%
    html_attr("title") %>%
    paste(collapse = ", ")
  ages[i] <- html_text(html_element(row, 'td[data-title="Age"]'))
  teams[i] <- row %>%
    html_element('td[data-title="Team"] a') %>%
    html_attr("title")
}

players_df <- data.frame(
  player_id = player_ids,
  name = names,
  nationality = nationalities,
  overall = overalls,
  potential = potentials,
  preferred_positions = positions,
  age = ages,
  team = teams,
  stringsAsFactors = FALSE
)
