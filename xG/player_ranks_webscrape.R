library(rvest)
library(httr)
library(tidyverse)
library(RSelenium)

# https://www.fifaindex.com/players/?gender=0&league=1&order=desc
readRenviron("data/.Renviron")
cookie <- Sys.getenv("cookie")
user_agent <- Sys.getenv("agent")

# get cookies with RSelenium
rD <- rsDriver(browser = "firefox")
remDr <- rD$client
remDr$navigate("https://www.fifaindex.com/players/fifa24_599/?page=0&gender=0&league=1&order=desc")
cookies <- remDr$getAllCookies()
cookies

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

players_df <- data.frame()

for (page in 1:last_page){
  Sys.sleep(1)
  cat(paste0("now scraping for page: ",page, " , out of: ",last_page))
  url_loop <- paste0("https://www.fifaindex.com/players/fifa24_599/?page=",page,"&gender=0&league=1&order=desc")
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
  
  for (i in seq_along(rows)) {
    row <- rows[[i]]
    
    player <- tryCatch({
      data.frame(
        player_id = html_attr(row, "data-playerid"),
        name = html_text(html_element(row, 'td[data-title="Name"] a')),
        nationality = html_attr(html_element(row, 'td[data-title="Nationality"] a'), "title"),
        overall = html_text(html_elements(row, 'td[data-title="OVR / POT"] span')[1]),
        potential = html_text(html_elements(row, 'td[data-title="OVR / POT"] span')[2]),
        preferred_positions = paste(
          html_attr(html_elements(row, 'td[data-title="Preferred Positions"] a'), "title"),
          collapse = ", "
        ),
        age = html_text(html_element(row, 'td[data-title="Age"]')),
        team = html_attr(html_element(row, 'td[data-title="Team"] a'), "title"),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message("Fejl i række ", i, ": ", e$message)
      NULL
    })
    
      players_df <- rbind(players_df, player)
  }
}
