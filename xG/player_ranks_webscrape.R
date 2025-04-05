library(rvest)
library(httr)
library(tidyverse)
library(fuzzyjoin)
library(stringr)

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
  "Connection" = "keep-alive",
  "Upgrade-Insecure-Requests" = "1",
  "Sec-Fetch-Dest" = "document",
  "Sec-Fetch-Mode" = "navigate",
  "Sec-Fetch-Site" = "same-origin",
  "Sec-Fetch-User" = "?1"
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
  cat(paste0("now scraping for page: ",page, " , out of: ",last_page,"\n"))
  url_loop <- paste0("https://www.fifaindex.com/players/fifa24_599/?page=",page,"&gender=0&league=1&order=desc")
  headers <- c(
    "User-Agent" = user_agent,
    "Cookie" = cookie,
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Referer" = "https://www.fifaindex.com/",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "same-origin",
    "Sec-Fetch-User" = "?1"
  )
  
  response <- GET(url_loop, add_headers(.headers = headers))
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

#saveRDS(players_df, "xG/Scraped_Data/players_fifaindex.RDS")

#clean
players_df$team <- gsub(" FIFA 24","", players_df$team)

# take only current season from allplayers
allplayers_2324 <- allplayers_raw %>% filter(SEASON_WYID == "188945")



# try and standadize names
#makinbg a full names column
allplayers_2324 <- allplayers_2324 %>%
  mutate(
    fullname = str_trim(paste(FIRSTNAME, MIDDLENAME, LASTNAME)),
    fullname = str_replace_all(fullname, "NA", ""),     # remove literal NAs
    fullname = str_squish(fullname),                    # remove double spaces
    fullname = tolower(fullname)                        # lowercase for matching
  )

# make lower case
players_df <- players_df %>%
  mutate(
    fullname = tolower(str_squish(name))
  )

# merge
# fuzzy to try and find best match
matched_df <- stringdist_left_join(
  players_df,             # start from players_df
  allplayers_2324,        # join from allplayers_2324
  by = "fullname",
  method = "jw",          # Jaro-Winkler (good for names)
  max_dist = 0.25,        # lower = stricter, higher = looser
  distance_col = "match_score"
)

matched_df$overall <- as.integer(matched_df$overall)
matched_df$potential <- as.integer(matched_df$potential)

# identify NAs
unmatched_players <- matched_df %>% 
  filter(is.na(PLAYER_WYID)) %>% select(PLAYER_WYID, fullname.x)

count(unmatched_players)
head(unmatched_players, 10)




