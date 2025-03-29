library(RMariaDB)
library(tidyverse)

#### Data Retrievel ####
readRenviron("data/.Renviron")
SQLpassword <- Sys.getenv("SQLpassword")
SQLpassword <- paste0(SQLpassword,'"')
host <- Sys.getenv("host")
port <- Sys.getenv("port")
user <- Sys.getenv("user")
con <- dbConnect(MariaDB(),
                 dbname = "Eksamen",
                 host = host,
                 port = port,
                 user = user,
                 password = SQLpassword)

dbListTables(con)
allshots_raw <- dbReadTable(con, "wyscout_matchevents_shots_sl") 
allevents_raw <- dbReadTable(con, "wyscout_matchevents_common_sl") 

#bind those badboys
allshotevents_raw <- allshots_raw %>%
  left_join(allevents_raw, by = "EVENT_WYID")
# husk shots are ikke frispark, selvom osv


# variabler
  # kan det være muligt at beregne en spillers ELO?



# Forskellige modeller der kan bruges:
# Decision tree
    # Random forest
# glm