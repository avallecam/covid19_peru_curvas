# FIG.D LOCAL-IMPORTED CASE -> MODEL THE TRANMISSION NOT CASES

# CANADA

# FIG.C DELAY SIMPTOMS-CONFIRMATION INTERNACIONAL ---------

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(aweek)
library(rlang)

ncovdb_sheetname <- "https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/edit#gid=0"


# ncovdb_in_hubei <- sheets_get(ss = ncovdb_sheetname) %>% 
#   read_sheet(sheet = "Hubei") %>% 
#   mutate(hubei="in") %>% 
#   mutate_at(.vars = vars(contains("date")),.funs = dmy)

ncovdb_out_hubei <- sheets_get(ss = ncovdb_sheetname) %>%
  read_sheet(sheet = "Cases")