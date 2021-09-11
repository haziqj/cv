library(tidyverse)
library(googlesheets4)
gs4_deauth()

sheets <- c("cv_entries", "publications")
dir.create("content")

store_sheet_in_csv <- function(sheet_name){
  read_sheet("https://docs.google.com/spreadsheets/d/1wPqqaIK70DcUNOuOB82n7JYGwW-nySkqsEkSoJxJ9B4/edit?usp=sharing",
             sheet = sheet_name, col_types = c("c")) %>% 
    write_csv(here::here(str_c("content/", sheet_name, ".csv")))
}

walk(sheets, store_sheet_in_csv)