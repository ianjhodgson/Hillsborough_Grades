## CLEAN KELLY SUB DATA ##

library(tidyverse)
library(readxl)
library(janitor)

## list files ----
ks_files <- list.files('data/KellySubs/')

## cleaning function ----

x = ks_files[1]
fn_clean_ks <- function(x){
  paste0('data/KellySubs/', x) %>% 
    read_xlsx(skip = 13) %>% 
    clean_names() %>% 
    filter(!is.na(school), 
           !str_detect(school, "SUBTOTAL"),
           !str_detect(school,"RECAP"))
}

## lapply function over all files ----
ks_data <- lapply(ks_files, fn_clean_ks) %>% 
  bind_rows()

write_csv(ks_data, 'data/kelly_subs_clean.csv', na = "")
