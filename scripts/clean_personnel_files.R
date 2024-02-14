library(tidyverse)
library(readxl)
library(janitor)

personnel_tables <- read_xlsx("data/hillsborough_district/personnel_reports_pinpoint.xlsx", 
                              sheet = 1) %>% 
  clean_names() %>% 
  mutate(doc_id = str_extract(validation_link, "docid=[a-z0-9]*") %>% 
           str_remove("^docid=")) %>% 
  rename(file_name_table = file_name, 
         validation_link_table = validation_link)
personnel_fields <- read_xlsx("data/hillsborough_district/personnel_reports_pinpoint.xlsx", 
                              sheet = 2) %>% 
  clean_names() %>% 
  mutate(doc_id = str_extract(validation_link, "docid=[a-z0-9]*") %>% 
           str_remove("^docid="))

personnel_joined <- full_join(personnel_tables, 
                              personnel_fields, 
                              by = "doc_id") %>% 
  write_csv("data/hillsborough_district/personnel_reports_clean.csv", 
            na = "")
