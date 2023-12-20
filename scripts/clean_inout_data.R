## clean Hillsborough In/Out reports
library(tidyverse)
library(stringdist)
rm(list = ls())

inout_files <- list.files('data/InOutReports/raw')
sum_na <- function(x){sum(x, na.rm = T)}

## School and district numbers ----
hills_school_ids <- read_csv("data/schoolgrades23_linked.csv") %>% 
  distinct(district_number, district_name, school_number, school_name)

## In Reports ----
in_reports <- lapply(inout_files[which(str_detect(inout_files, "InReport"))],
                     function(x){
                       read_csv(paste0('data/InOutReports/raw/', x),
                                          col_types = "cccciiiiiii")%>% 
                         janitor::clean_names() %>% 
                         fill(school_level) %>% 
                         filter(!str_detect(school_level, "School"),
                                !str_detect(school_level, "Total")) %>% 
                         fill(current_school) %>% 
                         mutate(current_placement = ifelse(str_detect(current_school, "Total"), 
                                                       "Total",
                                                       current_placement)) %>% 
                         fill(current_placement) %>% 
                         mutate(zoned_school = ifelse(is.na(zoned_school), 
                                                      current_placement,
                                                      zoned_school)) %>% 
                         mutate(current_school = str_remove(current_school, "Total") %>% str_squish())
                       }) %>% 
  bind_rows %>% 
  group_by(school_level, current_school, current_placement, zoned_school) %>% 
  summarise_at(c("asian", "black", "hispanic", "indian", "multi", "white", "grand_total"), 
               sum, na.rm = T) %>% 
  mutate(total_row = str_detect(zoned_school, "Total"))

### Match school names to school id from grade report
unique(in_reports %>% current_school) %>% 
  lapply(function(x){
    potential_matches = hills_school_ids %>% 
      match_value = strdis
  })


write_csv(in_reports %>% filter(total_row == T),
          "data/InOutReports/InReports_Total.csv")

write_csv(in_reports %>% filter(total_row == F),
          "data/InOutReports/InReports.csv")

## Out Reports ----
out_reports <- lapply(inout_files[which(str_detect(inout_files, "OutReport"))],
                      function(x){read_csv(paste0('data/InOutReports/raw/', x),
                                           col_types = "cccciiiiiii") %>% 
                          janitor::clean_names() %>% 
                          filter(!str_detect(school_level, "School"),
                                 !str_detect(school_level, "Total")) %>% 
                          fill(zoned_school:current_placement) %>% 
                          mutate(file = x)
                      }) %>% 
  bind_rows() %>% 
  distinct()

out_reports_totals <- out_reports %>% 
  group_by(zoned_school, current_placement) %>% 
  summarise_at(.vars = vars(asian:grand_total), .funs = function(x){sum(x, na.rm = T)}) %>% 
  bind_rows(out_reports %>% 
              group_by(zoned_school) %>% 
              summarise_at(.vars = vars(asian:grand_total), .funs = function(x){sum(x, na.rm = T)}) %>% 
              mutate(current_placement = "total")) %>% 
  arrange(zoned_school)
  

write_csv(out_reports, "data/InOutReports/OutReports.csv")
write_csv(out_reports_totals, "data/InOutReports/OutReports_Total.csv")

## Check ---- 
check <- in_reports %>% 
  filter(total_row == F) %>% 
  select(school_level, 
         current_school, 
         current_placement, 
         zoned_school, 
         grand_total_in = grand_total) %>% 
  full_join(out_reports %>% 
              select(school_level, 
                     current_school, 
                     current_placement, 
                     zoned_school, 
                     grand_total_out = grand_total))

