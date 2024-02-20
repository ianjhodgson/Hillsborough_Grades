rm(list = ls())
library(tidyverse)
library(janitor)
library(readxl)

## Note - still waiting on Hillsborough County Sheriff Department.
##        Plant City police department file format is awful.


## Tampa Police Department
tpd_files <- list.files("data/police_reports/pinpoint/tampa/")

tpd_raw <- lapply(tpd_files, 
                  function(file_name){
                    # file_name = tpd_files[1]
                    read_csv(paste0("data/police_reports/pinpoint/tampa/", file_name)) %>% 
                      clean_names()
                  }) %>% 
  bind_rows() %>% 
  distinct()

### some observations appear to have more than one observation. I deal with that here.

#### Isolate the single-value rows
tpd_raw <- tpd_raw %>% 
  mutate(date = date %>% str_remove("^[A-Za-z]{1,2} "),
         check_date = str_detect(date, "^[A-Za-z]{3}-[0-9]{2}-[0-9]{4}$"),
         check_error = ((str_detect(date, "Date|Page")) &
           !(str_detect(date, "^[A-Za-z]{3}-[0-9]{2}-[0-9]{4}"))))

tpd_raw <- tpd_raw %>% filter(check_error == F | 
                                is.na(check_error)) %>% 
  mutate(synopsis_appended = NA)

synopsis_append <- lapply(2:nrow(tpd_raw),
                  function(n){
                    if(is.na(tpd_raw$check_error[n] & 
                             !is.na(tpd_raw$synopsis[n]))) {
                      tibble(synopsis_append = paste(tpd_raw$synopsis[n-1], 
                                                           tpd_raw$synopsis[n]))
                    } else {
                      tibble(synopsis_append = tpd_raw$synopsis[n-1])
                    }
                  }) %>% 
  bind_rows

tpd_raw$synopsis_appended[1:nrow(tpd_raw)-1] = synopsis_append$synopsis_append

tpd_raw <- tpd_raw %>% filter(!is.na(check_error))

tpd_single <- tpd_raw %>%
  filter(check_date == T) %>% 
  select(-validation_link) %>% 
  distinct()

tpd_synopsis <- unique(tpd_single$synopsis_appended) %>% paste(collapse = "|") %>% 
  str_replace_all("\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)")
tpd_event_file_types <- unique(tpd_single$event_file_type) %>% paste(collapse = "|")
tpd_locations <- unique(tpd_single$location) %>% paste(collapse = "|")

#### for double+ rows. Iteratively extract the observations.
tpd_doubles <- tpd_raw %>% 
  filter(check_date == F)

tpd_doubles_fixed <- lapply(1:nrow(tpd_doubles),
                            function(n){
                              # n = 4
                              ## roles 
                              roles = str_extract_all(tpd_doubles$role[n], "Police Call")
                              n_roles = length(roles[[1]])
                              ## event_file_types
                              event_file_types = str_extract_all(tpd_doubles$event_file_type[n], 
                                                                 tpd_event_file_types)
                              n_event_types = length(event_file_types[[1]])
                              ## file numbers
                              report_file_numbers = str_extract_all(tpd_doubles$report_file_number[n], 
                                                                    "[A-Za-z]{2} [0-9]{4}-[0-9]{4,7}")
                              n_file_numbers = length(report_file_numbers[[1]])
                              ## dates
                              dates = str_extract_all(tpd_doubles$date[n], "[A-Za-z]{3}-[0-9]{2}-[0-9]{4}")
                              n_dates = length(dates[[1]])
                              ## synopses
                              synopses = str_extract_all(tpd_doubles$synopsis_appended[n], 
                                                         tpd_synopsis)
                              n_synopses = length(synopses[[1]])
                              
                              ## build tibble from the separated observations
                              temp <- tibble(file_name = tpd_doubles$file_name[n],
                                     role = roles[[1]],
                                     event_file_type = event_file_types[[1]],
                                     report_file_number = report_file_numbers[[1]],
                                     date = dates[[1]],
                                     synopsis_appended = ifelse(n_synopses == n_dates,
                                                              synopses[[1]],
                                                              "error-check"),
                                     check_row = n,
                                     validation_link = tpd_doubles$validation_link[n])
                              print(paste("row", n, "complete"))
                              temp
                            }) %>% 
  bind_rows()

## check synopsis errors by hand
write_csv(tpd_doubles_fixed %>% 
            filter(synopsis_appended == "error-check"),
            "data/temp/tpd_synopsis_errors.csv")
tpd_synopsis_errors_fixed <- read_csv("data/temp/tpd_synopsis_errors_FIXED.csv") %>% 
  rename(synopsis_appended = synopsis_append)

tpd_clean <- tpd_single %>% mutate(group = "singles") %>% 
  bind_rows(tpd_doubles_fixed %>% filter(synopsis_appended != "error-check") %>% mutate(group = "doubles")) %>% 
  bind_rows(tpd_synopsis_errors_fixed %>% mutate(group = "doubles_fixed")) %>% 
  mutate(synopsis_append = ifelse(is.na(synopsis_appended), synopsis, synopsis_appended)) %>% 
  select(-starts_with("check"), -validation_link) %>% 
  mutate(location_alt = file_name %>% 
           str_replace_all("_", " ") %>% 
           str_remove_all("\\.csv$") %>% 
           str_remove("Redacted"), 
         location = ifelse(is.na(location), 
                           location_alt, 
                           location))

## clean addresses - DO NOT RE-RUN -
# write_csv(tpd_clean %>% distinct(file_name), "data/police_reports/temp/location_name_xwalk.csv", na = "")
file_school_xwalk <- read_csv("data/police_reports/temp/location_name_xwalk.csv") %>% 
  janitor::clean_names() %>% 
  mutate(school_number = str_remove_all(school_number, "\\(|\\)"))

## categorize call synopsis - DO NOT RE-RUN - 
# write_csv(tpd_clean %>% distinct(synopsis_append, synopsis_appended), 
#           "data/police_reports/temp/synopsis_categories.csv", na = "NA")
synopsis_cats <- read_csv("data/police_reports/temp/synopsis_categories.csv") %>% 
  janitor::clean_names()

## write out cleaned file
tpd_clean <- tpd_clean %>% 
  left_join(file_school_xwalk, by = "file_name") %>% 
  left_join(synopsis_cats, by = "synopsis_append") %>% 
  filter(synopsis_category != "DELETE") %>% 
  write_csv("data/police_reports/tampa_pd_calls_clean.csv", na = "")
