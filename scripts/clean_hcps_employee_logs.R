rm(list = ls())

library(googlesheets4)
library(tidyverse)

employee_log_files <- list.files('data/hillsborough_district/employee_rolls/')

school_grades_hills <- read_csv('data/schoolgrades23_linked.csv') %>% 
  filter(charter_school == "NO", 
         alternative_ese_center_school == "N",
         district_name == "HILLSBOROUGH")

i = 1
temp <- readxl::read_xlsx(paste0('data/hillsborough_district/employee_rolls/',
                                 employee_log_files[i])) %>% 
  janitor::clean_names()


employee_data_raw <- lapply(1:length(employee_log_files), 
                        function(i){
                          readxl::read_xlsx(paste0('data/hillsborough_district/employee_rolls/',
                                                   employee_log_files[i])) %>% 
                            janitor::clean_names() %>% 
                            mutate(file_name = employee_log_files[i])
                        }) %>% 
  bind_rows()


employee_data <- employee_data_raw %>% 
  mutate(employee = ifelse(is.na(employee) & !is.na(employee_id),
                           employee_id, 
                           employee),
         last_name = ifelse(is.na(last_name) & !is.na(employee_name), 
                            str_extract(employee_name, 
                                        "^([a-z]|[A-Z]||-| |')*,") %>% 
                              str_remove(",$"),
                            last_name),
         school_year = str_extract(file_name, "[0-9]{4}-[0-9]{2}"))

### Sort positions into instructional and non-instructional staff
# employee_data %>% 
#   distinct(description) %>% 
#   write_csv("data/temp/HCPS_employee_rolls.csv", na = "")

employee_description <- read_csv("data/temp/HCPS_employee_rolls_DONE.csv")

### Clean up school names
school_numbers <- employee_data %>% 
  left_join(employee_description) %>% 
  filter(site_name %>% str_detect("High|Elementary|K-8|Middle|Elem|Tech"),
         !(site_name %>% str_detect("Region|Chief of|Dept$"))) %>% 
  distinct(site_name, site_number) %>% 
  arrange(site_name, site_number) %>% 
  group_by(site_name) %>% 
  fill(site_number) %>% 
  distinct() %>% 
  ## fill in missing numbers
  bind_rows(tribble(~site_name, ~site_number,
        "Rodgers Middle School", "3771",
        "Seminole Heights Elementary", "3921",
        "Pizzo K-8", "3381",
        "Monroe Middle", "2362",
        "Carrollwood K-8", "0701",
        "Collins PK-8", "0065",
        "Lutz Elementary", "2561",
        "Maniscalco Elementary", "2771",
        "Marshall Middle School", "2841",
        "Summerfield Crossing Elem", "0084")) %>% 
  arrange(site_name, site_number) %>% 
  group_by(site_name) %>% 
  fill(site_number) %>% 
  distinct() %>% 
  filter(!is.na(site_number))

employee_data_clean <- employee_data %>% 
  left_join(school_numbers %>% rename(school_number = site_number), 
            by = "site_name") %>% 
  mutate(role = case_when(
    !is.na(description) ~ description, 
    !is.na(assignment) ~ assignment,
    !is.na(job_description) ~ job_description,
    TRUE ~ "CHECK")) %>% 
  filter(role != "CHECK") %>% 
  select(school_year, site_name, school_number, employee, last_name, role, adj_hire_date, birthdate) %>% 
  left_join(employee_description, by = c("role" = "description")) %>%
  filter(instructional == T, !is.na(school_number))

## check employee number - LOOKS GOOD
emp_num_check <- employee_data_clean %>% 
  distinct(employee, birthdate) %>% 
  group_by(employee) %>% 
  mutate(count = n())

## normalize birth date and adjusted hire date -- assume oldest value is correct
employee_data_clean <- employee_data_clean %>% 
  group_by(employee) %>% 
  mutate(birthdate_cl = min(birthdate, na.rm = T), 
         adj_hire_date_cl = min(adj_hire_date, na.rm = T)) %>% 
  filter(school_number %in% school_grades_hills$school_number)

## calculate year-over-year turnover rate
years <- unique(employee_data_clean$school_year)
year_schools <- employee_data_clean %>% 
  distinct(school_year, school_number)


stay_rates <- lapply(1:(length(years)-1), 
       function(i){
  schools <- employee_data_clean %>% 
    ungroup() %>% 
    filter(school_year == years[i]) %>% 
    distinct(school_number, site_name) %>% 
    mutate(prev_year = school_number %in% year_schools$school_number[which(year_schools$school_year == years[i+1])])
  
  lapply(1:nrow(schools),
         function(j){
           if (schools$prev_year[j] == T){
             next_yr_staff <- employee_data_clean %>% 
               filter(school_year == years[i+1],
                      school_number == schools$school_number[j]) %>% 
               distinct(employee)
             staff <- employee_data_clean %>% 
               filter(school_year == years[i],
                      school_number == schools$school_number[j]) %>% 
               distinct(employee) %>% 
               mutate(stayed = employee %in% next_yr_staff$employee)
             tibble(school_year = years[i],
                    school_number = schools$school_number[j],
                    stay_rate_1yr = sum(staff$stayed)/nrow(staff),
                    stay_rate_inv_1yr = sum(staff$stayed)/nrow(next_yr_staff),
                    comp_year_1yr = years[i+1])
           } else {
             tibble(school_year = years[i],
                    school_number = schools$school_number[j],
                    stay_rate = NA)
           }
         }) %>% 
    bind_rows()
}) %>% 
  bind_rows

stay_rates_3yr <- lapply(1:(length(years)-3), 
                     function(i){
                       # i = 1
                       schools <- employee_data_clean %>% 
                         ungroup() %>% 
                         filter(school_year == years[i]) %>% 
                         distinct(school_number, site_name) %>% 
                         mutate(prev_year = school_number %in% year_schools$school_number[which(year_schools$school_year == years[i+3])])
                       
                       lapply(1:nrow(schools),
                              function(j){
                                # j=1
                                if (schools$prev_year[j] == T){
                                  
                                  next_3yr_staff <- employee_data_clean %>% 
                                    filter(school_year == years[i+3],
                                           school_number == schools$school_number[j]) %>% 
                                    distinct(employee)
                                  next_yr_staff <- employee_data_clean %>% 
                                    filter(school_year == years[i+1],
                                           school_number == schools$school_number[j]) %>% 
                                    distinct(employee)
                                  staff <- employee_data_clean %>% 
                                    filter(school_year == years[i],
                                           school_number == schools$school_number[j]) %>% 
                                    distinct(employee) %>% 
                                    mutate(stayed = employee %in% next_yr_staff$employee,
                                           stayed_3yr = employee %in% next_3yr_staff$employee)
                                  tibble(school_year = years[i],
                                         school_number = schools$school_number[j],
                                         stay_rate_1yr_check = sum(staff$stayed)/nrow(staff),
                                         stay_rate_3yr = sum(staff$stayed_3yr)/nrow(staff),
                                         stay_rate_inv_3yr = sum(staff$stayed_3yr)/nrow(next_3yr_staff),
                                         comp_year = years[i+3])
                                } else {
                                  tibble(school_year = years[i],
                                         school_number = schools$school_number[j],
                                         stay_rate_3yr = NA,
                                         comp_year_3yr = years[i+2])
                                }
                              }) %>% 
                         bind_rows()
                     }) %>% 
  bind_rows

check_rates <- left_join(stay_rates, 
                         stay_rates_3yr)

write_csv(check_rates, "data/hillsborough_school_staff_turnover_rates.csv", na = "")

stay_rates <- left_join(stay_rates, 
                        school_grades_hills %>% 
                          select(school_number, school_name, informational_baseline_grade_2023))

employees_2018 <- unique(employee_data$employee[which(employee_data$school_year == "2018-19")])
employees_2019 <- unique(employee_data$employee[which(employee_data$school_year == "2019-20")])
new_eployees_2019 <- employees_2019[!(employees_2019 %in% employees_2018)]


