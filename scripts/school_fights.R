library(tidyverse)
library(readxl)

school_grades <- read_csv('data/schoolgrades23_linked.csv') %>% 
  # NOTE Monroe Middle was changed late from an "I" for incomeplete to a "D" school.
  mutate(informational_baseline_grade_2023 = ifelse(school_number == "2362", "D",
                                                    informational_baseline_grade_2023)) %>% 
  filter(charter_school == "NO", 
         alternative_ese_center_school == "N")

enrollment_2122 <- read_xlsx("data/FLDOE/Enrollment/2122MembBySchoolByGrade.xlsx",
                             sheet = 2,
                             skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(district %in% c("HILLSBOROUGH",
                         "PINELLAS", 
                         "PASCO")) %>% 
  mutate(sch_number = as.numeric(school_number),
         number_of_students = as.numeric(number_of_students)) %>% 
  select(district, sch_number, number_of_students)

enrollment_2223 <-read_xlsx("data/FLDOE/Enrollment/2223MembBySchoolByGrade.xlsx",
                            sheet = 2,
                            skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(district == "HILLSBOROUGH") %>% 
  mutate(sch_number = as.numeric(school_number),
         number_of_students = as.numeric(number_of_students)) %>% 
  select(district, sch_number, number_of_students)

sesir <- read_xlsx("data/FLDOE/Discipline/sesir2223a-h.xlsx", 
                   sheet = 2, 
                   skip = 2) %>% 
  janitor::clean_names() %>% 
  filter(district_name %in% c("HILLSBOROUGH",
                              "PINELLAS", 
                              "PASCO")) %>% 
  left_join(school_grades %>% 
              mutate(school_number = as.numeric(school_number)) %>%
              select(district_name, school_number,
                     Grade = informational_baseline_grade_2023,
                     mathematics_achievement,
                     english_language_arts_achievement, 
                     type),
            by = c("district_name", "sch_number" = "school_number")) %>%
  filter(Grade != "I") %>% 
  left_join(enrollment_2223, by = c("district_name" = "district", "sch_number")) %>% 
  mutate(school_year = "2022-23")

sesir_2122 <- read_xls("data/FLDOE/Discipline/sesir2122a-h.xls", 
                       sheet = 2, 
                       skip = 2) %>% 
  janitor::clean_names() %>% 
  rename(incident_type = type_of_incident) %>% 
  filter(district_name == "HILLSBOROUGH") %>% 
  left_join(school_grades %>% 
              mutate(school_number = as.numeric(school_number)) %>%
              select(district_name, school_number,
                     Grade = informational_baseline_grade_2023,
                     mathematics_achievement,
                     english_language_arts_achievement, 
                     type),
            by = c("district_name", "sch_number" = "school_number")) %>%
  filter(Grade != "I") %>% 
  left_join(enrollment_2122, by = c("district_name" = "district", "sch_number")) %>% 
  mutate(school_year = "2021-22")


sesir_join <- sesir %>% 
  bind_rows(sesir_2122) %>% 
  arrange(district_name, sch_number, incident_type, school_year) %>% 
  mutate(incidents_per_100 = total_incidents/number_of_students)

type_totals <- sesir_join %>% 
  group_by(district_name, school_year, incident_type) %>% 
  summarise(total_incidents = sum(total_incidents),
            incidents_leo = sum(incidents_reported_to_law_enforcement),
            students = sum(number_of_students)) %>% 
  mutate(incidents_per_100 = total_incidents/students*1000, 
         leo_per_100 = incidents_leo/students*1000, 
         leo_rate = incidents_leo/total_incidents) %>% 
  arrange(district_name, incident_type)

type_totals %>% 
  ggplot() + 
  geom_point(aes(x = leo_rate, 
                 y = incident_type, 
                 col = school_year))
