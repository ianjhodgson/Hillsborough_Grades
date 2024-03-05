rm(list = ls())
library(readxl)
library(tidyverse)

## Grade 3-5 FAST PM2 Math and ElA ----

fast_sheets <- excel_sheets("data/FAST_2024/ela23_schools.xlsx")
fast_3_5 <- lapply(1:length(fast_sheets), 
                   function(i) {
                     temp <- read_xlsx("data/FAST_2024/ela23_schools.xlsx", 
                                       sheet = fast_sheets[i],
                                       skip = 3,
                                       na = "*") %>% 
                       janitor::clean_names() %>% 
                       mutate(exam = fast_sheets[i])
                   }) %>% 
  bind_rows()

## Grade 6-8 FAST PM2 Math and ELA ----
fast_sheets <- excel_sheets("data/FAST_2024/6-8 Performance by School on FAST PM2 2223_2324.xlsx")
fast_6_8 <- lapply(1:length(fast_sheets), 
                   function(i) {
                     temp <- read_xlsx("data/FAST_2024/6-8 Performance by School on FAST PM2 2223_2324.xlsx", 
                                       sheet = fast_sheets[i],
                                       skip = 3, na = "*") %>% 
                       janitor::clean_names() %>% 
                       mutate(exam = fast_sheets[i])
                   }) %>% 
  bind_rows()

## Grade 9-10 PM2 ELA ----
fast_sheets2 <- excel_sheets("data/FAST_2024/9-12 Performance by School on FAST PM2 2223_2324.xlsx")
fast_9_10 <- lapply(1:2, 
                    function(i) {
                      temp <- read_xlsx("data/FAST_2024/9-12 Performance by School on FAST PM2 2223_2324.xlsx", 
                                        sheet = fast_sheets2[i],
                                        skip = 3, 
                                        na = "*") %>% 
                        janitor::clean_names() %>% 
                        mutate(exam = fast_sheets[i])
                    }) %>% 
  bind_rows()

## combine years ----
fast <- fast_6_8 %>% 
  bind_rows(fast_9_10) %>% 
  bind_rows(fast_3_5)

fast_state_dist <- fast %>% 
  filter(school %in% c("State", "District"))

fast <- fast %>% 
  filter(!is.na(site_number))

names(fast) <- c("school_name",
                     "school_number", 
                     "region",
                     "level3_plus_22_23", 
                     "student_count_22_23", 
                     "level3_plus_23_24", 
                     "student_count_23_24",
                     "exam")

write_csv(fast, "data/FAST_2024/fast_2024_clean.csv")

### Aggregate analysis ----
fast_agg <- fast %>% 
  mutate(grade = str_extract(exam, "Grade [0-9]{1,2}"),
         subject = str_extract(exam, "Math|ELA$"),
         students_level3_plus_22_23 = level3_plus_22_23*student_count_22_23,
         students_level3_plus_23_24 = level3_plus_23_24*student_count_23_24) %>% 
  group_by(school_name, school_number, subject) %>% 
  summarise(students_level3_plus_23_24 = sum(students_level3_plus_23_24, na.rm = T),
            students_level3_plus_22_23 = sum(students_level3_plus_22_23, na.rm = T),
            student_count_22_23 = sum(student_count_22_23, na.rm = T),
            student_count_23_24 = sum(student_count_23_24, na.rm = T)) %>% 
  mutate(level3_plus_22_23 = students_level3_plus_22_23/student_count_22_23,
         level3_plus_23_24 = students_level3_plus_23_24/student_count_23_24)

