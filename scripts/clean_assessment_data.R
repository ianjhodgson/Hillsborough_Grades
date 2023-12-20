library(tidyverse)
library(readxl)
library(janitor)
library(stringdist)

## English Language Arts 2023 ----

## read in ela 2023 scores and combine grade levels into one dataset
ela23_files <- list.files('data/ELASpring23/')
ela23_raw <- lapply(ela23_files, 
                    function(x){
                      name <- paste0('data/ELASpring23/', x)
                      temp <- read_xls(name, 
                                       skip = 4,
                                       na = "*") %>% 
                        clean_names()
                    }) %>% 
  bind_rows() %>% 
  write_csv('data/ela23.csv')

## condense grade-level results to schools
ela23_schools <- ela23_raw %>% 
  group_by(district_name, school_name, district_number, school_number) %>%
  mutate(ela_level1_num = x1*number_of_students/100, 
         ela_level2_num = x2*number_of_students/100,
         ela_level3_num = x3*number_of_students/100,
         ela_level4_num = x4*number_of_students/100,
         ela_level5_num = x5*number_of_students/100) %>% 
  summarise(across(c('number_of_students', ends_with('num')), ~ round(sum(.x, na.rm = T)))) %>% 
  mutate(ela_level1 = ela_level1_num/number_of_students, 
         ela_level2 = ela_level2_num/number_of_students,
         ela_level3 = ela_level3_num/number_of_students,
         ela_level4 = ela_level4_num/number_of_students,
         ela_level5 = ela_level5_num/number_of_students) %>%
  filter(!str_detect(district_name, 'STATE TOTAL')) %>% 
  write_csv("data/ela23_schools.csv")

## Mathematics Spring 2023 ---- 
math23_files <- list.files('data/MathSpring23/')
math23_raw <- lapply(math23_files, 
                    function(x){
                      name <- paste0('data/MathSpring23/', x)
                      temp <- read_xls(name, 
                                       skip = 4,
                                       na = "*") %>% 
                        clean_names()
                    }) %>% 
  bind_rows() %>% 
  write_csv('data/math23.csv')

math23_schools <- math23_raw %>% 
  group_by(district_name, school_name, district_number, school_number) %>%
  mutate(math_level1_num = x1*number_of_students/100, 
         math_level2_num = x2*number_of_students/100,
         math_level3_num = x3*number_of_students/100,
         math_level4_num = x4*number_of_students/100,
         math_level5_num = x5*number_of_students/100) %>% 
  summarise(across(c('number_of_students', ends_with('num')), ~ round(sum(.x, na.rm = T)))) %>% 
  mutate(math_level1 = math_level1_num/number_of_students, 
         math_level2 = math_level2_num/number_of_students,
         math_level3 = math_level3_num/number_of_students,
         math_level4 = math_level4_num/number_of_students,
         math_level5 = math_level5_num/number_of_students) %>%
  filter(!str_detect(district_name, 'STATE TOTAL')) %>% 
  rename(number_of_students_math = number_of_students) %>% 
  write_csv("data/math23_schools.csv")

## Science spring 2023 ---- 



## School grades 2023 ----

school_grades <- lapply(excel_sheets('data/SchoolGrades23.xlsx'), 
                        function(x){read_xlsx('data/SchoolGrades23.xlsx', 
                                  sheet = x) %>% 
                            mutate(type = x)}) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  filter(school_name != "Edgenuity Grades KG-12") %>% 
  select(-school_name) %>% 
  left_join(ela23_schools,
            by = c("district_number",
                   "school_number",
                   "district_name")) %>% 
  left_join(math23_schools) %>% 
  write_csv('data/schoolgrades23_linked.csv')
  

         