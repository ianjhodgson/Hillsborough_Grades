rm(list = ls())

library(readxl)
library(janitor)
library(tidyverse)

### SCHOOL GRADES ----
school_grades <- read_csv('data/schoolgrades23_linked.csv') %>% 
  # NOTE Monroe Middle was changed late from an "I" for incomeplete to a "D" school.
  mutate(informational_baseline_grade_2023 = ifelse(school_number == "2362", 
                                                    "D",
                                                    informational_baseline_grade_2023),
         Grade = ifelse(informational_baseline_grade_2023 %in% c("D", "F"), "D/F",
                        informational_baseline_grade_2023)) %>% 
  filter(district_name == "HILLSBOROUGH",
         # informational_baseline_grade_2023 %in% c("D", "F"),
         type %in% c("Middle", "Elementary", "Combination"), 
         charter_school == "NO") %>% 
  select(school_number, 
         school_name, 
         type,
         informational_baseline_grade_2023,
         Grade)
  
### Inexperienced Teachers----
inexperienced <- read_csv("data/FLDOE/Report Cards/inexperienced.csv") %>% 
  mutate(school_number = as.numeric(s),
         inexperienced_teachers = inexp_r_tot/100,
         ineffective_teachers = ineff_r_tot/100) %>% 
  filter(sub == "Teachers",
         y == 2223) %>% 
  select(school_number, 
         inexperienced_teachers,
         ineffective_teachers)

school_grades %>% 
  mutate(school_number = as.numeric(school_number)) %>% 
  left_join(inexperienced) %>% 
  filter(type != "High", type != "Combination") %>% 
  group_by(Grade) %>% 
  summarise(inexperienced_teachers = mean(inexperienced_teachers, na.rm = T))

### Substitute ----

teachers <- read_xls("data/nces/ncesdata_294E6233_hillsborough.xls", 
                     skip = 14, 
                     na = "-") %>% 
  janitor::clean_names() %>% 
  mutate(school_number = as.numeric(str_extract(state_school_id, "[0-9]{4}$")),
         teachers = as.numeric(teachers)) %>% 
  select(school_number, teachers)

kelly_subs <- read_csv("data/kelly_subs_clean.csv") %>% 
  mutate(school_number = str_extract(school, "[0-9]{4}"),
         school_number_cln = case_when(
           is.na(school_number) ~"0011",
           school_number == "0155" ~ "0151", # Alonso
           school_number == "0285" ~ "0281", # Blake, 
           school_number == "0845" ~ "0842", # Dowdell
           school_number == "1885" ~ "1881", # Hillsborough High 
           school_number == "3785" ~ "3784", # Jefferson
           school_number == "2245" ~ "2241", # King
           school_number == "8256" ~ "2561", # Lutz
           school_number == "3005" ~ "3004", # Middleton
           school_number == "3735" ~ "3731", # Robinson
           school_number == "1485" ~ "1482", # Sligh
           school_number == "0094" ~ "0093", # Strawberry
           school_number == "8068" ~ "0682", # Woodson
           school_number == "4225" ~ "4221", # Tampa Bay Tech
           T ~ school_number
         ) %>% 
           str_squish())

kelly_subs_clean <- kelly_subs %>% 
  mutate(date_clean = mdy(date)) %>% 
  filter(date_clean >= mdy("8/10/2022"),
         date_clean <= mdy("5/26/2023")) %>% 
  group_by(school_number_cln) %>%
  mutate(total_hours = reg_units_hrs + overtime_units_hrs,
         days = ifelse(reg_units_hrs > 0, 1, -1)) %>% 
  summarise(total_hours = sum(total_hours, na.rm = T),
            total_days = sum(days),
            total_8hr_days = round(total_hours/8, 0)) %>% 
  mutate(school_number = as.numeric(school_number_cln)) %>% 
  left_join(teachers) %>% 
  mutate(teacher_hours = teachers*180*7.5, ## Rob Kriete - net out 30 min for lunch
         teacher_days = teachers*180, 
         sub_hours_pct = total_hours/teacher_hours,
         sub_hours_per_teacher = total_hours/teachers) %>% 
  select(school_number, share_days_with_sub = sub_hours_pct)

kelly_subs_long_term <- kelly_subs %>% 
  mutate(date_clean = mdy(date)) %>% 
  filter((full_time_teacher %>% str_detect("^999") | 
            grade_subject %>% str_detect("VAC")),
         reason_for_absence %>% str_detect("INS|Ins"),
         date_clean >= mdy("8/10/2022"),
         date_clean <= mdy("5/26/2023")) %>% 
  group_by(school_number_cln) %>%
  summarise(total_hours = sum(reg_units_hrs)) %>% 
  mutate(school_number = as.numeric(school_number_cln)) %>% 
  left_join(teachers) %>% 
  mutate(teacher_hours = teachers*180*7.5,
         sub_hours_pct = round(total_hours/teacher_hours, 3)) %>% 
  select(school_number, sub_hours_pct)

school_grades %>%
  filter(type != "High") %>% 
  mutate(school_number = as.numeric(school_number)) %>%
  left_join(kelly_subs_long_term) %>%
  group_by(Grade) %>%
  summarise(sub_hours_pct = mean(sub_hours_pct, na.rm = T)) %>%
  arrange(Grade)

### Students who leave ----

retention <- read_xlsx("data/InOutReports/Student Migration_ Out 2022-23.xlsx",
                       sheet = 1) %>% 
  janitor::clean_names() %>% 
  filter(is.na(grade)) %>% 
  mutate(school_number = str_extract(assigned_school_clean, "[0-9]{4}"),
         assigned_school_clean = str_remove(assigned_school_clean, " \\([0-9]{4}\\)") %>% 
           str_remove_all("Elementary|Middle|High|School|K-8|Elem|Magnet") %>% 
           str_replace_all("\\/", " ") %>% 
           str_to_upper() %>% 
           str_squish(),
         current_school_clean = str_remove_all(current_school_clean, 
                                               "Elementary|Middle|High|School|K-8|Magnet|Community") %>%
           str_to_upper() %>% 
           str_squish(),
         name_check = assigned_school_clean == current_school_clean,
         name_check = case_when(
           name_check == T ~ T,
           assigned_school_clean == "TOWN 'N' COUNTRY" & current_school_clean == "TOWN AND COUNTRY" ~ T,
           assigned_school_clean == "SGT. SMITH" & current_school_clean == "SMITH" ~ T,
           assigned_school_clean == "HUNTERS GREEN" & current_school_clean == "HUNTER'S GREEN" ~ T, 
           assigned_school_clean == "WOODSON" & current_school_clean == "DR CARTER G WOODSON" ~ T,
           assigned_school_clean == "SUMMERFIELD CROSSINGS" & current_school_clean == "SUMMERFIELD CROSSINGS ELEM" ~ T, 
           assigned_school_clean == "DAWSON" & current_school_clean == "WARREN HOPE DAWSON" ~ T,
           assigned_school_clean == "BRYAN" & current_school_clean == "BRYAN PLANT CITY" ~ T,
           assigned_school_clean == "B. T. WASHINGTON" & current_school_clean == "BT WASHINGTON" ~ T,
           T ~ F)) %>% 
  group_by(assigned_school_clean) %>% 
  mutate(at_least_one_name_match = max(name_check, na.rm = T))

retention_check <- retention %>% 
  filter(total_row == T) %>% 
  group_by(assigned_school_clean) %>% 
  mutate(assinged = max(total))

retention_stay_leave <- retention %>% 
  filter(!is.na(current_school_clean)) %>% 
  group_by(name_check, assigned_school_clean, school_number) %>% 
  summarise(students = sum(total, na.rm = T)) %>% 
  left_join(retention %>% 
              filter(is.na(placement_code_clean)) %>% 
              select(school_number, total),
            by = "school_number") %>% 
  ungroup() %>% 
  group_by(school_number) %>% 
  mutate(share = students/total) %>% 
  left_join(school_grades %>%
              select(school_number, school_name),
            by = "school_number") %>% 
  filter(name_check == F, 
         !is.na(school_name)) %>% 
  ungroup() %>% 
  mutate(school_number = as.numeric(school_number),
         students_who_leave = round(share, 3)) %>% 
  select(school_number,
         school_name, 
         students_who_leave)

### Child poverty rate ----
lunch_status <- read_xlsx("data/FLDOE/Lunch Status/2223FS3-Lunch-Status-School.xlsx", 
                          sheet = 2, 
                          skip = 2,
                          na = "*") %>% 
  janitor::clean_names() %>% 
  filter(district_name == "HILLSBOROUGH") %>% 
  mutate(free_lunch = frl_wo_usda_mult/as.numeric(number_of_students_denominator) %>% 
           round(3),
         school_number = as.numeric(school_number)) %>% 
  select(school_number, free_lunch)

### Absenteeism ----
absent <- read_xlsx('data/FLDOE/Absenteeism/2223ABS21Days10Comparison.xlsx', 
                    sheet = 2, 
                    skip = 2,
                    na = "*") %>% 
  janitor::clean_names() %>% 
  filter(district_name == "HILLSBOROUGH") %>% 
  mutate(school_number = as.numeric(school_number),
         percent_of_students_absent_10_percent_or_more = round(percent_of_students_absent_10_percent_or_more, 3))

### Combine data ----
scorecard <- school_grades %>% 
  mutate(school_number = as.numeric(school_number)) %>% 
  left_join(inexperienced) %>% 
  left_join(kelly_subs_long_term) %>% 
  left_join(retention_stay_leave) %>% 
  left_join(absent %>% 
              select(school_number, percent_of_students_absent_10_percent_or_more)) %>% 
  left_join(lunch_status) %>% 
  select(-Grade, -ineffective_teachers)

scorecard_averages_A <- scorecard %>% 
  group_by(type) %>% 
  filter(informational_baseline_grade_2023 == "A") %>% 
  select(type, 5:9) %>% 
  summarise_all(.funs = function(x){mean(x, na.rm = T) %>% round(3)})

names(scorecard_averages_A)[2:6] <- paste0(names(scorecard_averages_A)[2:6], "_A_ave")

scorecard_averages <- scorecard %>% 
  group_by(type) %>% 
  select(type, 5:9) %>% 
  summarise_all(.funs = function(x){mean(x, na.rm = T) %>% round(3)})

names(scorecard_averages)[2:6] <- paste0(names(scorecard_averages)[2:6], "_ave")
  
scorecard_df <- scorecard %>% 
  filter(informational_baseline_grade_2023 %in% c("D", "F")) %>% 
  left_join(scorecard_averages, by = "type") %>% 
  distinct() %>% 
  select(-school_number, -starts_with("ineff")) %>% 
  mutate(school_name_clean = school_name %>%
           str_remove(" SCHOOL") %>% 
           str_to_title()) %>% 
  write_csv("data/charts/scorecard_data.csv", na = "-")

#### DATAWRAPPER ----
# library(googledrive)
# library(googlesheets4)
drive_auth()
gs4_auth()

library(DatawRappr)
datawrapper_auth(api_key = "cpFLKz3GLUSCQ88HydLzLpmN7Vy7wV11920mRrkdQiexE1cvGOTOoxgNCqE2YWpI")
dw_test_key()

## Function to create data send
dw_data_template <- function(i) {
  temp <- tribble(~school_name, ~grade,
                  "Issue *(District average)*", "_",
                  #Inexperienced Teachers
                  glue("Inexperienced teachers *(Average: {round(100*scorecard_df$inexperienced_teachers_ave[i])}%)*"),
                  glue("**{round(100*scorecard_df$inexperienced_teachers[i])}%**"),
                  #Substitutes
                  glue("Time with substitutes *({round(100*scorecard_df$sub_hours_pct_ave[i])}%)*"),
                  glue("**{round(100*scorecard_df$sub_hours_pct[i])}**%"),
                  #Students leaving
                  glue("Zoned students choosing another school *({round(100*scorecard_df$students_who_leave_ave[i])}%)*"),
                  glue("**{round(100*scorecard_df$students_who_leave[i])}%**"),
                  #Chronically Absent
                  glue("Chronically absent *({round(100*scorecard_df$percent_of_students_absent_10_percent_or_more_ave[i])}%)*"),
                  glue("**{round(100*scorecard_df$percent_of_students_absent_10_percent_or_more[i])}%**"),
                  #Free lunch
                  glue("Free or reduced-price lunch *({round(100*scorecard_df$free_lunch_ave[i])}%)*"),
                  glue("**{round(100*scorecard_df$free_lunch[i])}%**"))
  names(temp) <- c(scorecard_df$school_name_clean[i], 
                   glue('<span style="color:red; font-size: 175%;">{scorecard_df$informational_baseline_grade_2023[i]}</span>'))
  temp
} 

## Mess with Sheehy
sheehy_data_orig <- dw_data_from_chart("gDxsG")
dw_data_to_chart(dw_data_template(1),
                 "gDxsG")

dw_retrieve_chart_metadata("gDxsG")

## test duplicate Sheehy with Kimbell
# kimbell_chart <- dw_copy_chart(copy_from = "gDxsG")
kimbell_id <- "ZyLh8"
dw_edit_chart(kimbell_id, 
              title = scorecard_df$school_name_clean[2],
              folderId = 237630)
dw_data_to_chart(x = dw_data_template(2),
                 chart_id = kimbell_id)

kimbell_chart_meta <- dw_retrieve_chart_metadata(kimbell_id)

## test2 duplicate Kimbell with Lamb
lamb_chart <- dw_copy_chart(copy_from = kimbell_id)
dw_edit_chart(lamb_chart$id,
              title = scorecard_df$school_name_clean[3],
              folderId = 237630)
dw_data_to_chart(x = dw_data_template(3),
                 chart_id = lamb_chart$id)


remaining_charts <- lapply(3:nrow(scorecard_df),
                           function(j){ 
                             current_chart <- dw_copy_chart(copy_from = kimbell_id)
                             dw_edit_chart(current_chart$id,
                                           title = scorecard_df$school_name_clean[j],
                                           folderId = 237630)
                             dw_data_to_chart(x = dw_data_template(j),
                                              chart_id = current_chart$id)
                             current_chart})


remaining_charts[[2]]$id


## update charts 
chart_ids <- c("gDxsG", "ZyLh8") %>% 
  append(lapply(1:length(remaining_charts),
       function(x){
         remaining_charts[[x]]$id
       }) %>% as.list())
lapply(1:33,
       function(k){
         dw_data_to_chart(chart_id = chart_ids[k],
                          x = dw_data_template(k))})

chart_id_lookup <- lapply(1:33, 
                          function(n){
                            temp <- dw_retrieve_chart_metadata(chart_id = chart_ids[n])
                            tibble(school_name = temp$content$title)
                          }) %>% 
  bind_rows() %>% 
  mutate(chart_id = chart_ids) %>% 
  as.data.frame()

write_csv(chart_id_lookup, 
          "data/charts/datawrapper_chart_id_lookup.csv")





  