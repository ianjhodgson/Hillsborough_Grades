rm(list = ls())

library(sf)
library(tidyverse)
sf_use_s2(F)

elem_zones <- st_read("data/shape files/Elementary") %>% 
  mutate(district_area = st_area(.))
middle_zones <- st_read("data/shape files/Middle") %>% 
  mutate(district_area = st_area(.))
school_grades <- read_csv('data/schoolgrades23_linked.csv') %>% 
  filter(charter_school == "NO", 
         alternative_ese_center_school == "N",
         district_name == "HILLSBOROUGH")
## poverty data
poverty <- st_read('data/census/acs2022_5yr_B17020_14000US12057011514 2/acs2022_5yr_B17020_14000US12057011514.geojson')
poverty_u18 <- poverty %>% 
  select(geoid, name,
         pov_6_11 = B17020004, 
         pov_6_11_error = B17020004..Error,
         pov_12_17 = B17020005,
         pov_12_17_error = B17020005..Error,
         npov_6_11 = B17020012, 
         npov_6_11_error = B17020012..Error,
         npov_12_17 = B17020013, 
         npov_12_17_error = B17020013..Error,
         geometry) %>% 
  mutate(tract_area = st_area(.))

## ELEMENTARY ----

## join census and school area data
intersect_pct <- st_intersection(elem_zones, 
                                 poverty_u18 %>% 
                                   filter(str_detect(name, "Census")))

intersect_pct_2 <- intersect_pct %>% 
  mutate(intersect_area = st_area(.)) %>% 
  select(EID, name, intersect_area, district_area, tract_area) %>% 
  st_drop_geometry() %>% 
  mutate(coverage_share = intersect_area/tract_area) %>% 
  group_by(name) %>% 
  mutate(max_coverage = max(coverage_share, na.rm = T)) %>% 
  select(EID, name, coverage_share)

joined <- st_join(elem_zones, poverty_u18) %>% 
  left_join(intersect_pct_2) %>% 
  filter(name %>% str_detect("Census")) %>% 
  mutate(pov_6_17 = (pov_6_11 + pov_12_17)*coverage_share, 
         pov_6_17_error_sq = (pov_6_11_error^2 + pov_12_17_error^2)*coverage_share,
         npov_6_17 = (npov_6_11 + npov_12_17)*coverage_share, 
         npov_6_17_error_sq = (npov_6_11_error^2 + npov_12_17_error^2)*coverage_share) %>% 
  st_drop_geometry() %>% 
  group_by(EID, EID_NAME) %>% 
  summarise(pov_6_17 = sum(pov_6_17, na.rm = T),
            pov_6_17_error_sq = sum(pov_6_17_error_sq, na.rm = T),
            npov_6_17 = sum(npov_6_17, na.rm = T),
            npov_6_17_error_sq = sum(npov_6_17_error_sq, na.rm = T)) %>% 
  mutate(pov_6_17_error = pov_6_17_error_sq^.5,
         npov_6_17_error = npov_6_17_error_sq^.5,
         pov_share = pov_6_17/(pov_6_17 + npov_6_17))

grades <- school_grades %>% left_join(joined %>% select(school_number = EID, pov_share))

grades %>% filter(type == "Elementary" | 
                    type == "Combination", 
                  !is.na(pov_share)) %>% 
  select(school_name, informational_baseline_grade_2023, pov_share) %>% 
  group_by(informational_baseline_grade_2023) %>% 
  summarise(min_pov = min(pov_share),
            mean_pov = mean(pov_share), 
            max_pov = max(pov_share))

grades %>% filter(informational_baseline_grade_2023 %in% c("F", "D"), !is.na(pov_share)) %>% 
  select(school_name, grade =informational_baseline_grade_2023, pov_share) %>% write_csv("data/child_poverty_rates.csv")

### Middle ---- 
intersect_pct_mid <- st_intersection(middle_zones, 
                                 poverty_u18 %>% 
                                   filter(str_detect(name, "Census")))

intersect_pct_2_mid <- intersect_pct_mid %>% 
  mutate(intersect_area = st_area(.)) %>% 
  select(MID, name, intersect_area, district_area, tract_area) %>% 
  st_drop_geometry() %>% 
  mutate(coverage_share = intersect_area/tract_area) %>% 
  group_by(name) %>% 
  mutate(max_coverage = max(coverage_share, na.rm = T)) %>% 
  select(MID, name, coverage_share)

joined_mid <- st_join(middle_zones, poverty_u18) %>% 
  left_join(intersect_pct_2_mid) %>% 
  filter(name %>% str_detect("Census")) %>% 
  mutate(pov_6_17 = (pov_6_11 + pov_12_17)*coverage_share, 
         pov_6_17_error_sq = (pov_6_11_error^2 + pov_12_17_error^2)*coverage_share,
         npov_6_17 = (npov_6_11 + npov_12_17)*coverage_share, 
         npov_6_17_error_sq = (npov_6_11_error^2 + npov_12_17_error^2)*coverage_share) %>% 
  st_drop_geometry() %>% 
  group_by(MID, MID_NAME) %>% 
  summarise(pov_6_17 = sum(pov_6_17, na.rm = T),
            pov_6_17_error_sq = sum(pov_6_17_error_sq, na.rm = T),
            npov_6_17 = sum(npov_6_17, na.rm = T),
            npov_6_17_error_sq = sum(npov_6_17_error_sq, na.rm = T)) %>% 
  mutate(pov_6_17_error = pov_6_17_error_sq^.5,
         npov_6_17_error = npov_6_17_error_sq^.5,
         pov_share = pov_6_17/(pov_6_17 + npov_6_17))

grades_mid <- school_grades %>% filter(district_name == "HILLSBOROUGH", type == "Middle") %>% 
  left_join(joined_mid %>% 
                                            select(school_number = MID, pov_share)) %>% 
  select(school_name, school_number, informational_baseline_grade_2023, type, pov_share)

grades %>% filter(type == "Elementary", district_name == "HILLSBOROUGH") %>% 
  select(school_name, school_number, informational_baseline_grade_2023, type, pov_share) %>%
  bind_rows(grades_mid) %>% 
  write_csv("data/child_poverty_rates.csv")
