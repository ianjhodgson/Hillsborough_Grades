library(tidyverse)
library(osfr)
rm(list = ls())

cr_project <- osf_retrieve_node("mv5e6")
osf_ls_files(cr_project)

candidate_data <- read_rds('ledb_candidatelevel.rds') %>% 
  filter(state_abb == "FL")

candidate_data %>% 
  distinct(geo_name, year) %>% 
  ggplot() + 
  geom_point(aes(x = as.character(year), y = geo_name, col = geo_name), alpha = .8, size = 4) + 
  theme_minimal()

candidate_data %>% 
  filter(!is.na(prob_republican),
         office_consolidated == "School Board") %>% 
  ggplot() + 
  geom_jitter(aes(x = year, 
               y = prob_republican,
               size = vote_share+.1, 
               col = winner), alpha = .5) + 
  facet_wrap(facets = "geo_name", ncol = 2) + 
  theme_minimal()

candidate_data %>% 
  filter(!is.na(prob_republican),
         office_consolidated == "School Board",
         geo_name == "Pinellas County") %>% 
  mutate(dist_from_middle = abs(prob_republican-.5)) %>% 
  ggplot() + 
  geom_jitter(aes(x = year, 
                  y = dist_from_middle,
                  size = vote_share+.1, 
                  col = winner), alpha = .5) + 
  facet_wrap(facet = "district") +
  geom_line(data = temp,
            aes(x=year, y = dist_from_middle), 
            alpha = .5, method = "lm", formula=y~x) + 
  theme_minimal()


sheriff <- candidate_data %>% 
  filter(office_consolidated == "Sheriff")


candidate_data %>% 
  filter(!is.na(contributor.cfscore)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = contributor.cfscore, col = winner), alpha = .5) + 
  geom_smooth(data = candidate_data %>% 
              filter(!is.na(contributor.cfscore),
                     winner == "win"),
            aes(x = year, y = contributor.cfscore), alpha = .5, method = "lm", formula = y~x) + 
  facet_wrap(facet = "pid_est") + 
  theme_minimal()
