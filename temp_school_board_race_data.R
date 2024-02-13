library(tidyverse)
library(osfr)

cr_project <- osf_retrieve_node("mv5e6")
osf_ls_files(cr_project)

candidate_data <- read_rds('ledb_candidatelevel.rds') %>% 
  filter(state_abb == "FL",
         office_consolidated == "School Board")

candidate_data %>% 
  distinct(geo_name, year) %>% 
  ggplot() + 
  geom_point(aes(x = as.character(year), y = geo_name, col = geo_name), alpha = .8, size = 4) + 
  theme_minimal()
