library(tidyverse)
library(rvest)
library(httr2)
library(glue)

schools <- read_csv('data/schoolgrades23_linked.csv') %>% 
  janitor::clean_names() %>% 
  filter(district_name == "HILLSBOROUGH") %>% 
  select(school_number, school_name, type, Grade = informational_baseline_grade_2023)

request_builder <- function(school_number){
  request(glue("https://edudata.fldoe.org/api/RCContent/GetExpeff/29{school_number}")) %>% 
    req_headers("Referer" = glue("https://edudata.fldoe.org/ReportCards/Schools.html?school={school_number}&district=29"),
                "Accept" = "application/json, text/javascript, */*; q=0.01",
                "Accept-Encoding" = "gzip, deflate, br, zstd") %>% 
    req_retry(max_tries = 10)
}

request_cleaner <- function(i){
  req <- request_builder(i)
  resp_raw <- req_perform(req) %>% 
    resp_body_json()
  lapply(1:length(resp_raw),
                    function(j){
                      as_tibble(resp_raw[[j]])
                    }) %>% 
    bind_rows() %>% 
    filter(s == i)
}

teacher_data <- lapply(schools$school_number,
                       request_cleaner) %>% 
  bind_rows %>% 
  write_csv("data/FLDOE/Report Cards/inexperienced.csv")
