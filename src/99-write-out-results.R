library(tidyverse)

dat <- read_rds(here::here("data", "lasso_sim_results.rds"))

dat %>% 
  group_split(model_run) %>% 
  map2(., here::here("data", sprintf("run-%s.csv",1:36)), write_csv)


