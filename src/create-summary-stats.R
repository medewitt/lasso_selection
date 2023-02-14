library(tidyverse)
library(data.table)

dat <- rbindlist(lapply(fs::dir_ls(here::here("data")), fread))

dat[,list(num_pred, predictors_retained, true_contain)][,
lapply(.SD, mean), by = "num_pred"]
