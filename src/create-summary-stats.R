library(tidyverse)
library(data.table)

dat <- rbindlist(lapply(fs::dir_ls(here::here("data")), fread))

head(dat)

tab_results <- dat[,list(true_beta, error, num_pred, predictors_retained, true_contain, rho)][,
lapply(.SD, mean), by = c("num_pred", "rho", "true_beta", "error")][order(num_pred, rho, true_beta,error)] |>
knitr::kable()



c("# Lasso Simulation\n",
"This repository contains simulations regarding the ability of the Lasso to detect the true parameters of interest with varying correlation, effect size, and number of predictors within a given data set.\n" ,
"Performance results:\n",
tab_results,
"\nGenerally, the Lasso selects too many parameters (only six parameters actually create the model, the rest are noise). As the signal to noise ratio decreases (i.e., small effect size compared to data variance (error)) the ability of the Lasso to recover the true model parameters decreases.") |>
writeLines("README.Md")
