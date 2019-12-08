# Read all files and put them back together

load_sim_data <- function(){
  files <- fs::dir_ls(path = here::here("data"), glob = "*.csv")
  
  purrr::map_dfr(.x = files, readr::read_csv)
}
load_sim_data()
