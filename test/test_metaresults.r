### Testing functions for saving meta results ###
# function in util.r

library(pacman)
pacman::p_load(tidyverse)
source("util.r")

results_df <- read_csv("test/test_saved_results.csv")

saved_meta_results <- save_results_meta(
    results_df = results_df,
    pp_final_expt_only = TRUE
)
