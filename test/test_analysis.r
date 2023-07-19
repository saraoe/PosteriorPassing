### Testing functions in analysis.r ###
library(pacman)
pacman::p_load(brms, dplyr, boot, igraph, tidyverse)
source("analysis1.r")

data_sets <- read_csv("test/test_data_sets.csv") %>%
    filter(
        true_sex_cond == 1 &
        participant_id <= 10
    )

analysis_df <- do_analyses(
    data_sets = data_sets,
    do_pp_linear = TRUE,
    do_pp_citation = TRUE,
    do_publication_bias = TRUE
    )

analysis_df <- analysis_df %>%
                    mutate(
                      repeat_id = 1,
                      true_base = 0,
                      true_sex = 0,
                      true_cond = 0,
                      true_sex_cond = -2
                    )