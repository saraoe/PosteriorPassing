### Testing functions in analysis.r ###
library(pacman)
pacman::p_load(brms, dplyr, boot, igraph, tidyverse)
source("analysis.r")
source("util.r")

data_sets <- read_csv("test/test_data_sets.csv") %>%
    filter(
        true_sex_cond == 1 &
            participant_id <= 10 &
            data_set < 3
    )

pb_prob_pos <- 0.9 # prob if b above zero and b lower above zero
pb_prob_null <- 0.2 # prob if b above zero and b lower below zero OR b below zero and b upper above zero
pb_prob_neg <- 0.6 # prob if b below zero and b upper below zero

# citation chain
citation_chain <- read_csv("test/test_citation_chain.csv")

analysis_df <- do_analyses(
    data_sets = data_sets,
    do_pp_linear = TRUE,
    do_pp_citation = FALSE,
    do_publication_bias = FALSE,
    pb_prob_pos = pb_prob_pos,
    pb_prob_neg = pb_prob_neg,
    pb_prob_null = pb_prob_null,
    citation_chain = citation_chain
)

analysis_df <- analysis_df %>%
    mutate(
        repeat_id = 1,
        true_base = 0,
        true_sex = 0,
        true_cond = 0,
        true_sex_cond = 1,
        var_pop = .5
    )
