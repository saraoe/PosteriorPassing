### Testing functions in simulation.r ###

library(pacman)
pacman::p_load(tidyverse)
source("simulation.R")

# true values
b_base <- 0
b_sex <- 0
b_cond <- 0
b_sex_cond <- 2

var_shape <- 5
var_scale <- 0.1

# size parameters
n_experiments_per_repeat <- 60
n_participants_per_experiment <- 40
n_trials_per_participant <- 25
n_people <- 100000

## functions

population <- create_population(n_people, var_shape, var_scale)

data_sets <- create_datasets(
            population,
            n_people,
            n_participants_per_experiment,
            n_trials_per_participant,
            b_base,
            b_sex,
            b_cond,
            b_sex_cond
          )