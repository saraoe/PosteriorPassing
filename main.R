#!/usr/bin/env Rscript
# imports
library(pacman)
pacman::p_load(tidyverse, brms, dplyr, boot, igraph)
source("util.r")
source("simulation.R")
source("analysis.r")

###       ###
### SETUP ###
###       ###

###            ###
### Parameters ###
###            ###

### True values
# these set the true values of the population
# putting multiple values in will cause simulations to run
# with different true values.
# The first four give the mean value of the four parameters
# that determine participant behaviour. The next two allow you
# to determine the shape and scale of the gamma distribution that
# that the individual variance of the simulated population
# will be randomly drawn from for each repeat.
b_bases <- c(0)
b_sexs <- c(0)
b_conds <- c(0)
b_sex_conds <- c((-2), (-1), 0, 1, 2)

var_shape <- 5
var_scale <- 0.1

### Citation chain
# Here you can insert an edgelist. Based on the degree
# distribution of this edgelist, a citation_chain where
# each node represents a study and the connections represent
# citations between studies through which posteriors can
# be passed. The size of this citation_chain can be specified
# in size parameters below (number of nodes specified in
# n_experiments_per_repeat below).
citation_list <- read_csv("my_edgelist.csv")
citation_list <- graph_from_data_frame(d = citation_list, directed = T)

### Size parameters
# These determine the size of the simulations for every set of
# parameter values given in the "True values" section above.
# Warning! n_participants_per_experiment and n_people need to
# be divisible by 4!
n_repeats <- 4
n_experiments_per_repeat <- 60
sample_n_participants_per_experiment <- T # if T, then draw a sample size from a poisson distribution, otherwise use the value of n_participants_per_experiment
n_participants_per_experiment <- 40 # only used if sample_n_participants_per_experiment is F
n_trials_per_participant <- 25
n_people <- 100000

total_simulations <- length(b_bases) * length(b_sexs) * length(b_conds) * length(b_sex_conds) * n_repeats
current_simulation <- 1

### Analysis parameters
# These allow you to choose which analyses do you want
do_pp_linear <- TRUE
do_pp_citation <- TRUE

### Publication bias
# This allows you to choose whether or not the analysis
# should be run with publication bias or not. If you choose
# to run with publication bias, you can set the values for
# the probability of being published depending on direction
# of effect and whether or not CIs include 0. This also allows
# you do decide whether you want the publication bias to be
# symmetric or asymmetric.
do_publication_bias <- TRUE

pb_prob_pos <- 0.9 # prob if b above zero and b lower above zero
pb_prob_null <- 0.2 # prob if b above zero and b lower below zero OR b below zero and b upper above zero
pb_prob_neg <- 0.6 # prob if b below zero and b upper below zero
# for symmetrical publication bias pb_prob_pos is also prob if b below zero and b upper below zero

### Posterior-passing parameters
# These give you various options wrt posterior passing
# log only the final experiment from each chain:
pp_final_expt_only <- TRUE

###                        ###
### SIMULATIONS START HERE ###
###                        ###

### For loops to iterate through parameter values
for (i in 1:length(b_bases)) {
  for (j in 1:length(b_sexs)) {
    for (k in 1:length(b_conds)) {
      for (l in 1:length(b_sex_conds)) {
        ### Set up values for the simulation
        # this function is in util.R
        # also save the values to the meta vectors
        prepare_for_simulation()

        ### Vectors to store data
        # this function is in util.R
        # These store the results of simulations within each set
        # of parameter values
        prepare_data_vectors()

        ###
        ### Each repeat starts here
        ###

        for (rep in 1:n_repeats) {
          print(paste(">>>> Repeat", rep, "of", n_repeats, sep = " "))
          print(paste(">>>> Simulation", current_simulation, "of", total_simulations, sep = " "))
          current_simulation <- current_simulation + 1

          ###
          ### Create the population ###
          ###
          # This function is in simulation.R
          # The population is simply a data table with 3 columns:
          # id, sex, d_base
          # It is created with the "doppelganger quadrangle" method
          # so there are equal number of men and women and the true
          # mean and true variance of each sub population is the same
          population <- create_population()

          ###
          ### Create citation chain
          ###
          citation_chain <- create_citation_chain(citation_list)


          ###
          ### Draw sample size ###
          ###
          # If sample_n_participants_per_experiment is T, the
          # sample size will be drawn from a poisson distribution.
          # Otherwise, the value of n_participants_per_experiment
          # is used.
          if (sample_n_participants_per_experiment == T) {
            n_participants_per_experiment <- rpois(1, lambda = 10) * 4 # multiply by 4 to make sure it's divisible by 4
          }


          ###
          ### Create the datasets for each experiment ###
          ###
          # This function is in simulation.R
          # The datasets are stored in a single table with 5 columns:
          # data_set - the id of the experiment
          # participant_id - the id of the participant within that experiment
          # sex - the sex of the participant
          # condition - the condition the participant was in
          # response - the mean response of that participant, ranging from 0 to 1
          data_sets <- create_datasets()

          ###
          ### run analyses ###
          ###
          # this function is in analyses.R
          # Now that we have all the data_sets we perform the desired analyses over them.
          # It is in these methods that we start to create the results table. Each entry
          # in this table corresponds to a single analysis on a single data_set.
          tmp_df <- do_analyses(data_sets, do_pp_linear, do_pp_citation, do_publication_bias) %>%
            mutate(
              repeat_id = rep,
              true_base = b_bases[i],
              true_sex = b_sexs[j],
              true_cond = b_conds[k],
              true_sex_cond = b_sex_conds[l],
              var_pop = unique(population$var_base)
            )

          if (exists("saved_results")) {
            saved_results <- rbind(saved_results, tmp_df)
          } else {
            saved_results <- tmp_df
          }

          ###
          ### Remove DLLs #
          ###
          # Code from https://github.com/stan-dev/rstan/issues/448
          loaded_dlls <- getLoadedDLLs()
          loaded_dlls <- loaded_dlls[str_detect(names(loaded_dlls), "^file")]
          if (length(loaded_dlls) > 10) {
            for (dll in head(loaded_dlls, -10)) {
              message("Unloading DLL ", dll[["name"]], ": ", dll[["path"]])
              dyn.unload(dll[["path"]])
            }
          }
          message("DLL Count = ", length(getLoadedDLLs()), ": [", str_c(names(loaded_dlls), collapse = ","), "]")
        } # end of for each repeat loop

        ###
        ### Create the Meta-results table
        ###
        # this function is in util.R
        # each entry in this table corresponds to a single repeat with unique
        # beta values for base, sex, cond, and sex_cond
        results_df <- saved_results %>%
          filter(
            true_base == b_bases[i] &
              true_sex == b_sexs[j] &
              true_cond == c_conds[k] &
              true_sex_conds == b_sex_conds[l]
          )
        tmp_meta_results <- save_results_meta(results_df, pp_final_expt_only)

        if (exists("saved_meta_results")) {
          saved_meta_results <- rbind(saved_meta_results, tmp_meta_results)
        } else {
          saved_meta_results <- tmp_meta_results
        }
      }
    }
  }
} # end of parameter value for loops

# save results
meta_results <- compile_meta_results()

write.csv(saved_results, "results/saved_results.csv")
write.csv(saved_meta_results, "results/meta_results.csv")
