prepare_meta_vectors <- function() {
  total_meta_conditions <<- length(b_bases) * length(b_sexs) * length(b_conds) * length(b_sex_conds) * n_repeats
  meta_n_repeats <<- rep(n_repeats, total_meta_conditions)
  meta_n_experiments_per_repeat <<- rep(n_experiments_per_repeat, total_meta_conditions)
  meta_n_participants_per_experiment <<- rep(n_participants_per_experiment, total_meta_conditions)
  meta_n_trials_per_participant <<- rep(n_trials_per_participant, total_meta_conditions)
  meta_n_people <<- rep(n_people, total_meta_conditions)
  meta_true_base <<- vector()
  meta_true_sex <<- vector()
  meta_true_cond <<- vector()
  meta_true_sex_cond <<- vector()
  pub_pp_l <<- vector()
  pub_pp_l_pb_sym <<- vector()
  pub_pp_l_pb_asym <<- vector()
  pub_pp_c <<- vector()
  pub_pp_c_pb_sym <<- vector()
  pub_pp_c_pb_asym <<- vector()
  meta_var_pop <<- vector()

  meta_base_estimate_pp_l <<- vector()
  meta_sex_estimate_pp_l <<- vector()
  meta_cond_estimate_pp_l <<- vector()
  meta_sex_cond_estimate_pp_l <<- vector()

  meta_base_estimate_pp_l_pb_sym <<- vector()
  meta_sex_estimate_pp_l_pb_sym <<- vector()
  meta_cond_estimate_pp_l_pb_sym <<- vector()
  meta_sex_cond_estimate_pp_l_pb_sym <<- vector()

  meta_base_estimate_pp_l_pb_asym <<- vector()
  meta_sex_estimate_pp_l_pb_asym <<- vector()
  meta_cond_estimate_pp_l_pb_asym <<- vector()
  meta_sex_cond_estimate_pp_l_pb_asym <<- vector()

  meta_base_estimate_pp_c <<- vector()
  meta_sex_estimate_pp_c <<- vector()
  meta_cond_estimate_pp_c <<- vector()
  meta_sex_cond_estimate_pp_c <<- vector()

  meta_base_estimate_pp_c_pb_sym <<- vector()
  meta_sex_estimate_pp_c_pb_sym <<- vector()
  meta_cond_estimate_pp_c_pb_sym <<- vector()
  meta_sex_cond_estimate_pp_c_pb_sym <<- vector()

  meta_base_estimate_pp_c_pb_asym <<- vector()
  meta_sex_estimate_pp_c_pb_asym <<- vector()
  meta_cond_estimate_pp_c_pb_asym <<- vector()
  meta_sex_cond_estimate_pp_c_pb_asym <<- vector()

  meta_base_estimate_upper_pp_l <<- vector()
  meta_sex_estimate_upper_pp_l <<- vector()
  meta_cond_estimate_upper_pp_l <<- vector()
  meta_sex_cond_estimate_upper_pp_l <<- vector()

  meta_base_estimate_upper_pp_l_pb_sym <<- vector()
  meta_sex_estimate_upper_pp_l_pb_sym <<- vector()
  meta_cond_estimate_upper_pp_l_pb_sym <<- vector()
  meta_sex_cond_estimate_upper_pp_l_pb_sym <<- vector()

  meta_base_estimate_upper_pp_l_pb_asym <<- vector()
  meta_sex_estimate_upper_pp_l_pb_asym <<- vector()
  meta_cond_estimate_upper_pp_l_pb_asym <<- vector()
  meta_sex_cond_estimate_upper_pp_l_pb_asym <<- vector()

  meta_base_estimate_upper_pp_c <<- vector()
  meta_sex_estimate_upper_pp_c <<- vector()
  meta_cond_estimate_upper_pp_c <<- vector()
  meta_sex_cond_estimate_upper_pp_c <<- vector()

  meta_base_estimate_upper_pp_c_pb_sym <<- vector()
  meta_sex_estimate_upper_pp_c_pb_sym <<- vector()
  meta_cond_estimate_upper_pp_c_pb_sym <<- vector()
  meta_sex_cond_estimate_upper_pp_c_pb_sym <<- vector()

  meta_base_estimate_upper_pp_c_pb_asym <<- vector()
  meta_sex_estimate_upper_pp_c_pb_asym <<- vector()
  meta_cond_estimate_upper_pp_c_pb_asym <<- vector()
  meta_sex_cond_estimate_upper_pp_c_pb_asym <<- vector()

  meta_base_estimate_lower_pp_l <<- vector()
  meta_sex_estimate_lower_pp_l <<- vector()
  meta_cond_estimate_lower_pp_l <<- vector()
  meta_sex_cond_estimate_lower_pp_l <<- vector()

  meta_base_estimate_lower_pp_l_pb_sym <<- vector()
  meta_sex_estimate_lower_pp_l_pb_sym <<- vector()
  meta_cond_estimate_lower_pp_l_pb_sym <<- vector()
  meta_sex_cond_estimate_lower_pp_l_pb_sym <<- vector()

  meta_base_estimate_lower_pp_l_pb_asym <<- vector()
  meta_sex_estimate_lower_pp_l_pb_asym <<- vector()
  meta_cond_estimate_lower_pp_l_pb_asym <<- vector()
  meta_sex_cond_estimate_lower_pp_l_pb_asym <<- vector()

  meta_base_estimate_lower_pp_c <<- vector()
  meta_sex_estimate_lower_pp_c <<- vector()
  meta_cond_estimate_lower_pp_c <<- vector()
  meta_sex_cond_estimate_lower_pp_c <<- vector()

  meta_base_estimate_lower_pp_c_pb_sym <<- vector()
  meta_sex_estimate_lower_pp_c_pb_sym <<- vector()
  meta_cond_estimate_lower_pp_c_pb_sym <<- vector()
  meta_sex_cond_estimate_lower_pp_c_pb_sym <<- vector()

  meta_base_estimate_lower_pp_c_pb_asym <<- vector()
  meta_sex_estimate_lower_pp_c_pb_asym <<- vector()
  meta_cond_estimate_lower_pp_c_pb_asym <<- vector()
  meta_sex_cond_estimate_lower_pp_c_pb_asym <<- vector()

  meta_base_uncertainty_pp_l <<- vector()
  meta_sex_uncertainty_pp_l <<- vector()
  meta_cond_uncertainty_pp_l <<- vector()
  meta_sex_cond_uncertainty_pp_l <<- vector()

  meta_base_uncertainty_pp_l_pb_sym <<- vector()
  meta_sex_uncertainty_pp_l_pb_sym <<- vector()
  meta_cond_uncertainty_pp_l_pb_sym <<- vector()
  meta_sex_cond_uncertainty_pp_l_pb_sym <<- vector()

  meta_base_uncertainty_pp_l_pb_asym <<- vector()
  meta_sex_uncertainty_pp_l_pb_asym <<- vector()
  meta_cond_uncertainty_pp_l_pb_asym <<- vector()
  meta_sex_cond_uncertainty_pp_l_pb_asym <<- vector()

  meta_base_uncertainty_pp_c <<- vector()
  meta_sex_uncertainty_pp_c <<- vector()
  meta_cond_uncertainty_pp_c <<- vector()
  meta_sex_cond_uncertainty_pp_c <<- vector()

  meta_base_uncertainty_pp_c_pb_sym <<- vector()
  meta_sex_uncertainty_pp_c_pb_sym <<- vector()
  meta_cond_uncertainty_pp_c_pb_sym <<- vector()
  meta_sex_cond_uncertainty_pp_c_pb_sym <<- vector()

  meta_base_uncertainty_pp_c_pb_asym <<- vector()
  meta_sex_uncertainty_pp_c_pb_asym <<- vector()
  meta_cond_uncertainty_pp_c_pb_asym <<- vector()
  meta_sex_cond_uncertainty_pp_c_pb_asym <<- vector()
}

save_results_meta <- function(results_df, pp_final_expt_only) {
  print(">>>>>>>> Saving meta results")

  meta_results <- results_df %>%
    group_by(
      repeat_id,
      analysis_type,
      pub_method,
      true_base,
      true_sex,
      true_cond,
      true_sex_cond
    ) %>%
    summarize(
      n_expt = max(expt),
      n_published = sum(pub_true),
      var_pop = unique(var_pop)
    )

  if (pp_final_expt_only == FALSE) {
    meta_estimates <- results_df %>%
      group_by(
        repeat_id,
        analysis_type,
        pub_method,
        true_base,
        true_sex,
        true_cond,
        true_sex_cond
      ) %>%
      summarize(
        # median
        mean_base_estimate = mean(b_base_med),
        mean_sex_estimate = mean(b_sex_med),
        mean_cond_estimate = mean(b_cond_med),
        mean_sex_cond_estimated = mean(b_sex_cond_med),
        # lower
        mean_base_lower = mean(b_base_lower),
        mean_sex_lower = mean(b_sex_lower),
        mean_cond_lower = mean(b_cond_lower),
        mean_sex_cond_lower = mean(b_sex_cond_lower),
        # upper
        mean_base_upper = mean(b_base_upper),
        mean_sex_upper = mean(b_sex_upper),
        mean_cond_upper = mean(b_cond_upper),
        mean_sex_cond_upper = mean(b_sex_cond_upper),
        # uncertainty
        uncertainty = mean(b_base_upper - b_base_lower)
      )
  } else {
    # mean or unique ???
    meta_estimates <- results_df %>%
      group_by(
        repeat_id,
        analysis_type,
        pub_method,
        true_base,
        true_sex,
        true_cond,
        true_sex_cond
      ) %>%
      filter(expt == max(expt)) %>%
      summarize(
        # median
        mean_base_estimate = mean(b_base_med),
        mean_sex_estimate = mean(b_sex_med),
        mean_cond_estimate = mean(b_cond_med),
        mean_sex_cond_estimated = mean(b_sex_cond_med),
        # lower
        mean_base_lower = mean(b_base_lower),
        mean_sex_lower = mean(b_sex_lower),
        mean_cond_lower = mean(b_cond_lower),
        mean_sex_cond_lower = mean(b_sex_cond_lower),
        # upper
        mean_base_upper = mean(b_base_upper),
        mean_sex_upper = mean(b_sex_upper),
        mean_cond_upper = mean(b_cond_upper),
        mean_sex_cond_upper = mean(b_sex_cond_upper),
        # uncertainty
        uncertainty = mean(b_base_upper - b_base_lower)
      )
  }

  meta_results <- merge(meta_results, meta_estimates)

  return(meta_results)
}

compile_meta_results <- function() {
  meta_base_estimate_pp_l_p <- exp(meta_base_estimate_pp_l) / (1 + exp(meta_base_estimate_pp_l))
  meta_sex_estimate_pp_l_p <- exp(meta_sex_estimate_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_estimate_pp_l + meta_base_estimate_pp_l)) - meta_base_estimate_pp_l_p
  meta_cond_estimate_pp_l_p <- exp(meta_cond_estimate_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_cond_estimate_pp_l + meta_base_estimate_pp_l)) - meta_base_estimate_pp_l_p
  meta_sex_cond_estimate_pp_l_p <- exp(meta_sex_cond_estimate_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_cond_estimate_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l)) - (meta_base_estimate_pp_l_p + meta_sex_estimate_pp_l_p + meta_cond_estimate_pp_l_p)

  meta_base_estimate_pp_l_pb_sym_p <- exp(meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_base_estimate_pp_l_pb_sym))
  meta_sex_estimate_pp_l_pb_sym_p <- exp(meta_sex_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - meta_base_estimate_pp_l_pb_sym_p
  meta_cond_estimate_pp_l_pb_sym_p <- exp(meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - meta_base_estimate_pp_l_pb_sym_p
  meta_sex_cond_estimate_pp_l_pb_sym_p <- exp(meta_sex_cond_estimate_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_cond_estimate_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - (meta_base_estimate_pp_l_pb_sym_p + meta_sex_estimate_pp_l_pb_sym_p + meta_cond_estimate_pp_l_pb_sym_p)

  meta_base_estimate_pp_l_pb_asym_p <- exp(meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_base_estimate_pp_l_pb_asym))
  meta_sex_estimate_pp_l_pb_asym_p <- exp(meta_sex_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - meta_base_estimate_pp_l_pb_asym_p
  meta_cond_estimate_pp_l_pb_asym_p <- exp(meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - meta_base_estimate_pp_l_pb_asym_p
  meta_sex_cond_estimate_pp_l_pb_asym_p <- exp(meta_sex_cond_estimate_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_cond_estimate_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - (meta_base_estimate_pp_l_pb_asym_p + meta_sex_estimate_pp_l_pb_asym_p + meta_cond_estimate_pp_l_pb_asym_p)

  meta_base_estimate_pp_c_p <- exp(meta_base_estimate_pp_c) / (1 + exp(meta_base_estimate_pp_c))
  meta_sex_estimate_pp_c_p <- exp(meta_sex_estimate_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_estimate_pp_c + meta_base_estimate_pp_c)) - meta_base_estimate_pp_c_p
  meta_cond_estimate_pp_c_p <- exp(meta_cond_estimate_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_cond_estimate_pp_c + meta_base_estimate_pp_c)) - meta_base_estimate_pp_c_p
  meta_sex_cond_estimate_pp_c_p <- exp(meta_sex_cond_estimate_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_cond_estimate_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c)) - (meta_base_estimate_pp_c_p + meta_sex_estimate_pp_c_p + meta_cond_estimate_pp_c_p)

  meta_base_estimate_pp_c_pb_sym_p <- exp(meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_base_estimate_pp_c_pb_sym))
  meta_sex_estimate_pp_c_pb_sym_p <- exp(meta_sex_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - meta_base_estimate_pp_c_pb_sym_p
  meta_cond_estimate_pp_c_pb_sym_p <- exp(meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - meta_base_estimate_pp_c_pb_sym_p
  meta_sex_cond_estimate_pp_c_pb_sym_p <- exp(meta_sex_cond_estimate_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_cond_estimate_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - (meta_base_estimate_pp_c_pb_sym_p + meta_sex_estimate_pp_c_pb_sym_p + meta_cond_estimate_pp_c_pb_sym_p)

  meta_base_estimate_pp_c_pb_asym_p <- exp(meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_base_estimate_pp_c_pb_asym))
  meta_sex_estimate_pp_c_pb_asym_p <- exp(meta_sex_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - meta_base_estimate_pp_c_pb_asym_p
  meta_cond_estimate_pp_c_pb_asym_p <- exp(meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - meta_base_estimate_pp_c_pb_asym_p
  meta_sex_cond_estimate_pp_c_pb_asym_p <- exp(meta_sex_cond_estimate_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_cond_estimate_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - (meta_base_estimate_pp_c_pb_asym_p + meta_sex_estimate_pp_c_pb_asym_p + meta_cond_estimate_pp_c_pb_asym_p)

  meta_base_uncertainty_pp_l_p <- exp(meta_base_estimate_upper_pp_l) / (1 + exp(meta_base_estimate_upper_pp_l)) - exp(meta_base_estimate_lower_pp_l) / (1 + exp(meta_base_estimate_lower_pp_l))
  meta_sex_uncertainty_pp_l_p <- exp(meta_sex_estimate_upper_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_estimate_upper_pp_l + meta_base_estimate_pp_l)) - exp(meta_sex_estimate_lower_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_estimate_lower_pp_l + meta_base_estimate_pp_l))
  meta_cond_uncertainty_pp_l_p <- exp(meta_cond_estimate_upper_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_cond_estimate_upper_pp_l + meta_base_estimate_pp_l)) - exp(meta_cond_estimate_lower_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_cond_estimate_lower_pp_l + meta_base_estimate_pp_l))
  meta_sex_cond_uncertainty_pp_l_p <- exp(meta_sex_cond_estimate_upper_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_cond_estimate_upper_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l)) - exp(meta_sex_cond_estimate_lower_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l) / (1 + exp(meta_sex_cond_estimate_lower_pp_l + meta_sex_estimate_pp_l + meta_cond_estimate_pp_l + meta_base_estimate_pp_l))

  meta_base_uncertainty_pp_l_pb_sym_p <- exp(meta_base_estimate_upper_pp_l_pb_sym) / (1 + exp(meta_base_estimate_upper_pp_l_pb_sym)) - exp(meta_base_estimate_lower_pp_l_pb_sym) / (1 + exp(meta_base_estimate_lower_pp_l_pb_sym))
  meta_sex_uncertainty_pp_l_pb_sym_p <- exp(meta_sex_estimate_upper_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_estimate_upper_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - exp(meta_sex_estimate_lower_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_estimate_lower_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym))
  meta_cond_uncertainty_pp_l_pb_sym_p <- exp(meta_cond_estimate_upper_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_cond_estimate_upper_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - exp(meta_cond_estimate_lower_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_cond_estimate_lower_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym))
  meta_sex_cond_uncertainty_pp_l_pb_sym_p <- exp(meta_sex_cond_estimate_upper_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_cond_estimate_upper_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym)) - exp(meta_sex_cond_estimate_lower_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym) / (1 + exp(meta_sex_cond_estimate_lower_pp_l_pb_sym + meta_sex_estimate_pp_l_pb_sym + meta_cond_estimate_pp_l_pb_sym + meta_base_estimate_pp_l_pb_sym))

  meta_base_uncertainty_pp_l_pb_asym_p <- exp(meta_base_estimate_upper_pp_l_pb_asym) / (1 + exp(meta_base_estimate_upper_pp_l_pb_asym)) - exp(meta_base_estimate_lower_pp_l_pb_asym) / (1 + exp(meta_base_estimate_lower_pp_l_pb_asym))
  meta_sex_uncertainty_pp_l_pb_asym_p <- exp(meta_sex_estimate_upper_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_estimate_upper_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - exp(meta_sex_estimate_lower_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_estimate_lower_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym))
  meta_cond_uncertainty_pp_l_pb_asym_p <- exp(meta_cond_estimate_upper_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_cond_estimate_upper_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - exp(meta_cond_estimate_lower_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_cond_estimate_lower_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym))
  meta_sex_cond_uncertainty_pp_l_pb_asym_p <- exp(meta_sex_cond_estimate_upper_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_cond_estimate_upper_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym)) - exp(meta_sex_cond_estimate_lower_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym) / (1 + exp(meta_sex_cond_estimate_lower_pp_l_pb_asym + meta_sex_estimate_pp_l_pb_asym + meta_cond_estimate_pp_l_pb_asym + meta_base_estimate_pp_l_pb_asym))

  meta_base_uncertainty_pp_c_p <- exp(meta_base_estimate_upper_pp_c) / (1 + exp(meta_base_estimate_upper_pp_c)) - exp(meta_base_estimate_lower_pp_c) / (1 + exp(meta_base_estimate_lower_pp_c))
  meta_sex_uncertainty_pp_c_p <- exp(meta_sex_estimate_upper_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_estimate_upper_pp_c + meta_base_estimate_pp_c)) - exp(meta_sex_estimate_lower_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_estimate_lower_pp_c + meta_base_estimate_pp_c))
  meta_cond_uncertainty_pp_c_p <- exp(meta_cond_estimate_upper_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_cond_estimate_upper_pp_c + meta_base_estimate_pp_c)) - exp(meta_cond_estimate_lower_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_cond_estimate_lower_pp_c + meta_base_estimate_pp_c))
  meta_sex_cond_uncertainty_pp_c_p <- exp(meta_sex_cond_estimate_upper_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_cond_estimate_upper_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c)) - exp(meta_sex_cond_estimate_lower_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c) / (1 + exp(meta_sex_cond_estimate_lower_pp_c + meta_sex_estimate_pp_c + meta_cond_estimate_pp_c + meta_base_estimate_pp_c))

  meta_base_uncertainty_pp_c_pb_sym_p <- exp(meta_base_estimate_upper_pp_c_pb_sym) / (1 + exp(meta_base_estimate_upper_pp_c_pb_sym)) - exp(meta_base_estimate_lower_pp_c_pb_sym) / (1 + exp(meta_base_estimate_lower_pp_c_pb_sym))
  meta_sex_uncertainty_pp_c_pb_sym_p <- exp(meta_sex_estimate_upper_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_estimate_upper_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - exp(meta_sex_estimate_lower_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_estimate_lower_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym))
  meta_cond_uncertainty_pp_c_pb_sym_p <- exp(meta_cond_estimate_upper_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_cond_estimate_upper_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - exp(meta_cond_estimate_lower_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_cond_estimate_lower_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym))
  meta_sex_cond_uncertainty_pp_c_pb_sym_p <- exp(meta_sex_cond_estimate_upper_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_cond_estimate_upper_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym)) - exp(meta_sex_cond_estimate_lower_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym) / (1 + exp(meta_sex_cond_estimate_lower_pp_c_pb_sym + meta_sex_estimate_pp_c_pb_sym + meta_cond_estimate_pp_c_pb_sym + meta_base_estimate_pp_c_pb_sym))

  meta_base_uncertainty_pp_c_pb_asym_p <- exp(meta_base_estimate_upper_pp_c_pb_asym) / (1 + exp(meta_base_estimate_upper_pp_c_pb_asym)) - exp(meta_base_estimate_lower_pp_c_pb_asym) / (1 + exp(meta_base_estimate_lower_pp_c_pb_asym))
  meta_sex_uncertainty_pp_c_pb_asym_p <- exp(meta_sex_estimate_upper_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_estimate_upper_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - exp(meta_sex_estimate_lower_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_estimate_lower_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym))
  meta_cond_uncertainty_pp_c_pb_asym_p <- exp(meta_cond_estimate_upper_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_cond_estimate_upper_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - exp(meta_cond_estimate_lower_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_cond_estimate_lower_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym))
  meta_sex_cond_uncertainty_pp_c_pb_asym_p <- exp(meta_sex_cond_estimate_upper_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_cond_estimate_upper_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym)) - exp(meta_sex_cond_estimate_lower_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym) / (1 + exp(meta_sex_cond_estimate_lower_pp_c_pb_asym + meta_sex_estimate_pp_c_pb_asym + meta_cond_estimate_pp_c_pb_asym + meta_base_estimate_pp_c_pb_asym))

  return(data.frame(
    meta_n_repeats, meta_n_experiments_per_repeat, meta_n_participants_per_experiment,
    meta_n_trials_per_participant, meta_n_people,
    meta_true_base, meta_true_sex, meta_true_cond, meta_true_sex_cond,
    pub_pp_l, pub_pp_l_pb_sym, pub_pp_l_pb_asym, pub_pp_c, pub_pp_c_pb_sym, pub_pp_c_pb_asym, meta_var_pop,
    meta_base_estimate_pp_l, meta_sex_estimate_pp_l, meta_cond_estimate_pp_l, meta_sex_cond_estimate_pp_l,
    meta_base_estimate_pp_l_pb_sym, meta_sex_estimate_pp_l_pb_sym, meta_cond_estimate_pp_l_pb_sym, meta_sex_cond_estimate_pp_l_pb_sym,
    meta_base_estimate_pp_l_pb_asym, meta_sex_estimate_pp_l_pb_asym, meta_cond_estimate_pp_l_pb_asym, meta_sex_cond_estimate_pp_l_pb_asym,
    meta_base_estimate_pp_c, meta_sex_estimate_pp_c, meta_cond_estimate_pp_c, meta_sex_cond_estimate_pp_c,
    meta_base_estimate_pp_c_pb_sym, meta_sex_estimate_pp_c_pb_sym, meta_cond_estimate_pp_c_pb_sym, meta_sex_cond_estimate_pp_c_pb_sym,
    meta_base_estimate_pp_c_pb_asym, meta_sex_estimate_pp_c_pb_asym, meta_cond_estimate_pp_c_pb_asym, meta_sex_cond_estimate_pp_c_pb_asym,
    meta_base_estimate_pp_l_p, meta_sex_estimate_pp_l_p, meta_cond_estimate_pp_l_p, meta_sex_cond_estimate_pp_l_p,
    meta_base_estimate_pp_l_pb_sym_p, meta_sex_estimate_pp_l_pb_sym_p, meta_cond_estimate_pp_l_pb_sym_p, meta_sex_cond_estimate_pp_l_pb_sym_p,
    meta_base_estimate_pp_l_pb_asym_p, meta_sex_estimate_pp_l_pb_asym_p, meta_cond_estimate_pp_l_pb_asym_p, meta_sex_cond_estimate_pp_l_pb_asym_p,
    meta_base_estimate_pp_c_p, meta_sex_estimate_pp_c_p, meta_cond_estimate_pp_c_p, meta_sex_cond_estimate_pp_c_p,
    meta_base_estimate_pp_c_pb_sym_p, meta_sex_estimate_pp_c_pb_sym_p, meta_cond_estimate_pp_c_pb_sym_p, meta_sex_cond_estimate_pp_c_pb_sym_p,
    meta_base_estimate_pp_c_pb_asym_p, meta_sex_estimate_pp_c_pb_asym_p, meta_cond_estimate_pp_c_pb_asym_p, meta_sex_cond_estimate_pp_c_pb_asym_p,
    meta_base_estimate_lower_pp_l, meta_sex_estimate_lower_pp_l, meta_cond_estimate_lower_pp_l, meta_sex_cond_estimate_lower_pp_l,
    meta_base_estimate_lower_pp_l_pb_sym, meta_sex_estimate_lower_pp_l_pb_sym, meta_cond_estimate_lower_pp_l_pb_sym, meta_sex_cond_estimate_lower_pp_l_pb_sym,
    meta_base_estimate_lower_pp_l_pb_asym, meta_sex_estimate_lower_pp_l_pb_asym, meta_cond_estimate_lower_pp_l_pb_asym, meta_sex_cond_estimate_lower_pp_l_pb_asym,
    meta_base_estimate_lower_pp_c, meta_sex_estimate_lower_pp_c, meta_cond_estimate_lower_pp_c, meta_sex_cond_estimate_lower_pp_c,
    meta_base_estimate_lower_pp_c_pb_sym, meta_sex_estimate_lower_pp_c_pb_sym, meta_cond_estimate_lower_pp_c_pb_sym, meta_sex_cond_estimate_lower_pp_c_pb_sym,
    meta_base_estimate_lower_pp_c_pb_asym, meta_sex_estimate_lower_pp_c_pb_asym, meta_cond_estimate_lower_pp_c_pb_asym, meta_sex_cond_estimate_lower_pp_c_pb_asym,
    meta_base_estimate_upper_pp_l, meta_sex_estimate_upper_pp_l, meta_cond_estimate_upper_pp_l, meta_sex_cond_estimate_upper_pp_l,
    meta_base_estimate_upper_pp_l_pb_sym, meta_sex_estimate_upper_pp_l_pb_sym, meta_cond_estimate_upper_pp_l_pb_sym, meta_sex_cond_estimate_upper_pp_l_pb_sym,
    meta_base_estimate_upper_pp_l_pb_asym, meta_sex_estimate_upper_pp_l_pb_asym, meta_cond_estimate_upper_pp_l_pb_asym, meta_sex_cond_estimate_upper_pp_l_pb_asym,
    meta_base_estimate_upper_pp_c, meta_sex_estimate_upper_pp_c, meta_cond_estimate_upper_pp_c, meta_sex_cond_estimate_upper_pp_c,
    meta_base_estimate_upper_pp_c_pb_sym, meta_sex_estimate_upper_pp_c_pb_sym, meta_cond_estimate_upper_pp_c_pb_sym, meta_sex_cond_estimate_upper_pp_c_pb_sym,
    meta_base_estimate_upper_pp_c_pb_asym, meta_sex_estimate_upper_pp_c_pb_asym, meta_cond_estimate_upper_pp_c_pb_asym, meta_sex_cond_estimate_upper_pp_c_pb_asym,
    meta_base_uncertainty_pp_l, meta_sex_uncertainty_pp_l, meta_cond_uncertainty_pp_l, meta_sex_cond_uncertainty_pp_l,
    meta_base_uncertainty_pp_l_pb_sym, meta_sex_uncertainty_pp_l_pb_sym, meta_cond_uncertainty_pp_l_pb_sym, meta_sex_cond_uncertainty_pp_l_pb_sym,
    meta_base_uncertainty_pp_l_pb_asym, meta_sex_uncertainty_pp_l_pb_asym, meta_cond_uncertainty_pp_l_pb_asym, meta_sex_cond_uncertainty_pp_l_pb_asym,
    meta_base_uncertainty_pp_c, meta_sex_uncertainty_pp_c, meta_cond_uncertainty_pp_c, meta_sex_cond_uncertainty_pp_c,
    meta_base_uncertainty_pp_c_pb_sym, meta_sex_uncertainty_pp_c_pb_sym, meta_cond_uncertainty_pp_c_pb_sym, meta_sex_cond_uncertainty_pp_c_pb_sym,
    meta_base_uncertainty_pp_c_pb_asym, meta_sex_uncertainty_pp_c_pb_asym, meta_cond_uncertainty_pp_c_pb_asym, meta_sex_cond_uncertainty_pp_c_pb_asym,
    meta_base_uncertainty_pp_l_p, meta_sex_uncertainty_pp_l_p, meta_cond_uncertainty_pp_l_p, meta_sex_cond_uncertainty_pp_l_p,
    meta_base_uncertainty_pp_l_pb_sym_p, meta_sex_uncertainty_pp_l_pb_sym_p, meta_cond_uncertainty_pp_l_pb_sym_p, meta_sex_cond_uncertainty_pp_l_pb_sym_p,
    meta_base_uncertainty_pp_l_pb_asym_p, meta_sex_uncertainty_pp_l_pb_asym_p, meta_cond_uncertainty_pp_l_pb_asym_p, meta_sex_cond_uncertainty_pp_l_pb_asym_p,
    meta_base_uncertainty_pp_c_p, meta_sex_uncertainty_pp_c_p, meta_cond_uncertainty_pp_c_p, meta_sex_cond_uncertainty_pp_c_p,
    meta_base_uncertainty_pp_c_pb_sym_p, meta_sex_uncertainty_pp_c_pb_sym_p, meta_cond_uncertainty_pp_c_pb_sym_p, meta_sex_cond_uncertainty_pp_c_pb_sym_p,
    meta_base_uncertainty_pp_c_pb_asym_p, meta_sex_uncertainty_pp_c_pb_asym_p, meta_cond_uncertainty_pp_c_pb_asym_p, meta_sex_cond_uncertainty_pp_c_pb_asym_p
  ))
}

prepare_for_simulation <- function() {
  b_base <<- b_bases[i]
  b_sex <<- b_sexs[j]
  b_cond <<- b_conds[k]
  b_sex_cond <<- b_sex_conds[l]
  meta_true_base <<- c(meta_true_base, rep(b_base, n_repeats))
  meta_true_sex <<- c(meta_true_sex, rep(b_sex, n_repeats))
  meta_true_cond <<- c(meta_true_cond, rep(b_cond, n_repeats))
  meta_true_sex_cond <<- c(meta_true_sex_cond, rep(b_sex_cond, n_repeats))
  print(paste("running simulation with parameters: b_base: ", b_base,
    ", b_sex: ", b_sex,
    ", b_cond: ", b_cond,
    ", b_sex_cond: ", b_sex_cond,
    sep = " "
  ))
}


tidy_chain <- function(edges, edge) {
  new_edge <- edge
  for (i in 1:nrow(edge)) {
    cite <- edges %>% filter(to == edge$from[i]) # filter all the studies the node cites
    if (nrow(cite) > 0) { # if the cited study cites anyone
      for (c in 1:nrow(cite)) {
        if (cite$from[c] %in% edge$from) { # if a cited study cites the same as the node
          new_edge <- new_edge[!(new_edge$from == cite$from[c]), ] # remove the study it cites
        }
      }
    }
  }
  return(new_edge)
}
