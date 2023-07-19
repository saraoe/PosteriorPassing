do_analyses <- function(
    data_sets,
    do_pp_linear,
    do_pp_citation,
    do_publication_bias,
    pb_prob_pos,
    pb_prob_neg,
    pb_prob_null) {
  print(">>>>>>>> Doing analyses")

  n_experiments_per_repeat <- max(data_sets$data_set)

  # make empty dataframes
  results_df <- dplyr::tibble(
    expt = numeric(),
    analysis_type = character(),
    pub_method = character(),
    pub_true = numeric(),
    posteriors_included = numeric(),
    b_base_med = numeric(),
    b_sex_med = numeric(),
    b_cond_med = numeric(),
    b_sex_cond_med = numeric(),
    b_base_lower = numeric(),
    b_sex_lower = numeric(),
    b_cond_lower = numeric(),
    b_sex_cond_lower = numeric(),
    b_base_upper = numeric(),
    b_sex_upper = numeric(),
    b_cond_upper = numeric(),
    b_sex_cond_upper = numeric()
  )

  meta_df <- dplyr::tibble(
    b_sex_cond_meta = numeric(),
    b_sex_cond_error_meta = numeric(),
    b_sex_cond_lower_meta = numeric(),
    b_sex_cond_upper_meta = numeric(),
    meta_n_exp = numeric()
  )

  ## LINEAR CHAIN ##
  if (do_pp_linear == TRUE) {
    ## WITHOUT PUBLICATION BIAS ##

    print(">>>>>>>>>>>> Doing posterior passing on linear chain (no pb)")
    # name analysis
    current_analysis_type <- "pp_linear"
    current_pub_method <- NA

    # set initial prior for b_sex_cond
    pp_u <- 0
    pp_sig <- 0.1

    for (experiment in seq_len(n_experiments_per_repeat)) {
      print(paste(">>>> Model", experiment, "for linear_no_pb")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))

      tmp_results_df <- run_model(
        data_set = data_sets[data_sets$data_set == experiment, ],
        pp_u = pp_u,
        pp_sig = pp_sig
      )

      # save the results of the pp
      tmp_results_df <- tmp_results_df %>%
        mutate(
          expt = experiment,
          analysis_type = current_analysis_type,
          pub_method = current_pub_method,
          pub_true = 1,
          posteriors_included = ifelse(experiment == 1, 0, 1),
        )

      results_df <- rbind(results_df, tmp_results_df)

      # update the priors for the next run
      pp_u <- tmp_results_df$b_sex_cond_med
      pp_sig <- tmp_results_df$b_sex_cond_error

      # running meta analysis
      meta_data <- results_df %>%
        filter(analysis_type == current_analysis_type & pub_true == 1)
      tmp_meta_df <- running_meta_analysis(meta_data)

      meta_df <- rbind(meta_df, tmp_meta_df)
    } # end of for each experiment loop
    # end of pp_linear without pb

    ## WITH PUBLICATION BIAS ##

    if (do_publication_bias == TRUE) {
      print(">>>>>>>>>>>> Doing posterior passing on linear chain (sym pb)")
      # name analysis
      current_analysis_type <- "pp_linear_pb_sym"
      current_pub_method <- "sym"

      # set initial prior for b_sex_cond
      pp_u <- 0
      pp_sig <- 0.1

      for (experiment in seq_len(n_experiments_per_repeat)) {
        print(paste(">>>> Model", experiment, "for linear_sym")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))

        tmp_results_df <- run_model(
          data_set = data_sets[data_sets$data_set == experiment, ],
          pp_u = pp_u,
          pp_sig = pp_sig
        )

        # update the priors for the next run
        pb <- publication_true(
          med = tmp_results_df$b_sex_cond_med,
          lower = tmp_results_df$b_sex_cond_lower,
          upper = tmp_results_df$b_sex_cond_lower,
          pb_prob_pos = pb_prob_pos,
          pb_prob_pos = pb_prob_pos,
          pb_prob_null = pb_prob_null
        )

        pp_u <- ifelse(pb == 1,
          tmp_results_df$b_sex_cond_med,
          pp_u[1]
        )
        pp_sig <- ifelse(pb == 1,
          tmp_results_df$b_sex_cond_error,
          pp_sig[1]
        )

        # save the results of the pp
        tmp_results_df <- tmp_results_df %>%
          mutate(
            expt = experiment,
            analysis_type = current_analysis_type,
            pub_method = current_pub_method,
            pub_true = ifelse(pb == 0, 0, 1),
            posteriors_included = ifelse((pp_u == 0 && pp_sig == 0.1), 0, 1)
          )

        results_df <- rbind(results_df, tmp_results_df)

        # running meta analysis
        meta_data <- results_df %>%
          filter(analysis_type == current_analysis_type & pub_true == 1)
        tmp_meta_df <- running_meta_analysis(meta_data)

        meta_df <- rbind(meta_df, tmp_meta_df)
      } # end of p_linear_sym

      print(">>>>>>>>>>>> Doing posterior passing on linear chain (asym pb)")
      # name analysis
      current_analysis_type <- "pp_linear_pb_asym"
      current_pub_method <- "asym"

      # set initial prior for b_sex_cond
      pp_u <- 0
      pp_sig <- 0.1

      for (experiment in seq_len(n_experiments_per_repeat)) {
        print(paste(">>>> Model", experiment, "for linear_asym")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))

        tmp_results_df <- run_model(
          data_set = data_sets[data_sets$data_set == experiment, ],
          pp_u = pp_u,
          pp_sig = pp_sig
        )

        # update the priors for the next run
        pb <- publication_true(
          med = tmp_results_df$b_sex_cond_med,
          lower = tmp_results_df$b_sex_cond_lower,
          upper = tmp_results_df$b_sex_cond_lower,
          pb_prob_pos = pb_prob_pos,
          pb_prob_pos = pb_prob_neg,
          pb_prob_null = pb_prob_null
        )

        pp_u <- ifelse(pb == 1,
          tmp_results_df$b_sex_cond_med,
          pp_u[1]
        )
        pp_sig <- ifelse(pb == 1,
          tmp_results_df$b_sex_cond_error,
          pp_sig[1]
        )

        # save the results of the pp
        tmp_results_df <- tmp_results_df %>%
          mutate(
            expt = experiment,
            analysis_type = current_analysis_type,
            pub_method = current_pub_method,
            pub_true = ifelse(pb == 0, 0, 1),
            posteriors_included = ifelse((pp_u == 0 && pp_sig == 0.1), 0, 1)
          )

        results_df <- rbind(results_df, tmp_results_df)

        # running meta analysis
        meta_data <- results_df %>%
          filter(analysis_type == current_analysis_type & pub_true == 1)
        tmp_meta_df <- running_meta_analysis(meta_data)

        meta_df <- rbind(meta_df, tmp_meta_df)
      } # end of pp_linear_asym
    } # end of pp_linear with pb
  } # end of do_pp_linear

  ## CITATION CHAIN ##
  if (do_pp_citation == TRUE) {
    ## WITHOUT PUBLCIATION BIAS ##

    print(">>>>>>>>>>>> Doing posterior passing on citation chain (no pb)")
    # name analysis
    current_analysis_type <- "pp_citation"
    current_pub_method <- NA

    # empty df to save data in each iteration
    chain_df <- as.data.frame(matrix(0, nrow = 0, ncol = 3))
    colnames(chain_df) <- c("pp_u", "pp_sig", "studyID")

    # loop
    for (experiment in seq_len(n_experiments_per_repeat)) {
      print(paste(">>>> Model", experiment, "for citation_no_pb")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))
      # for every experiment get the relevant data set
      this_data_set <- data_sets[data_sets$data_set == experiment, ]
      this_data_set$studyID <- as.character(this_data_set$studyID)
      this_chain_df <- as.data.frame(matrix(0, nrow = 1, ncol = 3))
      colnames(this_chain_df) <- colnames(chain_df)

      # set initial prior for b_sex_cond and update afterwards (if not first round, update pp_u and pp_sig)
      if (nrow(chain_df) == 0) {
        pp_u <- 0
        pp_sig <- 0.1

        pp_n <- 0
      } else {
        this_citation_chain <- citation_chain %>%
          filter(to == this_data_set$studyID[1]) %>%
          filter(from %in% chain_df$studyID)

        if (!(nrow(this_citation_chain) == 0)) {
          this_citation_chain <- tidy_chain(citation_chain, this_citation_chain)
        }

        if (nrow(this_citation_chain) == 0) { # if no studies are cited
          pp_u <- 0
          pp_sig <- 0.1

          pp_n <- 0
        } else {
          this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)

          if (nrow(this_chain_df) == 1) { # if one study is cited
            pp_u <- this_chain_df$pp_u
            pp_sig <- this_chain_df$pp_sig

            pp_n <- nrow(this_chain_df)
          } else {
            pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)

            pp_u <- pp[, 1]
            pp_sig <- pp[, 2]

            pp_n <- nrow(this_chain_df)
          }
        }
      }

      tmp_results_df <- run_model(
        data_set = this_data_set,
        pp_u = pp_u,
        pp_sig = pp_sig
      )

      # new dataframe with saved PP values
      this_chain_df$pp_u <- tmp_results_df$b_sex_cond_med
      this_chain_df$pp_sig <- tmp_results_df$b_sex_cond_error
      this_chain_df$studyID <- this_data_set$studyID[1]

      chain_df <- rbind(chain_df, this_chain_df)

      # save the results of the pp
      tmp_results_df <- tmp_results_df %>%
        mutate(
          expt = experiment,
          analysis_type = current_analysis_type,
          pub_method = current_pub_method,
          pub_true = 1,
          posteriors_included = pp_n
        )

      results_df <- rbind(results_df, tmp_results_df)

      # running meta analysis
      meta_data <- results_df %>%
        filter(analysis_type == current_analysis_type & pub_true == 1)
      tmp_meta_df <- running_meta_analysis(meta_data)

      meta_df <- rbind(meta_df, tmp_meta_df)
    } # end of for each experiment loop
    # end of pp_citation without pb

    ## WITH PUBLICATION BIAS ##

    if (do_publication_bias == TRUE) {
      print(">>>>>>>>>>>> Doing posterior passing on citation chain (pb sym)")
      # name analysis
      current_analysis_type <- "pp_citation_pb_sym"
      current_pub_method <- "sym"

      # empty df to save data in each iteration
      chain_df <- as.data.frame(matrix(0, nrow = 0, ncol = 3))
      colnames(chain_df) <- c("pp_u", "pp_sig", "studyID")

      # loop
      for (experiment in seq_len(n_experiments_per_repeat)) {
        print(paste(">>>> Model", experiment, "for citation_sym")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment, ]
        this_data_set$studyID <- as.character(this_data_set$studyID)
        this_chain_df <- as.data.frame(matrix(0, nrow = 1, ncol = 3))
        colnames(this_chain_df) <- colnames(chain_df)

        # set initial prior for b_sex_cond and update afterwards (if not first round, update pp_u and pp_sig)
        if (nrow(chain_df) == 0) {
          pp_u <- 0
          pp_sig <- 0.1

          pp_n <- 0 # adding a meassure for number of posteriors included
        } else {
          this_citation_chain <- citation_chain %>%
            filter(to == this_data_set$studyID[1]) %>%
            filter(from %in% chain_df$studyID)

          if (!(nrow(this_citation_chain) == 0)) {
            this_citation_chain <- tidy_chain(citation_chain, this_citation_chain)
          }

          if (nrow(this_citation_chain) == 0) {
            pp_u <- 0
            pp_sig <- 0.1

            pp_n <- 0
          } else {
            this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)

            if (nrow(this_chain_df) == 1) { # if one study is cited
              pp_u <- this_chain_df$pp_u
              pp_sig <- this_chain_df$pp_sig

              pp_n <- nrow(this_chain_df)
            } else {
              pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)

              pp_u <- pp[, 1]
              pp_sig <- pp[, 2]

              pp_n <- nrow(this_chain_df)
            }
          }
        }

        tmp_results_df <- run_model(
          data_set = this_data_set,
          pp_u = pp_u,
          pp_sig = pp_sig
        )

        # publication
        pb <- publication_true(
          med = tmp_results_df$b_sex_cond_med,
          lower = tmp_results_df$b_sex_cond_lower,
          upper = tmp_results_df$b_sex_cond_lower,
          pb_prob_pos = pb_prob_pos,
          pb_prob_pos = pb_prob_pos,
          pb_prob_null = pb_prob_null
        )

        # if published
        if (pb == 1) {
          # new dataframe with saved PP values
          this_chain_df$pp_u <- tmp_results_df$b_sex_cond_med
          this_chain_df$pp_sig <- tmp_results_df$b_sex_cond_error
          this_chain_df$studyID <- this_data_set$studyID[1]

          # binding into df with all PP values
          chain_df <- rbind(chain_df, this_chain_df)
        }

        # save the results of the pp
        tmp_results_df <- tmp_results_df %>%
          mutate(
            expt = experiment,
            analysis_type = current_analysis_type,
            pub_method = current_pub_method,
            pub_true = ifelse(pb == 0, 0, 1),
            posteriors_included = pp_n
          )

        results_df <- rbind(results_df, tmp_results_df)

        # running meta analysis
        meta_data <- results_df %>%
          filter(analysis_type == current_analysis_type & pub_true == 1)
        tmp_meta_df <- running_meta_analysis(meta_data)

        meta_df <- rbind(meta_df, tmp_meta_df)
      } # end of pp_citation_sym

      print(">>>>>>>>>>>> Doing posterior passing on citation chain (pb asym)")
      # name analysis
      current_analysis_type <- "pp_citation_pb_asym"
      current_pub_method <- "asym"

      # empty df to save data in each iteration
      chain_df <- as.data.frame(matrix(0, nrow = 0, ncol = 3))
      colnames(chain_df) <- c("pp_u", "pp_sig", "studyID")

      # loop
      for (experiment in seq_len(n_experiments_per_repeat)) {
        print(paste(">>>> Model", experiment, "for citation_asym")) # , in repeat", rep, "with true effect", b_sex_cond, sep = " "))
        # for every experiment get the relevant data set
        this_data_set <- data_sets[data_sets$data_set == experiment, ]
        this_data_set$studyID <- as.character(this_data_set$studyID)
        this_chain_df <- as.data.frame(matrix(0, nrow = 1, ncol = 3))
        colnames(this_chain_df) <- colnames(chain_df)

        # set initial prior for b_sex_cond and update afterwards (if not first round, update pp_u and pp_sig)
        if (nrow(chain_df) == 0) {
          pp_u <- 0
          pp_sig <- 0.1

          pp_n <- 0 # adding a meassure for number of posteriors included
        } else {
          this_citation_chain <- citation_chain %>%
            filter(to == this_data_set$studyID[1]) %>%
            filter(from %in% chain_df$studyID)

          if (!(nrow(this_citation_chain) == 0)) {
            this_citation_chain <- tidy_chain(citation_chain, this_citation_chain)
          }

          if (nrow(this_citation_chain) == 0) {
            pp_u <- 0
            pp_sig <- 0.1

            pp_n <- 0
          } else {
            this_chain_df <- chain_df %>% filter(chain_df$studyID %in% this_citation_chain$from)

            if (nrow(this_chain_df) == 1) { # if one study is cited
              pp_u <- this_chain_df$pp_u
              pp_sig <- this_chain_df$pp_sig

              pp_n <- nrow(this_chain_df)
            } else {
              pp <- kalman(mean = this_chain_df$pp_u, sd = this_chain_df$pp_sig)

              pp_u <- pp[, 1]
              pp_sig <- pp[, 2]

              pp_n <- nrow(this_chain_df)
            }
          }
        }

        tmp_results_df <- run_model(
          data_set = this_data_set,
          pp_u = pp_u,
          pp_sig = pp_sig
        )

        # publication
        pb <- publication_true(
          med = tmp_results_df$b_sex_cond_med,
          lower = tmp_results_df$b_sex_cond_lower,
          upper = tmp_results_df$b_sex_cond_lower,
          pb_prob_pos = pb_prob_pos,
          pb_prob_pos = pb_prob_neg,
          pb_prob_null = pb_prob_null
        )

        # if published
        if (pb == 1) {
          # new dataframe with saved PP values
          this_chain_df$pp_u <- tmp_results_df$b_sex_cond_med
          this_chain_df$pp_sig <- tmp_results_df$b_sex_cond_error
          this_chain_df$studyID <- this_data_set$studyID[1]

          # binding into df with all PP values
          chain_df <- rbind(chain_df, this_chain_df)
        }

        # save the results of the pp
        tmp_results_df <- tmp_results_df %>%
          mutate(
            expt = experiment,
            analysis_type = current_analysis_type,
            pub_method = current_pub_method,
            pub_true = ifelse(pb == 0, 0, 1),
            posteriors_included = pp_n
          )

        results_df <- rbind(results_df, tmp_results_df)

        # running meta analysis
        meta_data <- results_df %>%
          filter(analysis_type == current_analysis_type & pub_true == 1)
        tmp_meta_df <- running_meta_analysis(meta_data)

        meta_df <- rbind(meta_df, tmp_meta_df)
      } # end of pp_citation_asym
    } # end of pp_citation with pb
  } # end of do_pp_citation

  if (!do_pp_linear && !do_pp_citation) {
    print("No analyses are being performed - please set do_pp_linear or do_pp_citation to TRUE")
  }

  results_df <- cbind(results_df, meta_df)

  return(results_df)
} # end of do_analyses


run_model <- function(data_set, pp_u, pp_sig) {
  # model formula
  model_f <- bf(response ~ sex * condition + (1 | participant_id))

  # prior values
  x <- paste("normal(", pp_u, ",", pp_sig, ")", sep = "")
  prior_m <- c(
    prior(normal(0.5, 0.1), class = Intercept),
    prior_string(x, class = "b", coef = "sex:condition"),
    prior(normal(0, 0.1), class = b, coef = "sex"),
    prior(normal(0, 0.1), class = b, coef = "condition"),
    prior(normal(0, 0.1), class = sd),
    prior(normal(0.5, 0.1), class = sigma)
  )

  # run model
  model <- brm(
    formula = model_f,
    data = data_set,
    family = gaussian,
    prior = prior_m,
    sample_prior = T,
    chains = 2,
    cores = 2
  )

  # save the results of the analysis
  model_results <- dplyr::tibble(
    b_base_med = fixef(model)[, 1][[1]],
    b_sex_med = fixef(model)[, 1][[2]],
    b_cond_med = fixef(model)[, 1][[3]],
    b_sex_cond_med = fixef(model)[, 1][[4]],
    b_base_lower = fixef(model)[, 3][[1]],
    b_sex_lower = fixef(model)[, 3][[2]],
    b_cond_lower = fixef(model)[, 3][[3]],
    b_sex_cond_lower = fixef(model)[, 3][[4]],
    b_base_upper = fixef(model)[, 4][[1]],
    b_sex_upper = fixef(model)[, 4][[2]],
    b_cond_upper = fixef(model)[, 4][[3]],
    b_sex_cond_upper = fixef(model)[, 4][[4]],
    b_base_error = fixef(model)[, 2][[1]],
    b_sex_error = fixef(model)[, 2][[2]],
    b_cond_error = fixef(model)[, 2][[3]],
    b_sex_cond_error = fixef(model)[, 2][[4]]
  )

  return(model_results)
}


running_meta_analysis <- function(meta_data) {
  # meta analysis without pb
  meta_f <- bf(b_sex_cond_med | se(b_sex_cond_error) ~ 1 + (1 | expt))

  prior_meta <- c(
    prior(normal(0, 0.1), class = Intercept),
    prior(normal(0, 0.1), class = sd)
  )

  model <- brm(
    formula = meta_f,
    data = meta_data,
    family = gaussian,
    prior = prior_meta,
    sample_prior = T,
    chains = 2,
    cores = 2
  )

  # save results
  meta_df <- dplyr::tibble(
    b_sex_cond_meta = fixef(model)[, 1][[1]],
    b_sex_cond_error_meta = fixef(model)[, 2][[1]],
    b_sex_cond_lower_meta = fixef(model)[, 3][[1]],
    b_sex_cond_upper_meta = fixef(model)[, 4][[1]],
    meta_n_exp = nrow(meta_data)
  )

  # save all results
  return(meta_df)
}


kalman <- function(mean, sd) {
  for (i in 1:length(mean)) {
    if (i == 2) {
      k <- sd[1] / (sd[1] + sd[2]) # kalman gain
      k_mean <- mean[1] + k * (mean[2] - mean[1]) # kalman mean
      k_sd <- sd[1] - (k * sd[1]) # kalman sd
    }
    if (i > 2) {
      k <- k_sd / (k_sd + sd[i])
      k_mean <- k_mean + k * (mean[i] - k_mean)
      k_sd <- k_sd - (k * k_sd)
    }
  }

  return(data.frame(
    mean = k_mean,
    sd = k_sd
  ))
}

publication_true <- function(
    med,
    lower,
    upper,
    pb_prob_pos,
    pb_prob_neg,
    pb_prob_null) {
  if (med > 0 && lower > 0) {
    pb_prob <- pb_prob_pos
  } else if (med < 0 && upper < 0) {
    pb_prob <- pb_prob_neg
  } else {
    pb_prob <- pb_prob_null
  }

  return(rbinom(1, size = 1, prob = pb_prob))
}
