fit_model_performance <- function(performance_data, priors)  {
  
  performance_data <- performance_data |>
    filter(!is.na(yi) | !is.na(vi))
  
  model_performance <- brm(
    yi | se(sqrt(vi), sigma = TRUE) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
      (1 + time_hours + recovery_mode:time_hours || study_id) +
      (1 + time_hours || study_id:arm) +
      (1 | study_id:arm:outcome_specific),
    data = performance_data,
    prior = priors,
    chains = 4,
    cores = 4,
    seed = 1988,
    warmup = 2000,
    iter = 8000,
    # control = list(adapt_delta = 0.99),
    backend = "cmdstanr"
  )
  
  return(model_performance)
}

fit_model_biochemical <- function(biochemical_data, priors)  {
  
  biochemical_data <- biochemical_data |>
    filter(!is.na(yi) | !is.na(vi))
  
  model_biochemical <- brm(
    yi | se(sqrt(vi), sigma = TRUE) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
      (1 + time_hours + recovery_mode:time_hours || study_id) +
      (1 + time_hours || study_id:arm) +
      (1 | study_id:arm:outcome_specific),
    data = biochemical_data,
    prior = priors,
    chains = 4,
    cores = 4,
    seed = 1988,
    warmup = 2000,
    iter = 8000,
    # control = list(adapt_delta = 0.99),
    backend = "cmdstanr"
  )
  
  return(model_biochemical)
}

fit_model_perceptual <- function(perceptual_data, priors)  {
  
  perceptual_data <- perceptual_data |>
    filter(!is.na(yi) | !is.na(vi)) |>
    
    # re-level recovery mode due to missing modalities for perceptual outcomes
    mutate(recovery_mode = factor(recovery_mode,
                                  levels = c("control",
                                             "placebo",
                                             "active",
                                             "bfr",
                                             "cold",
                                             "contrast",
                                             "compression",
                                             "massage")))
  
  model_perceptual <- brm(
    yi | se(sqrt(vi), sigma = TRUE) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
      (1 + time_hours + recovery_mode:time_hours || study_id) +
      (1 + time_hours || study_id:arm) +
      (1 | study_id:arm:outcome_specific),
    data = perceptual_data,
    prior = priors,
    chains = 4,
    cores = 4,
    seed = 1988,
    warmup = 2000,
    iter = 8000,
    # control = list(adapt_delta = 0.99),
    backend = "cmdstanr"
  )
  
  return(model_perceptual)
}

# Model checks
make_rhat_plot <- function(model) {
  mod_rhat <- enframe(brms::rhat(model)) |>
    filter(!str_detect(name, "^r_id"))
  
  rhat_main_params <- mod_rhat$value
  
  mcmc_rhat(rhat_main_params) +
    scale_x_continuous(breaks = c(1, 1.01, 1.02, 1.03, 1.04, 1.05)) +
    geom_vline(xintercept = 1.01,
               linetype = "dashed",
               alpha = 0.25)
}

make_trace_plot <- function(model) {
  plot(model)
}

make_pp_check <- function(model) {
  pp_check(model)
}
