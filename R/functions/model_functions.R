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
    
    # relevel recovery mode due to missing modalities for perceptual outcomes
    mutate(recovery_mode = factor(recovery_mode,
                                  levels = c("control",
                                             "placebo",
                                             "active",
                                             "bfr",
                                             "cold",
                                             "contrast",
                                             "compression")))
  
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
