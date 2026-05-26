read_prepare_data <- function(file) {
  
  data <- readxl::read_xlsx(file) |>
    janitor::clean_names() |>
    mutate(
      # add pre-post correlation assumption
      r_pre_post = 0.75,
      
      # calculate missing sd from se
      sd_pre = case_when(
        is.na(sd_pre) ~ se_pre * sqrt(n),
        .default = sd_pre
      ),
      
      sd_post = case_when(
        is.na(sd_post) ~ se_post * sqrt(n),
        .default = sd_post
      ),
      
      # reorder levels for recovery mode with control as reference
      recovery_mode = factor(recovery_mode,
                            levels = c(
                              "control",
                              "placebo",
                              "active", 
                              "bfr",
                              "cold",
                              "heat",
                              "contrast",
                              "compression",
                              "massage"
                            ))
    )
  
  
  # specify direction for each outcome indicating fatigue/decrement
  outcome_directions <- tibble(
    outcome_specific = c(
      "creatine_kinase"    ,
      "ext_iso_knee"       ,
      "cmj_peak_force"     ,
      "sprint_30_m"        ,
      "scale_0_10"         ,
      "scale_1_5"          ,
      "cmj"                ,
      "sprint_20m"         ,
      "peak_torque"        ,
      "scale_1_10"         ,
      "sj"                 ,
      "sprint_20_m"        ,
      "test_505"           ,
      "work_10_reps"       ,
      "peak_power"         ,
      "relative_peak_power"  ,
      "time_exhaustion"        ,
      "mvc"                ,
      "ext_ecc"            ,
      "ext_iso"            ,
      "ext_60"             ,
      "ext_120"            ,
      "ext_180"            ,
      "ext_240"            ,
      "ext_300"            ,
      "flex_iso"           ,
      "flex_60"            ,
      "flex_120"           ,
      "flex_180"           ,
      "flex_240"           ,
      "flex_300"           ,
      "scale_1_100"        ,
      "cmj_power"          ,
      "il6"                ,
      "sprint_5m"          ,
      "sprint_10m"        ,
      "torque"             ,
      "scale_0_100"        ,
      "isometric"          ,
      "ext_torque"         ,
      "flex_torque"        ,
      "force"              ,
      "cmj_mean_power"     ,
      "cmj_mean_force"     ,
      "sj_peak_force"      ,
      "relative_power"     ,
      "mean_power"         ,
      "total_work"         ,
      "sprint_total"       ,
      "flex_iso_knee"      ,
      "flex_iso_hip"       ,
      "dj"                 ,
      "fatigue_rate"       ,
      "scale_0_20"         ,
      "repeated_sprint"    ,
      "scale_6_20"         ,
      "sit_reach_cm"       ,
      "scale_0_7"          ,
      "rom_ankle"          ,
      "rom_knee"           ,
      "rom_hip"            ,
      "bench_throw"        ,
      "bench_iso"          ,
      "iso_torque"         ,
      "knee_rom"           
    ),
    outcome_direction = c(
      "increase",
      "decrease",
      "decrease",
      "increase",
      "increase",
      "increase",
      "decrease",
      "increase",
      "decrease",
      "increase",
      "decrease",
      "increase",
      "increase",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "increase",
      "increase",
      "increase",
      "decrease",
      "increase",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "increase",
      "decrease",
      "decrease",
      "decrease",
      "increase",
      "increase",
      "increase",
      "increase",
      "decrease",
      "increase",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease",
      "decrease"
    ))
  
  data <- left_join(data, outcome_directions, by = "outcome_specific")
  
  return(data)
}

calculate_effect_sizes_performance <- function(data) {
  
  # Performance data
  performance_data <- data |>
    filter(outcome_type == "performance")
  
  performance_data_decrease <- escalc(
    measure = "SMCR",
    m1i = mean_post,
    m2i = mean_pre,
    sd1i = sd_pre,
    # sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = performance_data |> filter(outcome_direction == "decrease")
  )
  
  performance_data_increase <- escalc(
    measure = "SMCR",
    m1i = mean_pre,
    m2i = mean_post,
    sd1i = sd_pre,
    # sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = performance_data |> filter(outcome_direction == "increase")
  )
  
  performance_data <- bind_rows(performance_data_decrease, performance_data_increase)  |>
    # add study weights/sizes for plotting
    mutate(
      wi = 1/sqrt(vi),
      size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))
  
  return(performance_data)
  
}

calculate_effect_sizes_biochemical <- function(data) {
  
  # Biochemical data
  biochemical_data <- data |>
    filter(outcome_type == "biochemical")
  
  biochemical_data<- escalc(
    measure = "SMCR",
    m1i = mean_pre,
    m2i = mean_post,
    sd1i = sd_pre,
    # sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = biochemical_data
  )
  
  biochemical_data <- biochemical_data|>
    # add study weights/sizes for plotting
    mutate(
      wi = 1/sqrt(vi),
      size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))
  
  return(biochemical_data)
  
}

calculate_effect_sizes_perceptual <- function(data) {
  
  # Perceptual data
  perceptual_data <- data |>
    filter(outcome_type == "perceptual")
  
  perceptual_data_decrease <- escalc(
    measure = "SMCRPH",
    m1i = mean_post,
    m2i = mean_pre,
    sd1i = sd_pre,
    sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = perceptual_data |> filter(outcome_direction == "decrease")
  )
  
  perceptual_data_increase <- escalc(
    measure = "SMCRPH",
    m1i = mean_pre,
    m2i = mean_post,
    sd1i = sd_pre,
    sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = perceptual_data |> filter(outcome_direction == "increase")
  )
  
  perceptual_data <- bind_rows(perceptual_data_decrease, perceptual_data_increase)  |>
    # add study weights/sizes for plotting
    mutate(
      wi = 1/sqrt(vi),
      size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))
  
  return(perceptual_data)
  
}

read_rob2_between_data <- function(file) {
  
  rob2_between <- readxl::read_excel(file, sheet = 3, range = "A1:V131") |>
    clean_names() |>
    select(unique_id, 
           randomization_process, 
           deviations_from_intended_interventions,
           missing_outcome_data,
           measurement_of_the_outcome,
           selection_of_the_reported_result,
           overall_bias,
           weight)
  
  # Summary percentages
  summary_rob2_between <- rob2_between |>
    pivot_longer(2:7,
                 names_to = "domain",
                 values_to = "judgement") |>
    group_by(domain, judgement) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(domain) |>
    mutate(
      pct = n / sum(n) * 100
    ) |>
    
    # Ensure all combinations exist
    ungroup() |>
    complete(
      domain,
      judgement = c("Low", "Some concerns", "High"),
      fill = list(n = 0, pct = 0)
    ) |>
    
    mutate(
      domain = case_when(
        domain == "randomization_process" ~ "Bias arising from the randomisation process", 
        domain == "deviations_from_intended_interventions" ~ "Bias due to deviations from intended interventions",
        domain == "missing_outcome_data" ~ "Bias due to missing outcome data",
        domain == "measurement_of_the_outcome" ~ "Bias due to measurement of the outcome",
        domain == "selection_of_the_reported_result" ~ "Bias due to selection of the reported result",
        domain == "overall_bias" ~ "Overall bias"
      ),
      domain = factor(domain, 
                      levels = c(
                        "Bias arising from the randomisation process", 
                        "Bias due to deviations from intended interventions",
                        "Bias due to missing outcome data",
                        "Bias due to measurement of the outcome",
                        "Bias due to selection of the reported result",
                        "Overall bias"
                      )),
      judgement = factor(judgement,
                         levels = c(
                           "High",
                           "Some concerns",
                           "Low"
                         ))
    )
  
}


read_rob2_within_data <- function(file) {
  
  rob2_within <- readxl::read_excel("data/RoB2 Repeated.xlsm", sheet = 3, range = "A1:X93") |>
    clean_names() |>
    select(unique_id, 
           randomization_process, 
           bias_arising_from_period_and_carryover_effects,
           deviations_from_intended_interventions,
           missing_outcome_data,
           measurement_of_the_outcome,
           selection_of_the_reported_result,
           overall_bias,
           weight)
  
  # Summary percentages
  summary_rob2_within <- rob2_within |>
    pivot_longer(2:8,
                 names_to = "domain",
                 values_to = "judgement") |>
    group_by(domain, judgement) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(domain) |>
    mutate(
      pct = n / sum(n) * 100
    ) |>
    
    # Ensure all combinations exist
    ungroup() |>
    complete(
      domain,
      judgement = c("Low", "Some concerns", "High"),
      fill = list(n = 0, pct = 0)
    ) |>
    
    mutate(
      domain = case_when(
        domain == "randomization_process" ~ "Bias arising from the randomisation process", 
        domain == "bias_arising_from_period_and_carryover_effects" ~ "Bias arising from period and carryover effects",
        domain == "deviations_from_intended_interventions" ~ "Bias due to deviations from intended interventions",
        domain == "missing_outcome_data" ~ "Bias due to missing outcome data",
        domain == "measurement_of_the_outcome" ~ "Bias due to measurement of the outcome",
        domain == "selection_of_the_reported_result" ~ "Bias due to selection of the reported result",
        domain == "overall_bias" ~ "Overall bias"
      ),
      domain = factor(domain, 
                      levels = c(
                        "Bias arising from the randomisation process", 
                        "Bias arising from period and carryover effects",
                        "Bias due to deviations from intended interventions",
                        "Bias due to missing outcome data",
                        "Bias due to measurement of the outcome",
                        "Bias due to selection of the reported result",
                        "Overall bias"
                      )),
      judgement = factor(judgement,
                         levels = c(
                           "High",
                           "Some concerns",
                           "Low"
                         ))
    )
  
}
