read_prepare_data <- function(file) {
  
  data <- readxl::read_xlsx(file) |>
    janitor::clean_names() |>
    mutate(
      # add pre-post correlation assumption
      r_pre_post = 0.75,
      
      # calculate missing baseline sd from se
      sd_pre = case_when(
        is.na(sd_pre) ~ se_pre * sqrt(n),
        .default = sd_pre
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