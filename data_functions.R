read_prepare_data <- function(file) {
  
  data <- readxl::read_xlsx(file) |>
    janitor::clean_names() |>
    mutate(
      
      # add assumed pre-post correlation
      r_pre_post = 0.75,
      
      # calculate pre SD from SE
      sd_pre = case_when(
        is.na(sd_pre) ~ se_pre * sqrt(n),
        .default = sd_pre
      ),
      
      # order factor levels of recovery mode
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
  
  
  # specify directions for outcomes that indicate fatigue/impairment
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
  
}

calculate_effect_sizes <- function(data, outcome) {
  
  data <- data |>
    filter(outcome_type == outcome)
  
  data_decrease <- escalc(
    measure = "SMCR",
    m1i = mean_post,
    m2i = mean_pre,
    sd1i = sd_pre,
    # sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = data |> filter(outcome_direction == "decrease")
  )
  
  data_increase <- escalc(
    measure = "SMCR",
    m1i = mean_pre,
    m2i = mean_post,
    sd1i = sd_pre,
    # sd2i = sd_post,
    ni = n,
    ri = r_pre_post,
    data = data |> filter(outcome_direction == "increase")
  )
  
  data <- bind_rows(data_decrease, data_increase)  |>
    # add study weights/sizes for plotting
    mutate(
      wi = 1/sqrt(vi),
      size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))
  
}