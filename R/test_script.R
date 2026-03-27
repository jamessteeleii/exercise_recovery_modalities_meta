library(tidyverse)
library(metafor)
library(cmdstanr)
library(brms)
library(ordbetareg)
library(tidybayes)
library(marginaleffects)
library(patchwork)


data <- readxl::read_xlsx("data/data.xlsx") |>
  janitor::clean_names()


data <- data |>
  mutate(
    r_pre_post = 0.75,
    sd_pre = case_when(
      is.na(sd_pre) ~ se_pre * sqrt(n),
      .default = sd_pre
    )
  )

data <- data |>
  mutate(
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

# perceptual_data <- data |>
#   filter(outcome_type == "perceptual") |>
#   mutate(outcome_direction = case_when(
#     outcome_measure == "soreness" ~ "increase",
#     outcome_measure == "recovery" ~ "decrease",
#     outcome_measure == "fatigue" ~ "increase",
#     outcome_measure == "pain" ~ "increase"
#   )) |>
#   separate(outcome_specific, into = c("outcome_specific", "lower_bound", "upper_bound")) |>
#   mutate(lower_bound = as.numeric(lower_bound),
#          upper_bound = as.numeric(upper_bound)) |>
#   mutate(
#     mean_pre_norm = (mean_pre - lower_bound) / (upper_bound - lower_bound),
#     mean_post_norm = (mean_post - lower_bound) / (upper_bound - lower_bound),
#     yi = case_when(
#       outcome_direction == "decrease" ~ mean_pre_norm - mean_post_norm,
#       outcome_direction == "increase" ~ mean_post_norm - mean_pre_norm,
#       .default = NA_real_
#     )
#   ) |>
#   # add study weights/sizes for plotting
#   mutate(
#     wi = 1/n,
#     size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))

# 
# rstan_options(auto_write = TRUE)
# options(mc.cores = parallel::detectCores() - 1)

priors <- c(
  set_prior("student_t(3,0,2.5)", class = "b")
)


                  # ##### Performance outcomes model
                  # primary_model_performance <- brm(
                  #   yi | se(sqrt(vi)) ~ 1 +  time_hours + recovery_mode:time_hours +
                  #     (1 + time_hours + recovery_mode:time_hours || study_id) +
                  #     (1 + time_hours || study_id:arm) +
                  #     (1 | study_id:arm:outcome_specific),
                  #   data = performance_data,
                  #   prior = priors,
                  #   chains = 4,
                  #   cores = 4,
                  #   seed = 1988,
                  #   warmup = 2000,
                  #   iter = 8000,
                  #   control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15),
                  #   backend = "cmdstanr"
                  # )
                  # 
                  # plot(primary_model_performance)
                  # 
                  # pp_check(primary_model_performance) 
                  # 
                  # preds_performance <- predictions(primary_model_performance, 
                  #                      re_formula= NA,
                  #                      newdata   = datagrid(
                  #                        time_hours = seq(0,72, by = 4),
                  #                        recovery_mode = unique(performance_data$recovery_mode),
                  #                        vi = 0
                  #                      )
                  # ) |>
                  #   mutate(
                  #     recovery_mode = factor(recovery_mode,
                  #                            levels = c(
                  #                              "control",
                  #                              "placebo",
                  #                              "active", 
                  #                              "cold",
                  #                              "heat",
                  #                              "contrast",
                  #                              "bfr",
                  #                              "compression",
                  #                              "massage"
                  #                            ))
                  #   )
                  # 
                  # 
                  # preds_plot_performance <- preds_performance |> 
                  #   # filter(recovery_mode != "heat" &
                  #   #          recovery_mode != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_point(data = performance_data ,
                  #              aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Fatigue (SMD)",
                  #     title = "Predicted fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   facet_wrap("recovery_mode") +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # preds_plot_performance
                  # 
                  # contr_performance <- comparisons(primary_model_performance, 
                  #                      re_formula= NA,
                  #                      newdata   = datagrid(
                  #                        time_hours = seq(0,72, by = 12),
                  #                        vi = 0
                  #                      ),
                  #                      variables = list(recovery_mode = "pairwise"))
                  # 
                  # contr_performance <- contr_performance |>
                  #   separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
                  #   mutate(
                  #     `Condition One` = factor(`Condition One`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              )),
                  #     `Condition Two` = factor(`Condition Two`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              ))
                  #   ) |>
                  #   mutate(cond_one_lab = "Condition One",
                  #          cond_two_lab = "Condition Two")
                  # 
                  # contr_plot_performance <- contr_performance |>
                  #   filter(`Condition Two` != "heat" &
                  #            `Condition Two` != "massage" &
                  #            `Condition One` != "heat" &
                  #            `Condition One` != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Difference in Fatigue (SMD) i.e., Condition One - Condition Two",
                  #     title = "Between condition contrasts in fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   ggh4x::facet_nested(
                  #     cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One` 
                  #   ) +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   # scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # contr_plot_performance
                  # 
                  # ##### Biochemical outcomes model
                  # primary_model_biochemical <- brm(
                  #   yi | se(sqrt(vi)) ~ 1 +  time_hours + recovery_mode:time_hours +
                  #     (1 + time_hours + recovery_mode:time_hours || study_id) +
                  #     (1 + time_hours || study_id:arm) +
                  #     (1 | study_id:arm:outcome_specific),
                  #   data = biochemical_data,
                  #   prior = priors,
                  #   chains = 4,
                  #   cores = 4,
                  #   seed = 1988,
                  #   warmup = 2000,
                  #   iter = 8000,
                  #   control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15),
                  #   backend = "cmdstanr"
                  #   )
                  # 
                  # plot(primary_model_biochemical)
                  # 
                  # pp_check(primary_model_biochemical) 
                  # 
                  # preds_biochemical <- predictions(primary_model_biochemical, 
                  #                                  re_formula= NA,
                  #                                  newdata   = datagrid(
                  #                                    time_hours = seq(0,72, by = 4),
                  #                                    recovery_mode = unique(biochemical_data$recovery_mode),
                  #                                    vi = 0
                  #                                  )
                  # ) |>
                  #   mutate(
                  #     recovery_mode = factor(recovery_mode,
                  #                            levels = c(
                  #                              "control",
                  #                              "placebo",
                  #                              "active", 
                  #                              "bfr",
                  #                              "cold",
                  #                              "heat",
                  #                              "contrast",
                  #                              "compression",
                  #                              "massage"
                  #                            ))
                  #   )
                  # 
                  # 
                  # preds_plot_biochemical <- preds_biochemical |> 
                  #   filter(recovery_mode != "heat" &
                  #            recovery_mode != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_point(data = biochemical_data,
                  #              aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Fatigue (SMD)",
                  #     title = "Predicted fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   facet_wrap("recovery_mode") +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # preds_plot_biochemical
                  # 
                  # contr_biochemical <- comparisons(primary_model_biochemical, 
                  #                                  re_formula= NA,
                  #                                  newdata   = datagrid(
                  #                                    time_hours = seq(0,72, by = 12),
                  #                                    vi = 0
                  #                                  ),
                  #                                  variables = list(recovery_mode = "pairwise"))
                  # 
                  # contr_biochemical <- contr_biochemical |>
                  #   separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
                  #   mutate(
                  #     `Condition One` = factor(`Condition One`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              )),
                  #     `Condition Two` = factor(`Condition Two`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              ))
                  #   ) |>
                  #   mutate(cond_one_lab = "Condition One",
                  #          cond_two_lab = "Condition Two")
                  # 
                  # contr_plot_biochemical <- contr_biochemical |>
                  #   filter(`Condition Two` != "heat" &
                  #            `Condition Two` != "massage" &
                  #            `Condition One` != "heat" &
                  #            `Condition One` != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Difference in Fatigue (SMD)",
                  #     title = "Between condition contrasts in fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   ggh4x::facet_nested(
                  #     cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One` 
                  #   ) +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # contr_plot_biochemical
                  # 
                  # ##### perceptual outcomes model
                  # primary_model_perceptual <- brm(
                  #   yi | se(sqrt(vi)) ~ 1 +  time_hours + recovery_mode:time_hours +
                  #     (1 + time_hours + recovery_mode:time_hours || study_id) +
                  #     (1 + time_hours || study_id:arm) +
                  #     (1 | study_id:arm:outcome_specific),
                  #   data = perceptual_data,
                  #   prior = priors,
                  #   chains = 4,
                  #   cores = 4,
                  #   seed = 1988,
                  #   warmup = 2000,
                  #   iter = 8000,
                  #   control = list(adapt_delta = 0.99, stepsize = 0.01, max_treedepth = 15),
                  #   backend = "cmdstanr"
                  # )
                  # 
                  # plot(primary_model_perceptual)
                  # 
                  # pp_check(primary_model_perceptual) 
                  # 
                  # preds_perceptual <- predictions(primary_model_perceptual, 
                  #                                  re_formula= NA,
                  #                                  newdata   = datagrid(
                  #                                    time_hours = seq(0,72, by = 4),
                  #                                    recovery_mode = unique(perceptual_data$recovery_mode),
                  #                                    vi = 0
                  #                                  )
                  # ) |>
                  #   mutate(
                  #     recovery_mode = factor(recovery_mode,
                  #                            levels = c(
                  #                              "control",
                  #                              "placebo",
                  #                              "active", 
                  #                              "bfr",
                  #                              "cold",
                  #                              "heat",
                  #                              "contrast",
                  #                              "compression",
                  #                              "massage"
                  #                            ))
                  #   )
                  # 
                  # 
                  # preds_plot_perceptual <- preds_perceptual |> 
                  #   filter(recovery_mode != "heat" &
                  #            recovery_mode != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_point(data = perceptual_data,
                  #              aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Fatigue (SMD)",
                  #     title = "Predicted fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   facet_wrap("recovery_mode") +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # preds_plot_perceptual
                  # 
                  # contr_perceptual <- comparisons(primary_model_perceptual, 
                  #                                  re_formula= NA,
                  #                                  newdata   = datagrid(
                  #                                    time_hours = seq(0,72, by = 12),
                  #                                    vi = 0
                  #                                  ),
                  #                                  variables = list(recovery_mode = "pairwise"))
                  # 
                  # contr_perceptual <- contr_perceptual |>
                  #   separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
                  #   mutate(
                  #     `Condition One` = factor(`Condition One`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              )),
                  #     `Condition Two` = factor(`Condition Two`,
                  #                              levels = c(
                  #                                "control",
                  #                                "placebo",
                  #                                "active", 
                  #                                "bfr",
                  #                                "cold",
                  #                                "heat",
                  #                                "contrast",
                  #                                "compression",
                  #                                "massage"
                  #                              ))
                  #   ) |>
                  #   mutate(cond_one_lab = "Condition One",
                  #          cond_two_lab = "Condition Two")
                  # 
                  # contr_plot_perceptual <- contr_perceptual |>
                  #   filter(`Condition Two` != "heat" &
                  #            `Condition Two` != "massage" &
                  #            `Condition One` != "heat" &
                  #            `Condition One` != "massage") |>
                  #   ggplot(aes(x = time_hours, y = estimate)) +
                  #   geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
                  #   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                  #               alpha = 0.5, color = "black", size = 0.25) +
                  #   geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
                  #   labs(
                  #     x = "Time Post Exercise (hours)",
                  #     y = "Difference in Fatigue (SMD)",
                  #     title = "Between condition contrasts in fatigue/recovery",
                  #   ) +
                  #   guides(
                  #     size = "none"
                  #   ) +
                  #   ggh4x::facet_nested(
                  #     cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One` 
                  #   ) +
                  #   theme_classic() +
                  #   theme(
                  #     plot.title = element_text(size = 12),
                  #     axis.title = element_text(size = 10)
                  #   ) +
                  #   scale_y_continuous(limits = c(-7.5,7.5)) +
                  #   scale_x_continuous(limits = c(0,72))
                  # 
                  # 
                  # contr_plot_perceptual


###### Add smooth terms for time

  ##### performance_smooth outcomes model

  performance_data <- performance_data |>
    filter(!is.na(yi) | !is.na(vi))

  primary_model_performance_smooth <- brm(
    yi | se(sqrt(vi)) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
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

plot(primary_model_performance_smooth)

pp_check(primary_model_performance_smooth) 

preds_performance_smooth <- predictions(primary_model_performance_smooth, 
                                 re_formula= NA,
                                 newdata = primary_model_performance_smooth$data
                                 
) |>
  mutate(
    recovery_mode = factor(recovery_mode,
                           levels = c(
                             "control",
                             "placebo",
                             "active", 
                             "cold",
                             "heat",
                             "contrast",
                             "bfr",
                             "compression",
                             "massage"
                           ))
  )


preds_plot_performance_smooth <- preds_performance_smooth |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_point(data = performance_data,
             aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Fatigue (SMD)",
    title = "Predicted fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  facet_wrap("recovery_mode") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))


preds_plot_performance_smooth

contr_performance_smooth <- comparisons(primary_model_performance_smooth, 
                                 re_formula= NA,
                                 newdata   = datagrid(
                                   time_hours = seq(0,120, by = 4),
                                   vi = 0
                                 ),
                                 variables = list(recovery_mode = "pairwise"))

contr_performance_smooth <- contr_performance_smooth |>
  separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
  mutate(
    `Condition One` = factor(`Condition One`,
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
                             )),
    `Condition Two` = factor(`Condition Two`,
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
  ) |>
  mutate(cond_one_lab = "Condition One",
         cond_two_lab = "Condition Two")

contr_plot_performance_smooth <- contr_performance_smooth |>
  # filter(`Condition Two` != "heat" &
  #          `Condition Two` != "massage" &
  #          `Condition One` != "heat" &
  #          `Condition One` != "massage") |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  # geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.75) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Difference in Fatigue (SMD) i.e., Condition One - Condition Two",
    title = "Between condition contrasts in fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  ggh4x::facet_nested(
    cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One`,
    scales = "free_y"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  # scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24)) +
  theme(
    axis.text.x = element_text(size = 6)
  )


contr_plot_performance_smooth

performance_plot <- (preds_plot_performance_smooth + contr_plot_performance_smooth) +
  plot_annotation(title = "Performance Outcomes")

performance_plot

ggsave("performance_plot.png", device = "png", dpi = 300, width = 16, height = 8)

##### biochemical_smooth outcomes model
biochemical_data <- biochemical_data |>
  filter(!is.na(yi) | !is.na(vi))

primary_model_biochemical_smooth <- brm(
  yi | se(sqrt(vi)) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
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

plot(primary_model_biochemical_smooth)

pp_check(primary_model_biochemical_smooth) 

preds_biochemical_smooth <- predictions(primary_model_biochemical_smooth, 
                                        re_formula= NA,
                                        newdata = primary_model_biochemical_smooth$data
                                        
) |>
  mutate(
    recovery_mode = factor(recovery_mode,
                           levels = c(
                             "control",
                             "placebo",
                             "active", 
                             "cold",
                             "heat",
                             "contrast",
                             "bfr",
                             "compression",
                             "massage"
                           ))
  )


preds_plot_biochemical_smooth <- preds_biochemical_smooth |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_point(data = biochemical_data,
             aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Fatigue (SMD)",
    title = "Predicted fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  facet_wrap("recovery_mode") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(-7.5,7.5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))


preds_plot_biochemical_smooth

contr_biochemical_smooth <- comparisons(primary_model_biochemical_smooth, 
                                        re_formula= NA,
                                        newdata   = datagrid(
                                          time_hours = seq(0,120, by = 4),
                                          vi = 0
                                        ),
                                        variables = list(recovery_mode = "pairwise"))

contr_biochemical_smooth <- contr_biochemical_smooth |>
  separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
  mutate(
    `Condition One` = factor(`Condition One`,
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
                             )),
    `Condition Two` = factor(`Condition Two`,
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
  ) |>
  mutate(cond_one_lab = "Condition One",
         cond_two_lab = "Condition Two")

contr_plot_biochemical_smooth <- contr_biochemical_smooth |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  # geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.75) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Difference in Fatigue (SMD) i.e., Condition One - Condition Two",
    title = "Between condition contrasts in fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  ggh4x::facet_nested(
    cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One` 
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  # scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24)) +
  theme(
    axis.text.x = element_text(size = 6)
  )


contr_plot_biochemical_smooth

biochemical_plot <- (preds_plot_biochemical_smooth + contr_plot_biochemical_smooth) +
  plot_annotation(title = "Biochemical Outcomes")

biochemical_plot

ggsave("biochemical_plot.png", device = "png", dpi = 300, width = 16, height = 8)


##### perceptual_smooth outcomes model
perceptual_data <- perceptual_data |>
  filter(!is.na(yi) | !is.na(vi)) |>
  mutate(recovery_mode = factor(recovery_mode,
                                levels = c("control",
                                           "placebo",
                                           "active",
                                           "bfr",
                                           "cold",
                                           "contrast",
                                           "compression")))

primary_model_perceptual_smooth <- brm(
  yi | se(sqrt(vi)) ~ 1 +  s(time_hours, bs='tp') + s(time_hours, by = recovery_mode, bs='tp') +
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

plot(primary_model_perceptual_smooth)

pp_check(primary_model_perceptual_smooth) 

preds_perceptual_smooth <- predictions(primary_model_perceptual_smooth, 
                                        re_formula= NA,
                                       newdata = primary_model_perceptual_smooth$data
) |>
  mutate(
    recovery_mode = factor(recovery_mode,
                           levels = c(
                             "control",
                             "placebo",
                             "active", 
                             "cold",
                             "contrast",
                             "bfr",
                             "compression"
                           ))
  )


preds_plot_perceptual_smooth <- preds_perceptual_smooth |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_point(data = perceptual_data,
             aes(x = time_hours, y = yi, size = size), alpha = 0.25) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Fatigue (SMD)",
    title = "Predicted fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  facet_wrap("recovery_mode") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  scale_y_continuous(limits = c(-5,5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))


preds_plot_perceptual_smooth

contr_perceptual_smooth <- comparisons(primary_model_perceptual_smooth, 
                                        re_formula= NA,
                                        newdata   = datagrid(
                                          time_hours = seq(0,120, by = 4),
                                          vi = 0
                                        ),
                                        variables = list(recovery_mode = "pairwise"))

contr_perceptual_smooth <- contr_perceptual_smooth |>
  separate(contrast, sep = " - ", into = c("Condition One", "Condition Two")) |>
  mutate(
    `Condition One` = factor(`Condition One`,
                             levels = c(
                               "control",
                               "placebo",
                               "active", 
                               "bfr",
                               "cold",
                               "contrast",
                               "compression"
                             )),
    `Condition Two` = factor(`Condition Two`,
                             levels = c(
                               "control",
                               "placebo",
                               "active", 
                               "bfr",
                               "cold",
                               "contrast",
                               "compression"
                             ))
  ) |>
  mutate(cond_one_lab = "Condition One",
         cond_two_lab = "Condition Two")

contr_plot_perceptual_smooth <- contr_perceptual_smooth |>
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  # geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", alpha = 0.75) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), linewidth = 0.75, color = "black") +
  labs(
    x = "Time Post Exercise (hours)",
    y = "Difference in Fatigue (SMD) i.e., Condition One - Condition Two",
    title = "Between condition contrasts in fatigue/recovery",
  ) +
  guides(
    size = "none"
  ) +
  ggh4x::facet_nested(
    cond_two_lab + `Condition Two` ~ cond_one_lab + `Condition One` 
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 10)
  ) +
  # scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))


contr_plot_perceptual_smooth

perceptual_plot <- (preds_plot_perceptual_smooth + contr_plot_perceptual_smooth) +
  plot_annotation(title = "Perceptual Outcomes")

perceptual_plot

ggsave("perceptual_plot.png", device = "png", dpi = 300, width = 16, height = 8)



