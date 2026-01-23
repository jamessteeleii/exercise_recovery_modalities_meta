library(tidyverse)
library(metafor)
library(brms)
library(tidybayes)
library(marginaleffects)

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
    "decrease"
  ))

data <- left_join(data, outcome_directions, by = "outcome_specific")


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

data <- bind_rows(data_decrease, data_increase) |>
  # add study weights/sizes
  mutate(
    wi = 1/sqrt(vi),
    size = 0.5 + 3.0 * (wi - min(wi, na.rm=TRUE))/(max(wi, na.rm=TRUE) - min(wi, na.rm=TRUE)))


# data |>
#   filter(outcome_type == "performance") |>
#   ggplot(aes(x=yi)) +
#   geom_histogram()
# 
# data |>
#   filter(
#     # outcome_type == "performance",
#     time_hours <= 72
#   ) |>
#   ggplot(aes(x=time_hours, y=yi)) +
#   geom_point() +
#   geom_line(aes(group = interaction(study_id, arm, outcome_specific)),
#             alpha = 0.5) +
#   # geom_smooth() +
#   ggh4x::facet_grid2(recovery_mode ~ outcome_type,
#                      scales = "free")


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores() - 1)

priors <- c(
  set_prior("student_t(3,0,10)", class = "b")
)


primary_model <- brm(
  yi | se(sqrt(vi)) ~ 1 +  time_hours + recovery_mode:time_hours +
    (1 + time_hours + recovery_mode:time_hours | study_id) +
    (1 + time_hours | study_id:arm) +
    (1 | study_id:arm:outcome_specific),
  data = data,
  prior = priors,
  chains = 4,
  cores = 4,
  seed = 1988,
  warmup = 2000,
  iter = 4000
)

plot(primary_model)4

pp_check(primary_model)

preds <- predictions(primary_model, 
                     re_formula= NA,
                     newdata   = datagrid(
                       time_hours = c(0,72),
                       recovery_mode = unique(data$recovery_mode),
                       vi = 0
                       )
                     )


preds_plot <- preds |> 
  ggplot(aes(x = time_hours, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.75) +
  geom_point(data = data, 
             aes(x = time_hours, y = yi, size = size), alpha = 0.05) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.5, color = "black", size = 0.25) +
  geom_line(aes(y = estimate), size = 0.75, color = "black") +
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
  scale_y_continuous(limits = c(-10,10)) +
  scale_x_continuous(limits = c(0,72))



# model <- brm(
#   yi | se(sqrt(vi)) ~ 1 +  time_hours + recovery_mode:time_hours + outcome_type + recovery_mode:time_hours:outcome_type +
#     # (time_hours + recovery_mode:time_hours + outcome_type + recovery_mode:time_hours:outcome_type | study_id) +
#     (1 | study_id) +
#     (1 | study_id:arm) +
#     (1 | study_id:arm:outcome_specific),
#   data = data,
#   prior = priors,
#   chains = 4,
#   cores = 4,
#   seed = 1988,
#   warmup = 2000,
#   iter = 4000
# )
