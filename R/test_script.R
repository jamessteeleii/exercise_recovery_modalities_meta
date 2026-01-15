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


data <- escalc(
  measure = "SMCR",
  m1i = mean_post,
  m2i = mean_pre,
  sd1i = sd_pre,
  # sd2i = sd_post,
  ni = n,
  ri = r_pre_post,
  data = data
)


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
