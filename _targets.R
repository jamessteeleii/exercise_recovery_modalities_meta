# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "here",
    "metafor",
    "brms",
    "bayesplot",
    "marginaleffects",
    "patchwork"
  ), # Packages that your targets need for their tasks.
  memory = "transient",
  format = "qs",
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/functions/.")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  
  #### Read in and prepare data, calculate effect sizes etc. ----
  tar_target(
    all_data_prepared_file,
    here("data", "data.xlsx"),
    format = "file"
  ),
  
  tar_target(
    all_data_prepared,
    read_prepare_data(all_data_prepared_file)
  ),
  
  tar_target(
    performance_data,
    calculate_effect_sizes_performance(all_data_prepared)
  ),
  
  tar_target(
    biochemical_data,
    calculate_effect_sizes_biochemical(all_data_prepared)
  ),
  
  tar_target(
    perceptual_data,
    calculate_effect_sizes_perceptual(all_data_prepared)
  ),
  
  #### Fit models ----
  tar_target(
    weak_prior,
    {
      priors <- c(
        set_prior("student_t(3,0,2.5)", class = "b")
      )
    }
  ),
  
  tar_target(
    performance_model,
    fit_model_performance(performance_data, weak_prior)
  ),
  
  tar_target(
    biochemical_model,
    fit_model_biochemical(biochemical_data, weak_prior)
  ),
  
  tar_target(
    perceptual_model,
    fit_model_perceptual(perceptual_data, weak_prior)
  ),
  
  #### Model checks ----
  tar_target(
    rhat_performance_model,
    make_rhat_plot(performance_model)
  ),
  
  tar_target(
    trace_plot_performance_model,
    make_trace_plot(performance_model)
  ),
  
  tar_target(
    pp_check_performance_model,
    make_pp_check(performance_model)
  ),
  
  tar_target(
    rhat_biochemical_model,
    make_rhat_plot(biochemical_model)
  ),
  
  tar_target(
    trace_plot_biochemical_model,
    make_trace_plot(biochemical_model)
  ),
  
  tar_target(
    pp_check_biochemical_model,
    make_pp_check(biochemical_model)
  ),
  
  tar_target(
    rhat_perceptual_model,
    make_rhat_plot(perceptual_model)
  ),
  
  tar_target(
    trace_plot_perceptual_model,
    make_trace_plot(perceptual_model)
  ),
  
  tar_target(
    pp_check_perceptual_model,
    make_pp_check(perceptual_model)
  ),
  
  #### Plot results ----
  tar_target(
    performance_plot,
    plot_performance_model(performance_model)
  ),
  
  tar_target(
    performance_plot_tiff,
    {
      performance_plot
      
      ggsave(plot = performance_plot, filename = "plots/performance_plot.tiff", device = "tiff", dpi = 300, width = 10, height = 6)
      
    }
  ),
  
  tar_target(
    biochemical_plot,
    plot_biochemical_model(biochemical_model)
  ),
  
  tar_target(
    biochemical_plot_tiff,
    {
      biochemical_plot
      
      ggsave(plot = biochemical_plot, filename = "plots/biochemical_plot.tiff", device = "tiff", dpi = 300, width = 10, height = 6)
      
    }
  ),
  
  tar_target(
    perceptual_plot,
    plot_perceptual_model(perceptual_model)
  ),
  
  tar_target(
    perceptual_plot_tiff,
    {
      perceptual_plot
      
      ggsave(plot = perceptual_plot, filename = "plots/perceptual_plot.tiff", device = "tiff", dpi = 300, width = 10, height = 6)
      
    }
  )
)
