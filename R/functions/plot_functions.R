plot_performance_model <- function(performance_model) {
  
  max_time <- performance_model$data |>
    group_by(recovery_mode) |>
    summarise(max_time = max(time_hours, na.rm = TRUE))
  
  pred_grid <- max_time |>
    rowwise() |>
    mutate(
      time_hours = list(seq(0, max_time, by = 4))
    ) |>
    unnest(time_hours) |>
    ungroup() |>
    mutate(vi = 0)
  
  preds <- predictions(performance_model, 
                                          re_formula= NA,
                                          newdata = pred_grid
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
  
  
  preds_plot <- preds |>
    ggplot(aes(x = time_hours, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.5, color = "black", size = 0.25) +
    geom_line(aes(y = estimate), color = "black") +
    labs(
      x = "Time Post Exercise (hours)",
      y = "Fatigue (SMD)",
      title = "Predicted fatigue/recovery",
    ) +
    guides(
      size = "none"
    ) +
    facet_wrap("recovery_mode",
               labeller = labeller(
                 recovery_mode = function(x) {
                   x <- str_to_title(x)
                   x[x == "Bfr"] <- "Blood Flow Restriction"
                   x
                 }
               )) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5, 1.5, by = 0.5)) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  contr_ref <- comparisons(
    performance_model,
    re_formula = NA,
    newdata = datagrid(
      recovery_mode = levels(performance_model$data$recovery_mode),
      time_hours = seq(0,120, by = 4),
      vi = 0
    ),
    variables = list(recovery_mode = "reference"))
  
  legend_df <- tibble(
    contrast = "",
    time_hours = NA,
    estimate = NA,
    conf.low = NA,
    conf.high = NA
  )
  
  contr_ref <- contr_ref |>
    bind_rows(legend_df) |>
    mutate(contrast = str_remove(contrast, " - control")) |>
    mutate(
      contrast = factor(contrast,
                        levels = c(
                          "",
                          "placebo",
                          "active", 
                          "cold",
                          "heat",
                          "contrast",
                          "bfr",
                          "compression",
                          "massage"
                        ))
    ) |>
    arrange(contrast, time_hours)
  
  contr_ref <- contr_ref |>
    left_join(max_time, by = c("contrast" = "recovery_mode")) |>
    filter(is.na(max_time) | time_hours <= max_time)
  
  contr_plot <- ggplot(contr_ref, aes(time_hours, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.5, color = "black", size = 0.25
    ) +
    geom_line() +
    
    # Legend panel annotation
    geom_label(
      data = tibble(
        contrast = factor(
          "",
          levels = levels(contr_ref$contrast) 
        ),
        x = 60,
        y = 0,
        label = paste(
          "Each panel shows\ncondition vs control\n\n",
          "> 0 = better recovery than control\n",
          "< 0 = worse recovery than control\n"
        )
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 2.25,
      label.size = 2.5,
      label.padding = unit(2, "lines"),
      fill = "white",
      border.color = "white"
    ) +
    
    labs(
      x = "Time Post Exercise (hours)",
      y = "Difference in Fatigue (SMD)",
      title = "Contrasts between recovery modalities and controls"
    ) +
    facet_wrap(
      ~ contrast,
      ncol = 3,
      labeller = labeller(
        contrast = function(x) {
          x <- str_to_title(x)
          x[x == "Bfr"] <- "Blood Flow Restriction"
          x
        }
      )
    ) +  
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(limits = c(-0.75,0.75), breaks = seq(-0.75, 0.75, by = 0.25)) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  performance_plot <- (preds_plot + contr_plot) +
    plot_annotation(
      title =  "Performance Outcomes",
      caption = paste("Lines and ribbons are global grand mean estimates with 95% quantile intervals\n",
                      "Both predictions and contrasts are visualised only over time ranges supported by observed data for each modality")    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  return(performance_plot)
  
}

plot_biochemical_model <- function(biochemical_model) {
  
  max_time <- biochemical_model$data |>
    group_by(recovery_mode) |>
    summarise(max_time = max(time_hours, na.rm = TRUE))
  
  pred_grid <- max_time |>
    rowwise() |>
    mutate(
      time_hours = list(seq(0, max_time, by = 4))
    ) |>
    unnest(time_hours) |>
    ungroup() |>
    mutate(vi = 0)
  
  preds <- predictions(biochemical_model, 
                       re_formula= NA,
                       newdata = pred_grid
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
  
  
  preds_plot <- preds |>
    ggplot(aes(x = time_hours, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.5, color = "black", size = 0.25) +
    geom_line(aes(y = estimate), color = "black") +
    labs(
      x = "Time Post Exercise (hours)",
      y = "Fatigue (SMD)",
      title = "Predicted fatigue/recovery",
    ) +
    guides(
      size = "none"
    ) +
    facet_wrap("recovery_mode",
               labeller = labeller(
                 recovery_mode = function(x) {
                   x <- str_to_title(x)
                   x[x == "Bfr"] <- "Blood Flow Restriction"
                   x
                 }
               )) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(limits = c(-5.5,5.5), breaks = seq(-5, 5, by = 1)) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  contr_ref <- comparisons(
    biochemical_model,
    re_formula = NA,
    newdata = datagrid(
      recovery_mode = levels(biochemical_model$data$recovery_mode),
      time_hours = seq(0,120, by = 4),
      vi = 0
    ),
    variables = list(recovery_mode = "reference"))
  
  legend_df <- tibble(
    contrast = "",
    time_hours = NA,
    estimate = NA,
    conf.low = NA,
    conf.high = NA
  )
  
  contr_ref <- contr_ref |>
    bind_rows(legend_df) |>
    mutate(contrast = str_remove(contrast, " - control")) |>
    mutate(
      contrast = factor(contrast,
                        levels = c(
                          "",
                          "placebo",
                          "active", 
                          "cold",
                          "heat",
                          "contrast",
                          "bfr",
                          "compression",
                          "massage"
                        ))
    ) |>
    arrange(contrast, time_hours)
  
  max_time <- biochemical_model$data |>
    group_by(recovery_mode) |>
    summarise(max_time = max(time_hours, na.rm = TRUE))
  
  contr_ref <- contr_ref |>
    left_join(max_time, by = c("contrast" = "recovery_mode")) |>
    filter(is.na(max_time) | time_hours <= max_time)
  
  contr_plot <- ggplot(contr_ref, aes(time_hours, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.5, color = "black", size = 0.25
    ) +
    geom_line() +
    
    # Legend panel annotation
    geom_label(
      data = tibble(
        contrast = factor(
          "",
          levels = levels(contr_ref$contrast) 
        ),
        x = 60,
        y = -0.5,
        label = paste(
          "Each panel shows\ncondition vs control\n\n",
          "> 0 = better recovery than control\n",
          "< 0 = worse recovery than control\n"
        )
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 2.25,
      label.size = 2.5,
      label.padding = unit(2, "lines"),
      fill = "white",
      border.color = "white"
    ) +
    
    labs(
      x = "Time Post Exercise (hours)",
      y = "Difference in Fatigue (SMD)",
      title = "Contrasts between recovery modalities and controls"
    ) +
    facet_wrap(
      ~ contrast,
      ncol = 3,
      labeller = labeller(
        contrast = function(x) {
          x <- str_to_title(x)
          x[x == "Bfr"] <- "Blood Flow Restriction"
          x
        }
      )
    ) +  
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  biochemical_plot <- (preds_plot + contr_plot) +
    plot_annotation(
      title =  "Biochemical Outcomes",
      caption = paste("Lines and ribbons are global grand mean estimates with 95% quantile intervals\n",
                      "Both predictions and contrasts are visualised only over time ranges supported by observed data for each modality")
      ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  return(biochemical_plot)
  
}

plot_perceptual_model <- function(perceptual_model) {

  max_time <- perceptual_model$data |>
    group_by(recovery_mode) |>
    summarise(max_time = max(time_hours, na.rm = TRUE))
  
  pred_grid <- max_time |>
    rowwise() |>
    mutate(
      time_hours = list(seq(0, max_time, by = 4))
    ) |>
    unnest(time_hours) |>
    ungroup() |>
    mutate(vi = 0)
  
  preds <- predictions(perceptual_model, 
                       re_formula= NA,
                       newdata = pred_grid
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
  
  
  preds_plot <- preds |>
    ggplot(aes(x = time_hours, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.5, color = "black", size = 0.25) +
    geom_line(aes(y = estimate), color = "black") +
    labs(
      x = "Time Post Exercise (hours)",
      y = "Fatigue (SMD)",
      title = "Predicted fatigue/recovery",
    ) +
    guides(
      size = "none"
    ) +
    facet_wrap("recovery_mode",
               labeller = labeller(
                 recovery_mode = function(x) {
                   x <- str_to_title(x)
                   x[x == "Bfr"] <- "Blood Flow Restriction"
                   x
                 }
               )) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(limits = c(-3.5,3.5), breaks = seq(-3, 3, by = 1)) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  contr_ref <- comparisons(
    perceptual_model,
    re_formula = NA,
    newdata = datagrid(
      recovery_mode = levels(perceptual_model$data$recovery_mode),
      time_hours = seq(0,120, by = 4),
      vi = 0
    ),
    variables = list(recovery_mode = "reference"))
  
  legend_df <- tibble(
    contrast = "",
    time_hours = NA,
    estimate = NA,
    conf.low = NA,
    conf.high = NA
  )
  
  contr_ref <- contr_ref |>
    bind_rows(legend_df) |>
    mutate(contrast = str_remove(contrast, " - control")) |>
    mutate(
      contrast = factor(contrast,
                        levels = c(
                          "",
                          "placebo",
                          "active", 
                          "cold",
                          "heat",
                          "contrast",
                          "bfr",
                          "compression",
                          "massage"
                        ))
    ) |>
    arrange(contrast, time_hours)
  
  max_time <- perceptual_model$data |>
    group_by(recovery_mode) |>
    summarise(max_time = max(time_hours, na.rm = TRUE))
  
  contr_ref <- contr_ref |>
    left_join(max_time, by = c("contrast" = "recovery_mode")) |>
    filter(is.na(max_time) | time_hours <= max_time)
  
  contr_plot <- ggplot(contr_ref, aes(time_hours, estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = 0.5, color = "black", size = 0.25
    ) +
    geom_line() +
    
    # Legend panel annotation
    geom_label(
      data = tibble(
        contrast = factor(
          "",
          levels = levels(contr_ref$contrast) 
        ),
        x = 60,
        y = -0.5,
        label = paste(
          "Each panel shows\ncondition vs control\n\n",
          "> 0 = better recovery than control\n",
          "< 0 = worse recovery than control\n"
        )
      ),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      size = 2.25,
      label.size = 2.5,
      label.padding = unit(2, "lines"),
      fill = "white",
      border.color = "white"
    ) +
    
    labs(
      x = "Time Post Exercise (hours)",
      y = "Difference in Fatigue (SMD)",
      title = "Contrasts between recovery modalities and controls"
    ) +
    facet_wrap(
      ~ contrast,
      ncol = 3,
      labeller = labeller(
        contrast = function(x) {
          x <- str_to_title(x)
          x[x == "Bfr"] <- "Blood Flow Restriction"
          x
        }
      )
    ) +  
    theme_classic() +
    theme(
      plot.title = element_text(size = 12),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 6)
    ) +
    scale_y_continuous(limits = c(-1.75,1.75), breaks = seq(-1.5, 1.5, by = 0.5)) +
    scale_x_continuous(limits = c(0,120), breaks = seq(0,120, by = 24))
  
  
  perceptual_plot <- (preds_plot + contr_plot) +
    plot_annotation(
      title =  "Perceptual Outcomes",
      caption = paste("Lines and ribbons are global grand mean estimates with 95% quantile intervals\n",
                      "Both predictions and contrasts are visualised only over time ranges supported by observed data for each modality\n",
                      "Note, heat conditions not modelled due to only a single study reporting and providing insufficient data for effect size calculation")
    ) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")
  
  return(perceptual_plot)

}


