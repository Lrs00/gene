library(rstan)
library(dplyr)

df_stand <- read.csv("phase2_99_df2(5.18)_stand_newplate.csv")

## 20h_ML
light_regime_target <- "20h_ML"
timepoints <- paste0("y2_", 1:44)
all_plate_effects_ML7 <- data.frame()
options(warn = 1)
for (timepoint in timepoints) {
  
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_ML7 <- bind_rows(all_plate_effects_ML7, plate_effect_df)
  
}
all_plate_effects_ML7 
write.csv(all_plate_effects_ML7, "all_plate_effects_ML7(5.18).csv", row.names = FALSE)

## 20h_HL
light_regime_target <- "20h_HL"
timepoints <- paste0("y2_", 1:44)
all_plate_effects_HL7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_HL7 <- bind_rows(all_plate_effects_HL7, plate_effect_df)
  
  
}
all_plate_effects_HL7 
write.csv(all_plate_effects_HL7, "all_plate_effects_HL7(5.18).csv", row.names = FALSE)

## 2h-2h
light_regime_target <- "2h-2h"
timepoints <- paste0("y2_", 1:48)
all_plate_effects_2h_2h7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_2h_2h7 <- bind_rows(all_plate_effects_2h_2h7, plate_effect_df)
  
}
all_plate_effects_2h_2h7
write.csv(all_plate_effects_2h_2h7, "all_plate_effects_2h_2h7(5.18).csv", row.names = FALSE)

## 10min-10min

light_regime_target <- "10min-10min"
timepoints <- paste0("y2_", 1:84)
all_plate_effects_10min_10min7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_10min_10min7 <- bind_rows(all_plate_effects_10min_10min7, plate_effect_df)
  
}
all_plate_effects_10min_10min7
write.csv(all_plate_effects_10min_10min7, "all_plate_effects_10min_10min7(5.18).csv", row.names = FALSE)

## 5min-5min
light_regime_target <- "5min-5min"
timepoints <- paste0("y2_", 1:88)
all_plate_effects_5min_5min7 <- data.frame()
for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_5min_5min7 <- bind_rows(all_plate_effects_5min_5min7, plate_effect_df)
  
}
all_plate_effects_5min_5min7
write.csv(all_plate_effects_5min_5min7, "all_plate_effects_5min_5min7(5.18).csv", row.names = FALSE)

## 1min-5min
light_regime_target <- "1min-5min"
timepoints <- paste0("y2_", 1:88)
all_plate_effects_1min_5min7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_1min_5min7 <- bind_rows(all_plate_effects_1min_5min7, plate_effect_df)
  
}
all_plate_effects_1min_5min7
write.csv(all_plate_effects_1min_5min7, "all_plate_effects_1min_5min7(5.18).csv", row.names = FALSE)


## 1min-1min
light_regime_target <- "1min-1min"
timepoints <- paste0("y2_", 1:88)
all_plate_effects_1min_1min7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_1min_1min7 <- bind_rows(all_plate_effects_1min_1min7, plate_effect_df)
  
}
all_plate_effects_1min_1min7
write.csv(all_plate_effects_1min_1min7, "all_plate_effects_1min_1min7(5.18).csv", row.names = FALSE)



## 30s-30s

light_regime_target <- "30s-30s"
timepoints <- paste0("y2_", 1:88)
all_plate_effects_30s_30s7 <- data.frame()

for (timepoint in timepoints) {
  print(paste("Time step:", timepoint))
  
  # Filter data
  sub_df <- df_stand  %>%
    filter(light_regime == light_regime_target) %>%
    filter(!is.na(!!sym(timepoint)), !is.na(mutant_ID), !is.na(plate))
  
  if (nrow(sub_df) == 0) next
  
  # Factor encoding
  sub_df$mutant_factor <- as.integer(factor(sub_df$mutant_ID))
  sub_df$plate_factor <- as.integer(factor(sub_df$plate))
  plate_levels <- levels(factor(sub_df$plate))
  
  # Prepare Stan data
  stan_data <- list(
    N = nrow(sub_df),
    J = length(unique(sub_df$mutant_factor)),
    I = length(unique(sub_df$plate_factor)),
    mutant = sub_df$mutant_factor,
    plate = sub_df$plate_factor,
    
    y = sub_df[[timepoint]]
  )
  
  fit <- stan(
    file = "mutant_plate_model2.stan",
    data = stan_data,
    chains = 4,
    iter = 2000,
    seed = 105,
    refresh = 0
  )
  
  
  # Compute full summary (to get Rhat)
  full_summary <- summary(fit)$summary
  gamma_rhat <- full_summary[grep("^gamma\\[", rownames(full_summary)), "Rhat"]
  max_rhat <- max(full_summary[, "Rhat"], na.rm = TRUE)
  
  
  # Extract plate effect (gamma)
  samples <- rstan::extract(fit)
  plate_means <- colMeans(samples$gamma)
  
  # Create plate effect dataframe
  plate_effect_df <- data.frame(
    plate = plate_levels,
    plate_effect = plate_means,
    Rhat = gamma_rhat
  ) %>%
    mutate(
      light_regime = light_regime_target,
      y2_timepoint = timepoint,
      max_Rhat_in_model = max_rhat
    ) %>%
    select(light_regime, plate, y2_timepoint, plate_effect, Rhat, max_Rhat_in_model)
  
  all_plate_effects_30s_30s7 <- bind_rows(all_plate_effects_30s_30s7, plate_effect_df)
  
}
all_plate_effects_30s_30s7
write.csv(all_plate_effects_30s_30s7, "all_plate_effects_30s_30s7(5.18).csv", row.names = FALSE)


