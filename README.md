# Load Required Libraries ------------------------------------------------
library(tidyverse)        # Includes dplyr, ggplot2, readr, tidyr, etc.
library(brms)             # For Bayesian regression modeling
library(tidybayes)        # For extracting and working with posterior draws
library(modelsummary)     # For model summaries
library(sjPlot)           # Plotting summaries
library(ggthemes)         # Extra ggplot themes
library(viridis)          # Color palettes
library(ggeffects)        # For effect plots
library(interactions)     # Interaction plotting
library(broom)            # For tidying models
library(lavaan)           # Structural equation modeling

# Set working directory --------------------------------------------------
setwd('~/Dropbox/insects/input/')

# Read and clean dataset -------------------------------------------------
dataset <- read.csv("your_dataset.csv")  # Replace with actual path

# Update wing status for specific species --------------------------------
species_to_update <- c("Anthia fabricii", "Calosoma affine", ...)

# Set wing status to 'NW' for selected species
updated_dataset <- dataset %>%
  mutate(
    wing_status = ifelse(species %in% species_to_update & wing_status == "W", "NW", wing_status),
    mr_type = ifelse(mr_type == "Calling", "Locomotion", mr_type)
  )

# Save updated dataset
write.csv(updated_dataset, "updated_dataset.csv", row.names = FALSE)

# Add log-transformed variables ------------------------------------------
dat <- updated_dataset %>%
  mutate(
    log_mass = log10(mass_mg_22 / 1000),
    log_mr = log10(R_corrected)
  ) %>%
  filter(!mr_type %in% c("Calling", "Web building", "Burrowing", "Copulation"))

# Create combined taxa and wing status variable --------------------------
dat$taxa_status <- paste(dat$taxa, dat$wing_status, sep = "")

# Subset data for different groups ---------------------------------------
dat_sp <- dat %>%
  filter(order == "Araneae", !order %in% c("Scorpiones", "Ixodida"))

dat_ins <- dat %>% filter(taxa == "Insect")

dat_insnw <- dat_ins %>% filter(wing_status == "NW")
dat_insw  <- dat_ins %>% filter(wing_status == "W")

# Standardize variables --------------------------------------------------
dat_combined <- dat_insw %>%
  mutate(
    weight_c = scale(log_mass, center = TRUE, scale = FALSE),
    O2_c = scale(log_mr, center = TRUE, scale = FALSE)
  )

# Fit Bayesian Model -----------------------------------------------------
model_brm_phylo <- brm(
  O2_c ~ weight_c * mr_type * taxa_status,
  data = dat_combined,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0.75, 0.2), coef = "weight_c"),
    prior(normal(0, 1), class = "b")
  )
)

# Posterior predictive check
pp_check(model_brm_phylo)

# Refit with random intercept for species --------------------------------
model_brm <- brm(
  O2_c ~ weight_c * mr_type * taxa_status + (1 | species),
  data = dat_combined,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(normal(0.75, 0.2), coef = "weight_c"),
    prior(normal(0, 1), class = "b")
  )
)

# Extract posteriors -----------------------------------------------------
model_posts <- as_draws_df(model_brm)

bodysize_sims <- tibble(weight_c = seq(min(model_brm$data$weight_c), max(model_brm$data$weight_c), length.out = 20))
mr_sims <- unique(model_brm$data$mr_type)
size_mr_sims <- bodysize_sims %>% expand_grid(mr_type = mr_sims)

# Calculate conditional slopes and intercepts -----------------------------
mr_conditional_posts <- size_mr_sims %>% add_epred_draws(model_brm)

# Compute slope table
mr_slopes <- tibble(weight_c = c(0, 0.0001)) %>%
  expand_grid(model_brm$data %>% distinct(mr_type)) %>%
  add_epred_draws(model_brm) %>%
  pivot_wider(names_from = weight_c, values_from = .epred) %>%
  mutate(slope = 1e4*(1e-04 - 0)) %>%
  group_by(mr_type) %>%
  mutate(model = "log_a ~ log_m*mr_type")

slope_table <- mr_slopes %>%
  group_by(mr_type) %>%
  median_qi(slope) %>%
  mutate(slope = paste0(round(slope, 2), " (", round(.lower, 2), " to ", round(.upper, 2), ")")) %>%
  select(mr_type, slope)

# Compute intercept table
mr_intercept <- tibble(weight_c = 0) %>%
  expand_grid(model_brm$data %>% distinct(mr_type)) %>%
  add_epred_draws(model_brm) %>%
  mutate(model = "log_a ~ log_m*mr_type")

intercept_table <- mr_intercept %>%
  group_by(model, mr_type) %>%
  median_qi(.epred) %>%
  mutate(intercept = paste0(round(.epred, 2), " (", round(.lower, 2), " to ", round(.upper, 2), ")")) %>%
  select(mr_type, intercept)

# Merge intercept and slope tables for plotting --------------------------
merged_data <- left_join(intercept_table, slope_table, by = "mr_type")

# Save slope and intercept tables
write.csv(merged_data, "slopes_intercepts_summary.csv", row.names = FALSE)

# Plotting examples ------------------------------------------------------
# Figure 1: log_mass vs log_mr
fig1 <- ggplot(dat_sp, aes(x = log_mass, y = log_mr, color = mr_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(-1, 6)) +
  theme_few() +
  labs(x = "log body size", y = "log oxygen consumption")

# Figure 2: log(L) vs b
ggplot(b_and_L, aes(x = log(L), y = b)) +
  geom_point() +
  xlab("log normalized metabolic level (L)") +
  ylab("metabolic scaling exponent (b)") +
  xlim(3, 12) +
  ylim(0.5, 1.1) +
  theme_few()

# Figure 3: Intercepts vs Slopes
ggplot(merged_data, aes(x = intercept, y = slope, color = mr_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(limits = c(-0.5, 1.5)) +
  scale_y_continuous(limits = c(0.6, 1.1)) +
  theme_few() +
  labs(x = "metabolic level (L)", y = "metabolic scaling exponent (b)")
