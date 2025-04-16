# ============================
# R Script for Biphasic Scaling Analysis
# ============================

# ---- Step 1: Load Required Libraries ----
install.packages(c("ggplot2", "segmented", "ggthemes", "dplyr"))  # Install if not already installed
library(ggplot2)
library(segmented)
library(ggthemes)
library(dplyr)

# ---- Step 2: Load and Check Data ----
# Assume 'dat' is the dataset with variables log_mass (log body size) and log_mr (log metabolic rate)
# Ensure data is formatted correctly
str(dat)    # Check structure
summary(dat) # Check for missing values and data ranges

# Remove NA values (if present)
dat <- na.omit(dat)

# Ensure numeric data types
dat$log_mass <- as.numeric(dat$log_mass)
dat$log_mr <- as.numeric(dat$log_mr)

# ---- Step 3: Fit Initial Linear Model ----
lm_model <- lm(log_mr ~ log_mass, data = dat)

# Check model summary
summary(lm_model)

# ---- Step 4: Apply Segmented Regression ----
# Initial breakpoint guess based on median log_mass
initial_guess <- median(dat$log_mass, na.rm = TRUE)

# Try fitting segmented regression to detect a biphasic pattern
seg_model <- tryCatch(
  segmented(lm_model, seg.Z = ~log_mass, psi = list(log_mass = initial_guess)),
  error = function(e) NULL
)

# ---- Step 5: Validate Model Fit ----
if (!is.null(seg_model)) {
  
  # Extract breakpoints
  breakpoints <- seg_model$psi[, "Est."]
  cat("Detected Breakpoint at:", breakpoints, "\n")
  
  # Get segmented regression predictions
  dat$predicted <- predict(seg_model)
  
  # ---- Step 6: Visualize Results ----
  ggplot(dat, aes(x = log_mass, y = log_mr, color = mr_type)) +
    geom_point(alpha = 0.7) +  # Scatter plot of raw data
    geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, color = "black") +  # Linear fit
    geom_vline(xintercept = breakpoints, linetype = "dashed", color = "red", size = 1.2) +  # Breakpoint line
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(-1, 6)) +
    labs(
      y = "log oxygen consumption",
      x = "log body size",
      title = "Biphasic Scaling Analysis"
    ) +
    theme(
      plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
      text = element_text(size = 12, family = "Tahoma"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 11),
      legend.position = "top"
    ) +
    scale_color_manual(values = c("#669933", "#CC9900", "black")) +
    theme_few()
  
} else {
  
  # If no breakpoint found, print message and plot a simple linear model
  cat("No significant breakpoint detected. The data does not exhibit a biphasic pattern.\n")
  
  ggplot(dat, aes(x = log_mass, y = log_mr, color = mr_type)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_continuous(limits = c(0, 5)) +
    scale_y_continuous(limits = c(-1, 6)) +
    labs(
      y = "log oxygen consumption",
      x = "log body size",
      title = "No Biphasic Pattern Detected"
    ) +
    theme(
      plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
      text = element_text(size = 12, family = "Tahoma"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(size = 11),
      legend.position = "top"
    ) +
    scale_color_manual(values = c("#669933", "#CC9900", "black")) +
    theme_few()
}

#Check if segmented fails
summary(seg_model)
