T_ref <- 20  # Reference temperature (°C)
Q10 <- 2.5   # Q10 coefficient

data$metabolic_rate_corrected_Q10 <- data$mr_ul_h_22 * Q10^(-(data$t_m) / 10)

# Function to apply Q10 correction
q10_correction <- function(mr_ul_h_22, t_m, T_ref, Q10) {
  R_ref <- mr_ul_h_22 * (Q10 ^ ((T_ref - t_m) / 10))
  return(R_ref)
}

# Example Data: Observed metabolic rates and temperatures
data <- data.frame(
  Sample_ID = c(1, 2, 3, 4, 5),
  R_obs = c(0.5, 0.8, 1.2, 1.5, 2.0),  # Observed metabolic rates (e.g., in mL O2/hr)
  T_obs = c(15, 18, 20, 22, 25)       # Observed temperatures (°C)
)

# Set reference temperature (T_ref) and Q10 value
T_ref <- 20  # Reference temperature in °C
Q10 <- 2.5   # Assumed Q10 value (can be adjusted based on species)

# Apply Q10 correction to each metabolic rate in the dataset
dataset_updated$R_corrected <- mapply(q10_correction, dataset_updated$mr_ul_h_22, dataset_updated$t_m, MoreArgs = list(T_ref = T_ref, Q10 = Q10))

# Print results
print(data)

# Optional: Visualize the corrected vs. observed metabolic rates
library(ggplot2)

ggplot(data, aes(x = T_obs, y = R_corrected)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red") +
  labs(title = "Q10-Corrected Metabolic Rate vs. Observed Temperature",
       x = "Observed Temperature (°C)",
       y = "Metabolic Rate at Reference Temperature") +
  theme_minimal()