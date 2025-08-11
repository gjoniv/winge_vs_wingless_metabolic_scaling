# Load packages
library(ape)
library(caper)
library(dplyr)
library(brms)

# Prepare data
dat_combined <- dat %>%
  select(species, O2_c, weight_c, mr_type, wing_status) %>%
  tidyr::drop_na()
dat_combined$species <- as.factor(dat_combined$species)

# Generate phylogenetic tree (replace with read.tree if you have a real one)
species_levels <- levels(dat_combined$species)
my_tree <- rtree(n = length(species_levels), tip.label = species_levels)

# Covariance matrix from the tree
A <- ape::vcv.phylo(my_tree)
dat_combined$species <- factor(dat_combined$species, levels = rownames(A))

# Priors
priors <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0.75, 0.2), coef = "weight_c"),
  prior(normal(0, 1), class = "b")
)

# brms model with phylogenetic random intercept and slope
model_brm_phylo <- brm(
  O2_c ~ weight_c * mr_type * wing_status +
    (1 + weight_c | gr(species, cov = A)),
  data    = dat_combined,
  family  = gaussian(),
  data2   = list(A = A),
  prior   = priors,
  iter    = 4000,
  chains  = 4,
  cores   = 4,
  control = list(adapt_delta = 0.95, max_treedepth = 12)
)

# Output and checks
summary(model_brm_phylo)
pp_check(model_brm_phylo)

# Optional: PGLS for comparison
my_data_single <- dat %>%
  group_by(species) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
row.names(my_data_single) <- my_data_single$species
comp_data <- comparative.data(my_tree, my_data_single, names.col = "species", vcv = TRUE)
model_pgls <- pgls(log_mr ~ log_mass, data = comp_data, lambda = "ML")
summary(model_pgls)
