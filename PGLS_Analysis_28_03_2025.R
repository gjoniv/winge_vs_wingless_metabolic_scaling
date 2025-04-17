# Install required packages (only needs to be done once)
install.packages(c("ape", "caper"))

# Load necessary libraries
library(ape)     # For working with phylogenetic trees
library(caper)   # For comparative data analysis and PGLS
library(dplyr)   # For data manipulation

# Preview your dataset (assumes 'dat' is your main dataframe)
# Expected columns: species, log_mass, log_metabolic_rate (renamed below for consistency)
head(dat)

# Check that your dataset includes a column named 'species', and log-transformed body mass and metabolic rate
# If column names are different, rename them accordingly:
colnames(dat)[colnames(dat) == "log_metabolic_rate"] <- "log_mr"

# STEP 1: Generate or import a phylogenetic tree
# If you already have a tree, load it using read.tree("your_tree_file.nwk")
# Otherwise, generate a random ultrametric tree for demonstration (e.g., using rtree)

# Extract a list of unique species from the dataset
species <- unique(dat$species)

# Generate a random tree with equal branch lengths
my_tree <- rtree(n = length(species), tip.label = species)

# Visualize the tree
plot(my_tree)
title("Randomly Generated Phylogenetic Tree")

# STEP 2: Prepare species-level trait data
# Collapse the dataset so each species is represented by one row (e.g., the mean of replicate measurements)
my_data_single <- dat %>%
  group_by(species) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()

# Convert to standard data frame and assign species names as row names
my_data_df <- as.data.frame(my_data_single)
row.names(my_data_df) <- my_data_df$species

# STEP 3: Combine tree and data into a 'comparative.data' object
# This is needed to account for shared evolutionary history in the PGLS model
comp_data <- comparative.data(phy = my_tree, 
                              data = my_data_df, 
                              names.col = "species", 
                              vcv = TRUE, 
                              warn.dropped = TRUE)

# STEP 4: Fit a Phylogenetic Generalized Least Squares (PGLS) model
# This model accounts for potential phylogenetic signal using Pagel’s λ
model_pgls <- pgls(log_mr ~ log_mass, data = comp_data, lambda = "ML")

# STEP 5: View the model results
summary(model_pgls)