# Trait data extraction from TR8 database
# Alanis Rosa-Santiago
# Laura Ospina          - lauosgom@gmail.com
# February 2022

library(tidyverse)
library(TR8)

# Create a vector of plant species names
species_list <- read.csv()
# select the column where the names are
my_species   <- species_list$species
# you can build a vector with names separated by , and in ""
my_species   <- c()

# Check available traits
View(available_tr8)

# Create a vector with the trait names 
# TR8 can look for different traits at the same time
traits_quant <- c("h_max", "wood_dens", "le_area")
traits_quali <- c("Growth.Habit",
                  "growth_form",
                  "li_form",
                  "li_form_B",
                  "Resprout.Ability",
                  "Shade.Tolerance")

# Trait extraction
tr8_quant <- tr8(species_list = my_species,
                 download_list = traits_quant,
                 allow_persistent = TRUE)

tr8_quali <- tr8(species_list = my_species,
                 download_list = traits_quali,
                 allow_persistent = TRUE)

# View results of extraction
View(tr8_quant)
View(tr8_quali)

# Note that the class is a multidimentional object, we need to extract the information
class(tr8_quant)

# Extract values into a dataframe
tr8_quant_df <- extract_traits(tr8_quant)
tr8_quali_df <- extract_traits(tr8_quali)

# Optional - generate a .csv with the results
write.csv(tr8_quant_df, "TR8_quantitative_results.csv")
write.csv(tr8_quali_df, "TR8_qualitative_results.csv")