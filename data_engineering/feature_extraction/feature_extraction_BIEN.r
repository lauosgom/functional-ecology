# Trait data extraction from BIEN database
# Alanis Rosa-Santiago
# Laura Ospina          - lauosgom@gmail.com
# February 2022


install.packages("https://cran.r-project.org/src/contrib/Archive/BIEN/BIEN_1.2.4.tar.gz", repos = NULL, type="source")# nolint
install.packages("rgeos")
library(rgeos)
library(BIEN)

# Check available traits
View(BIEN_trait_list())

# BIEN continuos data

# Create a vector of plant species names
species_list <- read.csv()

# Or select the column where the names are
my_species   <- species_list$species

# Or you can build a vector with names separated by , and in ""
my_species   <- c("genus species")

# Create a vector with the trait name
trait_wood    <- "stem wood density"
trait_height  <- "whole plant height"
trait_height2 <- "maximum whole plant height"
trait_leaf    <- "leaf area"

# Trait extraction
bien_wood    <- BIEN_trait_mean(species= my_species, trait = trait_wood)
bien_height  <- BIEN_trait_mean(species= my_species, trait = trait_height)
bien_height2 <- BIEN_trait_mean(species= my_species, trait = trait_height2)
bien_leaf    <- BIEN_trait_mean(species= my_species, trait = trait_leaf)

# View results of extraction
View(bien_trait)

# Optional - generate a .csv with the results
write.csv(bien_trait, "BIEN_stem_wood_results.csv")

# BIEN categorical data
# Provided by Dr. Brody Sandel - bsandel@scu.edu

# Create a vector of plant species names
splist <- c()
splist <- my_species
splist <- species

# Create an empty dataframe for the results.
growth_form <- rep(NA, length(splist))
growth_form_source <- rep(NA, length(splist))
growth_form_name <- rep(NA, length(splist))

#Where did the growth form come from? Species level data with complete agreement (code 1)
#species level data where the majority form was taken (2), genus level with complete agreement (3)
#or genus levele where the majority form was taken (4)

for (i in seq_along(splist)) {
  sp_level_data <- BIEN_trait_traitbyspecies(splist[i], "whole plant growth form") # nolint
  #Make it all upper-case because some have mixed case, some don't
  tab <- table(toupper(sp_level_data$trait_value))
  #If there is exactly one growth form listed for the species
  if(length(tab) == 1) {growth_form[i] = names(tab); growth_form_source[i] = 1; growth_form_name[i] = splist[i]} # nolint
  #If there is >1 growth form listed for the species
  if(length(tab) > 1)
  {
    index <- which.max(tab)
    growth_form[i] <- names(tab[index])
    growth_form_source[i] <- 2
    growth_form_name[i] <- splist[i]
  }
  #If there is no species data, look at the genus level
  if (length(tab) == 0) {
    genus_level_data = BIEN_trait_traitbygenus(strsplit(spList[i]," ")[[1]][1], "whole plant growth form") # nolint
    #Make it all upper-case because some have mixed case, some don't
    tab <- table(toupper(genus_level_data$trait_value))
    #If there is exactly one growth form listed for the genus
    if (length(tab) == 1) {
      growth_form[i] = names(tab); growth_form_source[i] = 3; growth_form_name[i] = spList[i]} # nolint
    #If there is >1 growth form listed for the species
    if (length(tab) > 1) {
      index <- which.max(tab)
      growth_form[i] <- names(tab[index])
      growth_form_source[i] <- 4
      growth_form_name[i] <- spList[i]
    }
  }
}

# View results of extraction
View(growth_form)

# Create a data frame with results
bien_growth <- data.frame(species = growth_form_name, GrowthForm = growth_form, GrowthFormSource = growth_form_source) # nolint
View(bien_growth)
# Optional - generate a .csv with the results
write.csv(bien_growth, "BIEN_growth_form_results.csv")