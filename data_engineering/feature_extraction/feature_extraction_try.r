# Trait data extraction from TRY database
# Alanis Rosa-Santiago
# Laura Ospina          - lauosgom@gmail.com
# February 2022

library(tidyverse)

#.csv where the data is stored (qualitative data)
# change to the csv you downloaded from TRY
TRY <- read.csv("TRY.csv", header = TRUE)

# Check available traits
traits_available <- levels(as.factor(TRY$TraitName))

# Create a vector of plant species names
species_list <- read.csv()
# select the column where the names are
my_species   <- species_list$species
# you can build a vector with names separated by , and in ""
my_species   <- c()

# Create a vector with the trait names
# TRY can look for different traits at the same time
traits_quali <- c("Plant growth form",
                  "Plant resprouting capacity",
                  "Plant vegetative reproduction: clonality of ramets",
                  "Species tolerance to shade") 

#function for multiple species and multiple traits
traits_loop <- function(species, trait){
  results <- data.frame(Value=character(),
                        Species=character(),
                        Trait=character(),
                        Level=character())
  
  for (i in seq_along(species)) {
    for (j in seq_along(trait)) {
      TRY_species <- TRY %>%
        filter(TraitName == trait) %>%
        group_by(AccSpeciesName) %>%
        summarise(Frequency = n())
      
      if (species[i] %in% TRY_species$AccSpeciesName) {
        data <- TRY %>%
          filter(AccSpeciesName == species[i] & TraitNam == trait[j]) %>%
          group_by(OrigValueStr) %>%
          summarise(Frequency = n()) %>%
          filter(Frequency == max(Frequency)) %>%
          mutate(Species = species[i], Trait = trait[j], Level = "species")
        
        results <- rbind(results, data)
      
      } else if (word(species[i], 1) %in% TRY$genus) {
        data <- TRY %>%
          filter(genus == word(species[i], 1) & TraitName == trait[j]) %>%
          group_by(OrigValueStr) %>%
          summarise(Frequency = n()) %>%
          filter(Frequency == max(Frequency))%>%
          mutate(Species = species[i], Trait = trait[j], Level = "genus")
        
        results <- rbind(results, data)
        
      } else {
        print("Not found")
        print(species[i])
      }
    }
  }
  return(results)
}

# Trait extraction
try_quali <- traits_loop(species = my_species, traits_quali)

# View the results of extraction
View(try_quali)

## TRY continuos data

#.csv where the quantitative variables are stored
qualitative <- read_csv("TRY_quantitative.csv")

qualitative$genus <- word(qualitative$AccSpeciesName, 1)

# Create a vector of plant species names
species_list <- read.csv()
my_species   <- species_list$species
# you can build a vector with names separated by , and in ""
my_species   <- c("Aiouea areolata")

# Create a vector with the trait names
try_quant<- c("Stem specific density (SSD) or wood density (stem dry mass per stem fresh volume)",  # nolint
              "Plant height vegetative")

traits_loop <- function(species, trait) {
  results <- data.frame(Value = character(),
                      Species = character(),
                      Trait = character(),
                      Level = character())
  
  for (i in seq_along(species)) {
    for (j in seq_along(trait)) {
      if (species[i] %in% TRY_species$AccSpeciesName) {
        data <- qualitative %>%
          filter(AccSpeciesName == species[i] & TraitName == trait[j]) %>%
          summarise(Value = mean(OrigValueStr)) %>%
          mutate(Species = species[i], Trait=trait[j], Level = "species")
        
        results <- rbind(results,data)
        
      } else if (word(species[i], 1) %in% qualitative$genus) {
        data <- qualitative %>%
          filter(genus == word(species[i], 1) & TraitName == trait[j]) %>%
          summarise(Value = mean(OrigValueStr)) %>%
          mutate(Species = species[i], Trait = trait[j], Level = "genus")
        
        results <- rbind(results, data)
        
      } else {
        print("Not found")
        print(species[i])
      }
    }
  }
  return(results)
}

# Trait extraction
try_quant <- traits_loop(species = my_species, try_quant)

# View the results of extraction
View(try_quant)