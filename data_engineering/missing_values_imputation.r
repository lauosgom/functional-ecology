# Laura Ospina
# December 2021 - January 2022
# Chapter 1 - Part 1

# 1. Build pylogenetic tree
# 2. Eigenvalues
# 3. Imputation

# 1. Build the phylogenetic tree ####

# Import libraries. rtrees is the most important and can't be found in CRAN

devtools::install_github("daijiang/rtrees")
library(rtrees) # phylogeny
library(tidyverse) # data engineering
library(ape) # used by rtrees
library(DataCombine) # replace characters
library(mice) # imputation

# Import database
species_list <- data.frame(read.csv("species_traits_2022_04_02b.csv",
                            header = TRUE))

species_list_L <- species_list %>%
  replace(is.na(.), "") %>%
  select(species, genus, family, GROWTH)

View(species_list_L)
summary(species_list_L)

# Filter to remove Mosses and Lichen

species_list_L <- species_list_L %>%
  filter(GROWTH != "MOSS") %>%
  filter(GROWTH != "LICHEN")

# Build phylogenetic tree

tree_df <- get_one_tree(sp_list = species_list_L,
                        tree = tree_plant_otl,
                        taxon = "plant",
                        scenario = "random_below_basal",
                        show_grafted = T,
                        tree_by_user = FALSE)

# If scenario = "at_basal_node", a species is attached to the basal node of the same genus or the same family if the mega-tree does
# not have any species of this genus.

# If scenario = "random_below_basal", a species is attached to a randomly selected node that is at or below the basal node of the 
# same genus of the same family.

# If scenario = "at_or_above_basal", a species is attached to the basal node of the same genus

write.tree(tree_df, "Figure.tre")

# Plot tree

plot(as.phylo(tree_df),
     type = "fan",
     label.offset = 1.5,
     no.margin = TRUE,
     cex = 0.70,
     show.node.label = FALSE,
     show.tip.label = TRUE,
     tip.color = rep(c("black")), edge.color = "black")

# 2. Eigenvalues

# PCA with tree information

phylo.pca2 <- princomp(cophenetic(tree_df), cor = TRUE)
biplot(phylo.pca2)

# Dataframe for all eigenvectors

comp <- as.data.frame(phylo.pca2$scores)
comp <- cbind(species = rownames(comp), comp)
write.csv(comp, "pca_tree.csv")


# Dataframe of height, wood density and eigenvectors

# Select first 3 axes data frame

comp2 <- comp %>%
  select(species, Comp.1, Comp.2, Comp.3)

# Find and replace the _ for space

Replaces <- data.frame(from = c("_", "*"), to = c(" ", ""))

comp3 <- FindReplace(data = comp2, Var = "species", replaceData = Replaces,
                                from = "from", to = "to", exact = FALSE)

# Remove the * from the species name

comp4 <- comp3 %>%
  mutate(species = str_remove_all(species, "[*]"))

# Rename comp

eigen <- comp4

# Merge traits with comp

trait_eigenvector_df <- merge(species_list_L, eigen, by = "species")
View(trait_eigenvector_df)

species_list_tot <- species_list %>%
  select(species,
         RESPROUTING,
         SHADE,
         CLOLLITY,
         ROOT_ARCH,
         HEIGHT,
         WOOD,
         LEAF_AREA)

# Dataframe with eigenvalues, species and traits

trait_eigen_tot <- merge(trait_eigenvector_df, species_list_tot, by = "species")
View(trait_eigen_tot)

# To imput wood density, I will filter herbs and climbers and select only continuos traits

trait_eigen_tot_herb <- trait_eigen_tot %>%
  filter(GROWTH != "HERB") %>%
  filter(GROWTH != "CLIMBER") %>%
  select(species, Comp.1, Comp.2, Comp.3, HEIGHT, WOOD, LEAF_AREA)

#  3. Imputation

mice_blocks3 <- list("WOOD", "HEIGHT", "LEAF_AREA")

# First imputation to obtain predictor matrix using the Predictive mean matching method

imp3 <- mice(trait_eigen_tot_herb,
             maxit = 50,
             m = 5,
             blocks = mice_blocks3)

imp_pred3 <- imp3$predictorMatrix

imp_pred3[seq_lon(nrow(imp_pred3)), c(1, 6, 7)] <- 0

# Imputation with predictor matrix using the Predictive mean matching method

mice_imp3 <- mice(trait_eigen_tot_herb,
                  predictorMatrix = imp_pred3,
                  maxit = 50,
                  m = 5,
                  blocks = mice_blocks3)

mice_pmm <- complete(mice_imp3) %>%
  select(species,
         HEIGHT,
         WOOD,
         LEAF_AREA)

# For herbs and ferns, we need a tree with all species

species_list_L <- species_list %>%
  filter(GROWTH != "MOSS") %>%
  filter(GROWTH != "LICHEN")

# run the tree
# Then, we filter the woody plants to impute height

trait_eigen_tot_herb <- trait_eigen_tot %>%
  select(species, Comp.1, Comp.2, Comp.3, HEIGHT)

# First imputation to obtain predictor matrix using the Predictive mean matching method

mice_blocks3 <- list("HEIGHT")

imp_height <- mice(trait_eigen_tot_herb, maxit = 50, m=5, blocks = mice_blocks3)
imp_pred_height <- imp_height$predictorMatrix
imp_pred_height[seq_len(nrow(imp_pred_height)), c(1)] <- 0

# Non-woody imputation with predictor matrix using the Predictive mean matching method

mice_imp_h <- mice(trait_eigen_tot_herb,
                   predictorMatrix = imp_pred_height,
                   maxit = 50,
                   m = 5,
                   blocks = mice_blocks3)

mice_lr <- complete(mice_imp_h) %>%
  select(species, HEIGHT)

# Leaf area
# First imputation to obtain predictor matrix using the Predictive mean matching method

trait_eigen_tot_herb <- trait_eigen_tot %>%
  select(species,Comp.1, Comp.2, Comp.3, LEAF_AREA)

mice_blocks3 <- list("LEAF_AREA")

imp_leaf <- mice(trait_eigen_tot_herb, maxit = 50, m = 5, blocks = mice_blocks3)
imp_pred_leaf <- imp_leaf$predictorMatrix
imp_pred_leaf[seq_len(nrow(imp_pred_leaf)), c(1)] <- 0

# Non-woody imputation with predictor matrix using the Predictive mean matching method

mice_imp_leaf <- mice(trait_eigen_tot_herb,
                      predictorMatrix = imp_pred_leaf,
                      maxit = 50,
                      m = 5,
                      blocks = mice_blocks3)

mice_leaf <- complete(mice_imp_leaf) %>%
  select(species, LEAF_AREA)

View(trait_eigen_tot_herb)

# Resprouting

trait_eigen_tot_fact <- trait_eigen_tot %>%
  select(species, Comp.1, Comp.2, Comp.3, RESPROUTING, SHADE, ROOT_ARCH, CLOLLITY)
  
trait_eigen_tot_res <- trait_eigen_tot_fact %>%
  select(species, Comp.1, Comp.2, Comp.3, RESPROUTING) %>%
  mutate_all(na_if, "")

# Resprouting has to be a factor
trait_eigen_tot_res$RESPROUTING <- as.factor(trait_eigen_tot_res$RESPROUTING)

# Check resprouting is a factor and blanks are NA's
summary(trait_eigen_tot_res)

mice_blocks3 <- list("RESPROUTING")

# first imputation

imp34 <- mice(trait_eigen_tot_res,
              method = "logreg",
              maxit = 50,
              m=5,
              blocks = mice_blocks3)

imp_pred34 <- imp34$predictorMatrix
imp_pred34[, 1] <- 0
imp_pred34[1, ] <- 0

# imputation with predictor matrix

mice_imp_res <- mice(trait_eigen_tot_res,
                     method = "logreg",
                     predictorMatrix = imp_pred34,
                     maxit = 50,
                     m = 5, blocks = mice_blocks3)

mice_resp <- complete(mice_imp_res) %>%
  select(species, RESPROUTING)

# Shade tolerance

View(trait_eigen_tot_fact)

trait_eigen_tot_shade <- trait_eigen_tot_fact %>%
  select(species, Comp.1, Comp.2, Comp.3, SHADE) %>%
  mutate_all(na_if, "")

trait_eigen_tot_shade$SHADE <- as.factor(trait_eigen_tot_shade$SHADE)

mice_blocks3 <- list("SHADE")

imp_shade <- mice(trait_eigen_tot_shade, method = "polyreg", maxit = 50, m = 5)
imp_pred_shade <- imp_shade$predictorMatrix
imp_pred_shade[c(1:4),] <- 0
imp_pred_shade[5, c(2, 3, 4)] <- 1

mice_imp_shade <- mice(trait_eigen_tot_shade,
                       method = "polyreg",
                       predictorMatrix = imp_pred_shade,
                       maxit = 50,
                       m = 5)

mice_shade <- complete(mice_imp_shade) %>%
  select(species, SHADE)

# ROOT ARCHITECTURE

trait_eigen_tot_root <- trait_eigen_tot_fact %>%
  select(species, Comp.1, Comp.2, Comp.3, ROOT_ARCH) %>%
  mutate_all(na_if, "")

trait_eigen_tot_root$ROOT_ARCH <- as.factor(trait_eigen_tot_root$ROOT_ARCH)

mice_blocks3 <- list("ROOT_ARCH")

imp_root <- mice(trait_eigen_tot_root, method = "polyreg", maxit = 50, m = 5)
imp_pred_root <- imp_root$predictorMatrix
imp_pred_root[c(1:4), ] <- 0
imp_pred_root[, c(1, 5)] <- 0

mice_imp_root <- mice(trait_eigen_tot_root,
                      method = "polyreg",
                      predictorMatrix = imp_pred_root,
                      maxit = 50,
                      m= 5)

mice_root <- complete(mice_imp_root) %>%
  select(species, ROOT_ARCH)

# Clonallity

trait_eigen_tot_clon <- trait_eigen_tot_fact %>%
  select(species, Comp.1, Comp.2, Comp.3, CLOLLITY) %>%
  mutate_all(na_if, "")

trait_eigen_tot_clon$CLOLLITY <- as.factor(trait_eigen_tot_clon$CLOLLITY)

imp_clon <- mice(trait_eigen_tot_clon, method = "polyreg", maxit = 50, m = 5)
imp_pred_clon <- imp_clon$predictorMatrix
imp_pred_clon[c(1:4), ] <- 0
imp_pred_clon[, c(1, 5)] <- 0

mice_imp_clon <- mice(trait_eigen_tot_clon,
                      method = "polyreg",
                      predictorMatrix = imp_pred_clon,
                      maxit= 50,
                      m = 5)

mice_clon <- complete(mice_imp_clon) %>%
  select(species, CLOLLITY)


trait_eigen_tot_growth <- trait_eigen_tot %>%
  select(species, GROWTH)

summary(trait_eigen_tot_growth)

colnames(mice_pmm)
colnames(mice_lr) <- c("species", "HEIGHT_HERB")
# mice_resp
# mice_shade
# mice_root
# mice_clon

df_list <- list(mice_pmm,
                mice_lr,
                mice_resp,
                mice_shade,
                mice_root,
                mice_clon,
                mice_leaf,
                trait_eigen_tot_growth)

data_frame <- df_list %>%
  reduce(full_join, by = "species")

View(data_frame)

write.csv(data_frame, "trait_species_full_2022_05_16.csv")
