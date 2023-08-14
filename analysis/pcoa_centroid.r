#Chapter 1 PcOA analysis
#May 13 - L Ospina
#lauosgom@gmail.com

install.packages("remotes")
remotes::install_github("freysimon/TigR")
install.packages("stringr")
library(DataCombine)
library(splancs)
library(TigR)
library(tidyverse)
library(vegan)
library(ape)

#data<- read.csv("Vegetation_Landslides_N2_2022_05_23.csv",header = T, stringsAsFactors = F)
data<- read_csv("Vegetation_Landslides_N2_2022_08_9.csv")
replace<- read_csv("replacements_species2.csv")

data<-as.data.frame(data)
Replaces <- data.frame(from = replace$antes, to = replace$despues)
data <- FindReplace(data = data, Var = "Genus_species", replaceData = Replaces,
                    from = "from", to = "to", exact = TRUE)

#data for all the analysis
data_analysis <- data %>%
  filter(analysis == "yes" & Family != "(blank)") %>%
  filter(Family != "(blank)") %>%
  filter(Family != "Muerto") %>%
  filter(!grepl("Inde", Family)) %>%
  filter(!grepl("Unkn", Family)) %>%
  filter(!grepl("Brachytheciaceae", Family)) %>%
  filter(!grepl("Bryaceae", Family)) %>%
  filter(!grepl("Cladoniaceae", Family)) %>%
  filter(!grepl("Dicranaceae", Family)) %>%
  filter(!grepl("Grimmiaceae", Family)) %>%
  filter(!grepl("Hypnaceae", Family)) %>%
  filter(!grepl("Lembophyllaceae", Family)) %>%
  filter(!grepl("Lobariaceae", Family)) %>%
  filter(!grepl("Lunulariaceae", Family)) %>%
  filter(!grepl("Marchantiaceae", Family)) %>%
  filter(!grepl("Meteoriaceae", Family)) %>%
  filter(!grepl("Mniaceae", Family)) %>%
  filter(!grepl("Parmeliaceae", Family)) %>%
  filter(!grepl("Rhacocarpaceae", Family)) %>%
  filter(!grepl("Sphagnaceae", Family)) %>%
  filter(!grepl("Stereocaulaceae", Family)) %>%
  filter(!grepl("Thuidiaceae", Family)) %>%
  select(latitud_corregida, longitud_corregida, Pais, time, Habitat, altitude,
         Family, Genus, Species, Genus_species, p_min, Reference_code, analysis,
         code, trajectories, riqueza, betweensite, withinsite)


#matrix sites x plant families
data_family <- data_analysis %>%
  group_by(Family, code, Pais, Habitat, time) %>%
  summarise(min_pre = max(p_min)) %>%
  pivot_wider(names_from = "Family", values_from = "min_pre") %>%
  replace(is.na(.), 0)

chrono_codes<- read.csv("chrono_codes.csv",header = T)
data_family<-merge(data_family, chrono_codes[, c(1, 6, 7)], by= "code", all.x = TRUE)

colnames(data_family)
# Aggregations-> go to script 4 and run
source("analysis/4_chapter_1_aggregation.R")

data_family<-data_family[,-c(240:241)]
#data_family<-data_family[,-c(216:217)]

#sites information - for filtering on each analysis
data_sitios <- data_analysis %>%
  group_by(code, trajectories, riqueza, betweensite, withinsite) %>%
  summarise(min_pre = n())
  
colnames(data_sitios)<-c("code", "trajectories", "riqueza", "betweensite", "withinsite", "count")

# sites information - habitat, country  
sites_info<-data_family[, c(2:4)]
sites_info$code<-data_family$code
data_family<-data_family[, -c(1:4)]

# create the matrix for the PCoA
data_family <- as.matrix(data_family, row.names = sites_info$code)
rownames(data_family) <- sites_info$code

class(data_family) <- "numeric"

# delete the columns (families) that are not present on any site
data_trim<-data_family[,which(!colSums(data_family) == 0)]
spe<-data_trim

# PCoA Jaccard distance using cmdscale in vegan
spe.jac<-vegdist(spe, method = "jaccard")
spe.b.pcoa<-cmdscale(spe.jac, k=(nrow(spe)-1), eig=T)

pca<-pcoa(D=spe.jac, correction="cailliez", rn=NULL)

pca$Broken_stick

# plot ordination - raw PCoA
ordiplot(scores(spe.b.pcoa) [,c(1,2)], type="t", main="PCoA with species")
biplot(pca, Y=NULL, plot.axes = c(1,2), dir.axis1=-1, dir.axis2 = -1,
        rn=NULL, main=NULL)


spe.b.pcoa$species<- wascores(spe.b.pcoa$points, spe, expand = TRUE)

pl <- ordiplot(spe.b.pcoa, type = "none")
points(pl, "sites", pch=21, col="red", bg="yellow")
text(pl, "species", col="blue", cex=0.9)

spe.wa<- wascores(spe.b.pcoa$points,spe)
text(spe.wa, rownames(spe.wa),cex = 0.7, col="red")

write.csv(spe.wa,"scores_family.csv")

# extracting points and vector coordinates for visualization
data <- scores(spe.b.pcoa) [,c(1,2)]
data2<- as.data.frame(data)
data2$code<-sites_info$code

# add info sites
data_family_sites<-merge(data2,sites_info,by="code", all.x = TRUE)
data_family_sites<-merge(data_family_sites,data_sitios,by="code", all.x = TRUE)

#write.csv(pca$vectors.cor, "data_family_sites_2022_08_5.csv")

# plot all studies with trajectories in color and convex hull
convex_hull <- data_family_sites %>% 
  filter(trajectories == 'yes') %>%
  slice(chull(Dim1, Dim2))

ggplot(data_family_sites, aes(x = Dim1, y = Dim2, colour = trajectories)) +
  geom_point(size = 2)+
  geom_polygon(data = convex_hull,
               alpha = 0.25)+
  xlab("A1")+ylab("A2")+
  ylim(-0.65,0.65)+xlim(-0.5,0.5)+ 
  scale_color_manual(name="Trajectories" ,values = c("no"="lightgray", "yes"="navy"))+
  theme_linedraw()


# analysyis 1: chronosequences

data_chrono <- data_family_sites %>%
  filter(trajectories=="yes") %>%
  filter(Habitat != "Guatal")

data_chrono<-merge(data_chrono, chrono_codes, by="code",all.x = TRUE)

data_chrono[data_chrono$code == "00089-landslide-QuintIlha35-468",12] <-"Landslide"
data_chrono[data_chrono$code == "00089-landslide-QuintIlha35-468",13] <-"Brazil"
data_chrono[data_chrono$code == "00089-landslide-QuintIlha35-468",16] <-89
data_chrono[data_chrono$code == "00089-landslide-QuintIlha35-468",17] <-89
data_chrono[data_chrono$code == "00089-landslide-QuintIlha35-468",18] <-"NEO"

# plot all studies by habitat

ggplot(data_family_sites, aes(x = Dim1, y = Dim2, colour = trajectories)) +
  geom_point(size = 2)+
  geom_point(data = data_chrono, aes(x = Dim1, y = Dim2, colour = Habitat.x))+
  xlab("A1") + ylab("A2")+
  ylim(-0.65,0.65) + xlim(-0.5,0.5)+ 
  scale_color_manual(name="Habitat" ,values = c("Forest"="darkgreen", "Landslide"="darkgoldenrod3", "no"="lightgray"))+
  #scale_color_manual(name="Trajectories" ,values = c("yes"="white", "no"="lightgray"))+
  theme_linedraw()

convex_hull <- data_chrono %>% slice(chull(Dim1, Dim2))

plot<-ggplot(data_chrono, aes(x = Dim1, y = Dim2)) + 
  geom_point() + 
  geom_polygon(data = convex_hull,
               alpha = 0.25)+
  ylim(-0.65,0.65)+xlim(-0.5,0.5)+
  ggtitle(paste("PCoA with plant families"))+
  theme_classic()

plot
#calculate area of convex hull
area<-areapl(as.matrix(convex_hull[,c(2,3)]))

# color by zone
ggplot() +
  geom_point(data=data_family_sites, aes(x = Dim1, y = Dim2), colour="lightgray")+
  geom_point(data=data_chrono, aes(x = Dim1, y = Dim2, colour = bio_zone))+
  xlab("A1")+ylab("A2")+
  ylim(-0.65,0.65)+xlim(-0.5,0.5)+ 
  scale_color_manual(name="Bio zone" ,values = c("AFR"="orange", "AUS"="lightgreen", "IND"="darkred",
                                                "NEA"="lightblue", "NEO"="navy", "OCE"="purple","PAL"="darkgoldenrod3" ))+
  stat_ellipse(data=data_chrono, aes(x = Dim1, y = Dim2, colour = bio_zone))+
  theme_linedraw()

# deal with forest ages from 0 to 6000

Replaces <- data.frame(from = "0", to = "6000")

data_chrono <- FindReplace(data = data_chrono, Var = "time.x", replaceData = Replaces,
                     from = "from", to = "to", exact = TRUE)

#manual changes

data_chrono[data_chrono$code == "00048-0",6]     <-0
data_chrono[data_chrono$code == "00058-L3",6]    <-0
data_chrono[data_chrono$code == "00058-LF2-F",5] <-"Forest"

#class(data_chrono$time.x)
data_chrono$time.x<-as.numeric(data_chrono$time.x)

# plot with arrows

for (i in levels(as.factor(data_chrono$crono_code))){
  data <- data_chrono %>%
    filter(crono_code == i)
  
  plot<-ggplot(data[order(data$time.x),], aes(x = Dim1, y = Dim2, colour = Habitat.x)) +
    geom_segment(aes(xend = c(tail(Dim1, n = -1), NA), 
                     yend = c(tail(Dim2, n = -1), NA)),
                 arrow = arrow(length = unit(0.4, "cm")),
                 color = "gray") +
    geom_point(size = 2)+
    ggtitle(paste("PCoA with plant families",i))+
    xlab("A1")+ylab("A2")+
    ylim(-0.65,0.65)+xlim(-0.5,0.5)+
    scale_color_manual(name="Habitat" ,values = c("Forest"="darkgreen", "Landslide"="darkgoldenrod3"))+
    theme_linedraw()
  
  print(plot)
}


# convex hull and area
convex_hull_data <- tibble()
area_bd <- tibble()

for (i in levels(as.factor(data_chrono$crono_code))){
  data <- data_chrono %>%
    filter(crono_code == i)
  
  convex_hull <- data %>% slice(chull(Dim1, Dim2))
  
  plot<-ggplot(data) + 
    geom_point(aes(x = Dim1, y = Dim2, colour = Habitat.x)) + 
    geom_polygon(data = convex_hull, aes(x = Dim1, y = Dim2),
                 alpha = 0.25)+
    ylim(-0.65,0.65) + xlim(-0.5, 0.5)+
    ggtitle(paste("PCoA with plant families", i))+
    xlab("A1")+ylab("A2")+
    scale_color_manual(name="Habitat" ,values = c("Forest"="black", "Landslide"="black"))+
    theme_linedraw()
  
  area<-areapl(as.matrix(convex_hull[,c(2,3)]))
  convex_hull_data<-rbind(convex_hull_data,area)
  
  print(plot)
  print(paste("Chronosequence:", i, "area convex hull:", area))
  area_bd<- rbind(area_bd,area)
}

write.csv(convex_hull_data, 'convex_hull_area.csv')

# Dispersion - centroid
data_centro_cu<- data_chrono %>%
  filter(Habitat.x != "Forest") %>%
  group_by(crono_code,time.x) %>%
  summarise(dim1_cen=mean(Dim1), dim2_cen=mean(Dim2), count=n())

data_centroide<-data_chrono %>%
  filter(Habitat.x != "Forest") %>%
  group_by(crono_code) %>%
  summarise(dim1_centro=mean(Dim1), dim2_centro=mean(Dim2), count=n())

data_centro_calc <- merge(data_centro_cu, data_centroide, by="crono_code",all.x = TRUE)

X2<-data_centro_calc$dim1_centro
Y2<-data_centro_calc$dim2_centro

X1<-data_centro_calc$dim1_cen
Y1<-data_centro_calc$dim2_cen

data_centro_calc$dist<- sqrt(((X2-X1)^2)+((Y2-Y1)^2))
#View(data_centro_calc)

data_dist_centro<- data_centro_calc %>%
  group_by(crono_code) %>%
  summarise(dist_cen = mean(dist))
View(data_dist_centro)

write.csv(data_dist_centro, "crono_centro.csv")

# distance from centroid to forest

data_forest<- data_chrono %>%
  filter(Habitat.x == "Forest") %>%
  select(crono_code,Dim1, Dim2)

data_cen_forest<-merge(data_centroide, data_forest, by="crono_code", all.x = TRUE)

data_cen_forest$dist <- dist(data_cen_forest$Dim1,
                             data_cen_forest$Dim2,
                             data_cen_forest$dim1_centro,
                             data_cen_forest$dim2_centro)

write.csv(data_cen_forest, "crono_forest.csv")

#longitude path?

for (j in levels(as.factor(data_chrono$crono_code))){
  data_10 <- data_chrono %>%
    filter(crono_code == j)
  
  distance <- 0
  for (i in 2:dim(data_10)[1]){
    
    X2<-data_10$Dim1[i]
    Y2<-data_10$Dim2[i]
    
    X1<-data_10$Dim1[i-1]
    Y1<-data_10$Dim2[i-1]
    
    dist<- dist(data_10$Dim1[i-1], data_10$Dim2[i-1], data_10$Dim1[i], data_10$Dim2[i])
    distance <- distance + dist
  }
  print(paste("crono",j,"distance",distance))
}
