

library(factoextra)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(ggfortify)
library(ggrepel)
library(dplyr)
library(data.table)
library(stringr)
library(emmeans)
library(multcomp)
library(readr)
library(forcats)
library(tidyr)
library(gridExtra)
library(gclus)
library(ggdendro)
library(remotes)
library(tidyverse)
library(bibtex)
library(bib2df)
library(readxl)
library(janitor)
library(here)
library(quanteda)
library(collostructions)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(csv)
library(corrplot)
library(RColorBrewer)
library(lme4)


morfi_benchmarking_nonwood_sections_pub <- read_csv("morfi_benchmarking_nonwood_sections_publication.csv")

morfi_benchmarking_nonwood_sections_PCA <- subset(morfi_benchmarking_nonwood_sections_pub,select = -c(Biomass,Section,Level,Morfi.Rep))

PCA.benchmarking.nonwood.sections <- prcomp(~ ., data = morfi_benchmarking_nonwood_sections_PCA, na.action=na.omit, center = TRUE, scale=TRUE)



PCA.benchmarking.nonwood.biomass.sections <- c(rep("Banana-Leaf",12),
                                               rep("Banana-Stem",12),
                                               rep("Sugarcane-Bagasse",12),
                                               rep("Sugarcane-Mulch",12),
                                               rep("Spinifex-Leaf",12),
                                               rep("Sorghum-Leaf",12),
                                               rep("Sorghum-Sheath",12),
                                               rep("Sorghum-Stem(<1m)",12),
                                               rep("Sorghum-Stem(>1m)",12))

PCA.benchmarking.nonwood.biomass.sections <- factor(PCA.benchmarking.nonwood.biomass.sections, levels = c("Banana-Leaf",
                                                                                                          "Banana-Stem",
                                                                                                          "Sugarcane-Bagasse",
                                                                                                          "Sugarcane-Mulch",
                                                                                                          "Spinifex-Leaf",
                                                                                                          "Sorghum-Leaf",
                                                                                                          "Sorghum-Sheath",
                                                                                                          "Sorghum-Stem(<1m)",
                                                                                                          "Sorghum-Stem(>1m)"))

# PCA.benchmarking.nonwood.HPH.sections <- c(rep("L",4),rep("M",4),rep("H",4),  #Banana-Leaf
#                                            rep("L",4),rep("M",4),rep("H",4),  #Banana-Stem
#                                            rep("L",4),rep("M",4),rep("H",4),  #Sugarcane-Bagasse
#                                            rep("L",4),rep("M",4),rep("H",4),  #Sugarcane-Mulch
#                                            rep("L",4),rep("M",4),rep("H",4),  #Spinifex
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Tall-Leaf
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Tall-Sheath
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Tall-Stem<1m
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Tall-Stem>1m
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Short-Leaf
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4),  #Sorghum-Short-Sheath
#                                            rep("L",4),rep("M",4),rep("H",4),rep("L",4),rep("M",4),rep("H",4))  #Sorghum-Short-Stem
# 
# 
# PCA.benchmarking.nonwood.HPH.sections <- factor(PCA.benchmarking.nonwood.HPH.sections, levels = c("L","M","H"))


pca_biomass_nonwood_sections <- ggbiplot(PCA.benchmarking.nonwood.sections,varname.size = 3,obs.scale = 0.5, ellipse=TRUE, ellipse.prob = 0.68, alpha = 0.5, groups = PCA.benchmarking.nonwood.biomass.sections) + 
  scale_color_manual(name = 'Biomass', values=c("darkgoldenrod1","darkgoldenrod3","chartreuse3","chartreuse3","brown", "deepskyblue","dodgerblue2","dodgerblue4","darkblue", "orchid1", "darkorchid1", "darkorchid4")) + 
  theme(legend.direction = 'horizontal', legend.position = 'top', panel.background = element_blank()) + 
  geom_hline(yintercept=0,colour = "grey80", size = 0.5, lty="dashed") + 
  geom_vline(xintercept=0,colour = "grey80", size = 0.5, lty="dashed")

pca_biomass_nonwood_sections + theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background = element_rect(fill="white",colour = "black", size = 2))






















