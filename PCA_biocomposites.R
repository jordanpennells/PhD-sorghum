
library(factoextra)
library(ggplot2)
library(ggfortify)
library(devtools)
library(ggbiplot)
library(ggfortify)
library(ggrepel)
library(dplyr)
library(FactoMineR)
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
library(dendextend)
library(corrplot)
library(gridExtra)
library(reactablefmtr)


################# PCA - Pulp ###################


PCA_pulp <- read_csv("PCA_pulp.csv")


PCA_pulp_clean <- subset(PCA_pulp,select = -c(Sample_name,Biomass,Type,Location,Morfi.Rep,Section))

# colnames(morfi_cnf_PCA)[14] <- "\nfibre_broken"


PCA.pulp <- prcomp(~ ., data=PCA_pulp_clean, na.action=na.omit, center = TRUE, scale.=TRUE)

summary(PCA.pulp)

fviz_eig(PCA.pulp)

ggbiplot(PCA.pulp)



# PCA.pulp.location <- c(rep("PP",4),rep("Yemen",48),rep("GreenleafBMR",36),rep("Graingrass",35))

PCA.pulp.location <- factor(PCA_pulp$Location, levels = c("PP","Lab","PhD"))

p_pulp_location <- ggbiplot(PCA.pulp,
                      varname.size = 3,
                      obs.scale = 0.5, 
                      ellipse=TRUE, 
                      ellipse.prob = 0.68, 
                      alpha = 0.5, 
                      groups = PCA.pulp.location) + 
  scale_color_manual(name = 'Location', values=c("black","grey","blue")) + 
  theme(legend.direction = 'horizontal',legend.position = 'top', panel.background = element_blank()) + 
  geom_hline(yintercept=0,colour = "grey80",size = 0.5,lty="dashed") + 
  geom_vline(xintercept=0,colour = "grey80",size = 0.5, lty="dashed")


p_pulp_location + theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.background = element_rect(fill="white",colour = "black", size = 2))





################# PCA - MFC ###################

PCA_MFC <- read_csv("PCA_MFC.csv")


PCA_MFC_clean <- subset(PCA_MFC,select = -c(Sample_name,Biomass,Type,Location,Extrusion,Morfi.Rep,Section))

# colnames(morfi_cnf_PCA)[14] <- "\nfibre_broken"


PCA.MFC <- prcomp(~ ., data=PCA_MFC_clean, na.action=na.omit, center = TRUE, scale.=TRUE)

summary(PCA.MFC)

fviz_eig(PCA.MFC)

ggbiplot(PCA.MFC)



PCA.MFC.location <- factor(PCA_MFC$Location, levels = c("PP","Lab","PhD"))

p_MFC_location <- ggbiplot(PCA.MFC,
                      varname.size = 3,
                      obs.scale = 0.5, 
                      ellipse=TRUE, 
                      ellipse.prob = 0.68, 
                      alpha = 0.5, 
                      groups = PCA.MFC.location) + 
  scale_color_manual(name = 'Location', values=c("black","grey","purple")) + 
  theme(legend.direction = 'horizontal',legend.position = 'top', panel.background = element_blank()) + 
  geom_hline(yintercept=0,colour = "grey80",size = 0.5,lty="dashed") + 
  geom_vline(xintercept=0,colour = "grey80",size = 0.5, lty="dashed")


p_MFC_location + theme(panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.background = element_rect(fill="white",colour = "black", size = 2))






PCA.MFC.extrusion <- factor(PCA_MFC$Extrusion, levels = c("Correct","Incorrect","PhD"))

p_MFC_extrusion <- ggbiplot(PCA.MFC,
                           varname.size = 3,
                           obs.scale = 0.5, 
                           ellipse=TRUE, 
                           ellipse.prob = 0.68, 
                           alpha = 0.5, 
                           groups = PCA.MFC.extrusion) + 
  scale_color_manual(name = 'Extrusion conditions', values=c("black","grey50","purple" )) + 
  theme(legend.direction = 'horizontal',legend.position = 'top', panel.background = element_blank()) + 
  geom_hline(yintercept=0,colour = "grey80",size = 0.5,lty="dashed") + 
  geom_vline(xintercept=0,colour = "grey80",size = 0.5, lty="dashed")


p_MFC_extrusion + theme(panel.grid.minor = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.background = element_rect(fill="white",colour = "black", size = 2))


















################# PCA - Pulp + MFC ###################

PCA_pulpMFC <- read_csv("PCA_pulpMFC.csv")


PCA_pulpMFC_clean <- subset(PCA_pulpMFC,select = -c(Sample_name,Biomass,Type,Location,Morfi.Rep,Section,Extrusion))

PCA.pulpMFC <- prcomp(~ ., data = PCA_pulpMFC_clean, na.action=na.omit, center = TRUE, scale.=TRUE)


summary(PCA.pulpMFC)

fviz_eig(PCA.pulpMFC)

ggbiplot(PCA.pulpMFC)



PCA.PulpMFC.Type <- factor(PCA_pulpMFC$Type, levels = c("Pulp-PP","Pulp-Lab","Pulp-PhD","MFC"))


p_PulpMFC_type <- ggbiplot(PCA.pulpMFC,
                            varname.size = 3,
                            obs.scale = 0.75, 
                            ellipse=TRUE, 
                            ellipse.prob = 0.68, 
                            alpha = 0.5, 
                            groups = PCA.PulpMFC.Type) + 
  scale_color_manual(name = 'Type', values=c("black","grey50","grey80","green")) + 
  theme(legend.direction = 'horizontal',legend.position = 'top', panel.background = element_blank()) + 
  geom_hline(yintercept=0,colour = "grey80",size = 0.5,lty="dashed") + 
  geom_vline(xintercept=0,colour = "grey80",size = 0.5, lty="dashed")


p_PulpMFC_type + theme(panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.background = element_rect(fill="white",colour = "black", size = 2))











