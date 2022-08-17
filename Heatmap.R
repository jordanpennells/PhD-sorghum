
library(corrplot)
library(RColorBrewer)
library(kableExtra)
library(dplyr)
library(seriation)
library(gplots)
library(tidyverse)
library(dendextend)
library(ggcorrplot)
library(gplots)




###################### HEATMAP ##########################

morfi_cnf_heatmap <- read.csv("morfi_cnf_heatmap_all.csv")

morfi_cnf_heatmap <- morfi_cnf_heatmap %>% dplyr::group_by(Variety,Section,HPH) %>% dplyr::summarise(across(.cols=everything(),mean,na.rm=TRUE)) %>% dplyr::ungroup()

morfi_cnf_heatmap2 <- morfi_cnf_heatmap[,4:24]

morfi_cnf_heatmap2 <- as_tibble(scale(morfi_cnf_heatmap2, scale=TRUE,center = TRUE))

morfi_cnf_heatmap <- bind_cols(morfi_cnf_heatmap[,1:3],morfi_cnf_heatmap2)




heatmap.cor <- cor(morfi_cnf_heatmap2)

heatmap.dist <- as.dist(1 - heatmap.cor)
heatmap.tree <- hclust(heatmap.dist, method="complete")
plot(heatmap.tree)

heatmap.dend <- as.dendrogram(heatmap.tree)

clusters <- cutree(heatmap.dend, k=4, order_clusters_as_data = FALSE)
table(clusters)

clusters.df <- data.frame(ID = names(clusters), cluster = clusters)

cluster3.ID <- filter(clusters.df, cluster == 3)$ID

cat(as.character(cluster3.ID[1:5]), quote=FALSE,sep="\n")




heatmap.long <- pivot_longer(morfi_cnf_heatmap,cols=4:24)

heatmap.long <- heatmap.long %>% dplyr::group_by(Variety,Section,HPH,name) %>% dplyr::summarise(value=mean(value,na.rm=TRUE))



color.scheme <- rev(brewer.pal(4,"RdYlGn"))

heatmap.long %>%
  ggplot(aes(x = HPH, y = name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colors=color.scheme, limits = c(-4,4)) + 
  theme(axis.text.y = element_text(size = 6))

heatmap.long %>%
  ggplot(aes(x = HPH, y = name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colors=color.scheme, limits = c(-4,4)) + 
  theme(axis.text.y = element_text(size = 6)) + facet_grid(~Variety)

heatmap.long %>%
  ggplot(aes(x = HPH, y = name)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colors=color.scheme, limits = c(-4,4)) + 
  theme(axis.text.y = element_text(size = 6)) + facet_grid(Variety~Section)

