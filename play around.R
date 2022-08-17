## Playing around with RStudio to get an idea of how things work

library(readr)
library(agricolae)
library(MASS)
library(lattice)
library(effects)
library(ggplot2)

## Import nanopaper data

nanopaper <- read_csv("nanopaper.csv")
morfi_cnf <- read_csv("morfi_cnf.csv")
morfi_dl <- read_csv("morfi_dl.csv")
sedimentation <- read_csv("sedimentation.csv")
biochem_comp <- read_csv("Biochem comp.csv")


## Fixing up column names to be RStudio compatible

colnames(nanopaper)[1] <- "sample_name"
colnames(nanopaper)[2] <- "variety"
colnames(nanopaper)[3] <- "rep"
colnames(nanopaper)[4] <- "section"
colnames(nanopaper)[5] <- "HPH"
colnames(nanopaper)[6] <- "nanopaper_rep"
colnames(nanopaper)[7] <- "weight"
colnames(nanopaper)[8] <- "thickness"
colnames(nanopaper)[9] <- "density"
colnames(nanopaper)[10] <- "porosity"
colnames(nanopaper)[11] <- "grammage"
colnames(nanopaper)[12] <- "strain"
colnames(nanopaper)[13] <- "UTS"
colnames(nanopaper)[14] <- "modulus"
colnames(nanopaper)[15] <- "toughness"
colnames(nanopaper)[16] <- "TI"
colnames(nanopaper)[17] <- "strain_star"
colnames(nanopaper)[18] <- "UTS_star"
colnames(nanopaper)[19] <- "modulus_star"
colnames(nanopaper)[20] <- "toughness_star"
colnames(nanopaper)[21] <- "TI_star"

colnames(morfi_cnf)[1] <- "sample_name"
colnames(morfi_cnf)[2] <- "variety"
colnames(morfi_cnf)[3] <- "rep"
colnames(morfi_cnf)[4] <- "section"
colnames(morfi_cnf)[5] <- "HPH"

colnames(morfi_dl)[1] <- "sample_name"
colnames(morfi_dl)[2] <- "variety"
colnames(morfi_dl)[3] <- "rep"
colnames(morfi_dl)[4] <- "section"
colnames(morfi_dl)[5] <- "HPH"

colnames(sedimentation)[1] <- "sample_name"
colnames(sedimentation)[2] <- "variety"
colnames(sedimentation)[3] <- "rep"
colnames(sedimentation)[4] <- "section"
colnames(sedimentation)[5] <- "HPH"

colnames(biochem_comp)[1] <- "sample_name"
colnames(biochem_comp)[2] <- "variety"
colnames(biochem_comp)[3] <- "section"
colnames(biochem_comp)[4] <- "celignis.rep"
colnames(biochem_comp)[5] <- "Total Sugars"

summary(nanopaper)


## Density plots

plot(density(nanopaper$density, na.rm=T), main= "Density plot of Density")
plot(density(nanopaper$porosity, na.rm=T), main= "Density plot of Porosity")
plot(density(nanopaper$grammage, na.rm=T), main= "Density plot of Grammage")
plot(density(nanopaper$TI_star, na.rm=T), main= "Density plot of TI*")
plot(density(nanopaper$toughness_star, na.rm=T), main= "Density plot of Toughness*")
plot(density(nanopaper$modulus_star, na.rm=T), main= "Density plot of Modulus*")

## Cumulative Distribution Function (CDF) plots

plot(ecdf(nanopaper$density))
plot(ecdf(nanopaper$grammage))
plot(ecdf(nanopaper$TI_star))


## Visualising boxplots for important variables

boxplot(nanopaper$'TI_star', main='TI* boxplot',ylab='Tensile Index (Nm/g)')
#OR
plot(TI_star ~ factor(variety), data = nanopaper, xlab="", ylab="Tensile Index (Nm/g)", main="Tensile Index of different sorghum varieties")

plot_TIvsHPH <- with(nanopaper, boxplot(density ~ HPH, main = "TI* vs HPH", xlab = "HPH energy", ylab = "Density (g/cm3)" ))
#OR
plot(density ~ factor(HPH), data = nanopaper, xlab="HPH energy", ylab="Density (g/cm3)", main="TI* vs HPH")

plot(TI_star ~ factor(section), data = nanopaper, xlab="Sorghum section", ylab="Tensile Index (Nm/g)", main="Tensile Index of different sorghum sections")
plot(toughness_star ~ factor(section), data = nanopaper, xlab="Sorghum section", ylab="Toughness (MJ/m3)", main="Toughness of different sorghum sections")


## Adding mean data labels to the boxplot

plot(TI_star ~ factor(variety), data = nanopaper, xlab="", ylab="Tensile Index (Nm/g)", main="Tensile Index of different sorghum varieties")
TI_star.means <- aggregate(TI_star ~ variety, data = nanopaper, mean)
points(1:4, TI_star.means$TI_star, col = "red")
text(1:4, TI_star.means$TI_star + 6, labels = TI_star.means$TI_star)

## Reordering boxplot to go up in increasing HPH level

new_order <- with(nanopaper, reorder(HPH, density, median, na.rm=T))
plot_DensityvsHPH <- with(nanopaper, boxplot(density ~ new_order, main = "Density vs HPH", xlab = "HPH energy", ylab = "Density (g/cm3)", col="#69b3a2"))
plot_TIvsHPH <- with(nanopaper, boxplot(TI_star ~ new_order, main = "TI* vs HPH", xlab = "HPH energy", ylab = "TI* (Nm/g)", col="#69b3a2"))
plot_ToughvsHPH <- with(nanopaper, boxplot(toughness_star ~ new_order, main = "Toughness* vs HPH", xlab = "HPH energy", ylab = "Toughness* (MJ/m3)", col="#69b3a2"))
plot_ModvsHPH <- with(nanopaper, boxplot(modulus_star ~ new_order, main = "Modulus* vs HPH", xlab = "HPH energy", ylab = "Modulus* (GPa)", col="#69b3a2"))


## Scatterplot of multiple variables

plot(nanopaper$TI_star~nanopaper$strain_star)
plot(nanopaper$TI_star~nanopaper$modulus_star)
plot(nanopaper$TI_star~nanopaper$density)
plot(nanopaper$TI_star~nanopaper$grammage)
plot(nanopaper$toughness_star~nanopaper$modulus_star)
plot(nanopaper$toughness_star~nanopaper$density)


## t test examples

t.test(TI_star ~ factor(variety), data = nanopaper[nanopaper$variety %in% c("GreenleafBMR", "Graingrass"),])
t.test(TI_star ~ factor(variety), data = nanopaper[nanopaper$variety %in% c("Sugargraze", "Graingrass"),])
t.test(TI_star ~ factor(variety), data = nanopaper[nanopaper$variety %in% c("Yemen", "Graingrass"),])
t.test(TI_star ~ factor(variety), data = nanopaper[nanopaper$variety %in% c("GreenleafBMR", "Sugargraze"),])
t.test(TI_star ~ factor(variety), data = nanopaper[nanopaper$variety %in% c("GreenleafBMR", "Yemen"),])


#Redefining a factor

HPH.fac <- factor(nanopaper$HPH, levels = c("L", "M", "H"))
variety.fac <- factor(nanopaper$variety, levels = c("Sugargraze", "Yemen", "GreenleafBMR", "Graingrass"))
section.fac <- factor(nanopaper$section, levels = c("Leaf", "Sheath", "Stem", "Stem(<1m)", "Stem(>1m)"))


## Starting to look at Analysis of Variance (ANOVA)

TI_star.aov.A <- aov(TI_star ~ variety + section + HPH.fac, data=nanopaper)
anova(TI_star.aov.A)
TI_star.aov.M <- aov(TI_star ~ variety * section * HPH.fac, data=nanopaper)
anova(TI_star.aov.M)

toughness_star.aov.A <- aov(toughness_star ~ variety + section + HPH.fac, data=nanopaper)
anova(toughness_star.aov.A)

modulus_star.aov.A <- aov(modulus_star ~ variety + section + HPH.fac, data=nanopaper)
anova(modulus_star.aov.A)


modulus_star.aov.B <- aov(modulus_star ~ variety + section + HPH.fac + (1), data=nanopaper)
anova(modulus_star.aov.B)

## Viewing ANOVA effects

allEffects(TI_star.aov.A)
plot(allEffects(TI_star.aov.A))

allEffects(toughness_star.aov.A)
plot(allEffects(toughness_star.aov.A))

allEffects(modulus_star.aov.A)
plot(allEffects(modulus_star.aov.A))

## Estimate of the overall experimental variation

cv.model(TI_star.aov.A)


## Combining CSV files into a master table


ID=1:nrow(nanopaper)
nanopaper <- cbind(ID,nanopaper)

#master_table <- merge(sedimentation,nanopaper)
#master_table <- merge(morfi_cnf,master_table)
#master_table <- master_table[,c(29,1:28,30:45)]



write.csv(x=nanopaper, "C:/Users/s4294173/Desktop/Sorghum2020/nanopaper_clean.csv", row.names = FALSE)


