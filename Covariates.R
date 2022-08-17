

library(ggplot2)
library(readr)
library(dplyr)
library(tidyverse)
library(olsrr)


 ## Analysing covariate data - Biochemical composition

biochem_comp <- read_csv("Biochem comp.csv")


 ## Generating new variable based on wt% basis and CHL basis

biochem_comp$C_wt <- biochem_comp$Glucan
biochem_comp$H_wt <- biochem_comp$Xylan + biochem_comp$Mannan + biochem_comp$Arabinan + biochem_comp$Galactan + biochem_comp$Rhamnan
biochem_comp$L_wt <- biochem_comp$Klason_Lignin + biochem_comp$AS_Lignin
biochem_comp$E_wt <- biochem_comp$Extractives
biochem_comp$A_wt <- biochem_comp$Ash

biochem_comp$C_CHL <- biochem_comp$C_wt/(biochem_comp$C_wt + biochem_comp$H_wt + biochem_comp$L_wt)
biochem_comp$H_CHL <- biochem_comp$H_wt/(biochem_comp$H_wt + biochem_comp$H_wt + biochem_comp$L_wt)
biochem_comp$L_CHL <- biochem_comp$L_wt/(biochem_comp$L_wt + biochem_comp$H_wt + biochem_comp$L_wt)



biochem_comp$biochem.ID <- paste(biochem_comp$Variety, biochem_comp$Section, sep = "-")

fits.coef$biochem.ID <- rownames(fits.coef)

fits.coef$mag <- sqrt((fits.coef$intercept^2)+(fits.coef$slope^2))

biochem_fit.coef <- merge(biochem_comp,fits.coef,by="biochem.ID")



biochem_model <- lm(mag ~ C_wt + H_wt + L_wt + E_wt + A_wt + 0 , data = biochem_fit.coef)

ols_step_all_possible(biochem_model)

ols_step_best_subset(biochem_model)


#biochem_model_CHL <- lm(mag ~ C_CHL + H_CHL + L_CHL, data = biochem_fit.coef)

#ols_step_all_possible(biochem_model_CHL)

#ols_step_best_subset(biochem_model_CHL)


biochem_model_angle <- lm(angle ~ C_wt + H_wt + L_wt + E_wt + A_wt + 0, data = biochem_fit.coef)

ols_step_all_possible(biochem_model_angle)

ols_step_best_subset(biochem_model_angle)


biochem_model_mag <- lm(mag ~ C_wt + L_wt + E_wt + A_wt +0, data = biochem_fit.coef)
summary(biochem_model_mag)


biochem_model_angle2 <- lm(angle ~ C_wt + L_wt + A_wt +0, data = biochem_fit.coef)
summary(biochem_model_angle2)




