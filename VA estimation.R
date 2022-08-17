
library(dp)
library(lme4)

fish<-read.csv("C:/Users/jorda/OneDrive/Complex Evolution/R Code/Week 3/BIOL3350_zfish_data.csv")


head(fish)
summary(fish)
fish$sire = as.factor(fish$sire)
fish$dam = as.factor(fish$dam)
fish$sex = as.factor(fish$sex)

female = fish[fish$sex=='F',]
male = fish[fish$sex=='M',]

boxplot(female$A_SL~female$sire)
boxplot(female$B_CPD~female$sire)
boxplot(female$C_maxBD~female$sire)
boxplot(female$D_preD_l~female$sire)
boxplot(female$burst~female$sire)

boxplot(male$A_SL~male$sire)
boxplot(male$B_CPD~male$sire)
boxplot(male$C_maxBD~male$sire)
boxplot(male$D_preD_l~male$sire)
boxplot(male$burst~male$sire)

f1=lmer(A_SL~1+(1|sire)+(1|sire:dam),data=female)
f2=lmer(B_CPD~1+(1|sire)+(1|sire:dam),data=female)
f3=lmer(C_maxBD~1+(1|sire)+(1|sire:dam),data=female)
f4=lmer(D_preD_l~1+(1|sire)+(1|sire:dam),data=female)

m1=lmer(A_SL~1+(1|sire)+(1|sire:dam),data=male)
m2=lmer(B_CPD~1+(1|sire)+(1|sire:dam),data=male)
m3=lmer(C_maxBD~1+(1|sire)+(1|sire:dam),data=male)
m4=lmer(D_preD_l~1+(1|sire)+(1|sire:dam),data=male)

summary(f1)
summary(f2)
summary(f3)
summary(f4)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

logLik(f1)
logLik(f2)
logLik(f3)
logLik(f4)
logLik(m1)
logLik(m2)
logLik(m3)
logLik(m4)

f1r = lmer(A_SL ~ 1 + (1|sire:dam), data=female)
summary(f1r)
f2r = lmer(B_CPD ~ 1 + (1|sire:dam), data=female)
summary(f1r)
f3r = lmer(C_maxBD ~ 1 + (1|sire:dam), data=female)
summary(f1r)
f4r = lmer(D_preD_l ~ 1 + (1|sire:dam), data=female)
summary(f1r)
logLik(f1r)
logLik(f2r)
logLik(f3r)
logLik(f4r)

m1r = lmer(A_SL ~ 1 + (1|sire:dam), data=male)
m2r = lmer(B_CPD ~ 1 + (1|sire:dam), data=male)
m3r = lmer(C_maxBD ~ 1 + (1|sire:dam), data=male)
m4r = lmer(D_preD_l ~ 1 + (1|sire:dam), data=male)
logLik(m1r)
logLik(m2r)
logLik(m3r)
logLik(m4r)

pvalue = 1-(pchisq(2*(logLik(f1) - logLik(f1r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(f2) - logLik(f2r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(f3) - logLik(f3r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(f4) - logLik(f4r)),df=0.5))
pvalue

pvalue = 1-(pchisq(2*(logLik(m1) - logLik(m1r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(m2) - logLik(m2r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(m3) - logLik(m3r)),df=0.5))
pvalue
pvalue = 1-(pchisq(2*(logLik(m4) - logLik(m4r)),df=0.5))
pvalue

male_means = aggregate(cbind(male$A_SL,male$B_CPD,male$C_maxBD,male$D_preD_l), by =
                         list(male$sire), FUN=mean)
colnames(male_means) = c('sire','A_SL','B_CPD','C_maxBD','D_preD_l')
female_means = aggregate(cbind(female$A_SL,female$B_CPD,female$C_maxBD,female$D_preD_l), by =
                           list(female$sire), FUN=mean)
colnames(female_means) = c('sire', 'A_SL','B_CPD','C_maxBD','D_preD_l')

cor(cbind(male_means$A_SL,male_means$B_CPD,male_means$C_maxBD,male_means$D_preD_l))
cor(cbind(male$A_SL,male$B_CPD,male$C_maxBD,male$D_preD_l))
