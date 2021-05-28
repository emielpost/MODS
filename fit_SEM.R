#!/usr/bin/env Rscript

myModel <- '
# regressions
Albumin ~ nor + Liver + Albumin_lag_1
Albumin_lag_1 ~ nor_lag_1 + Liver_lag_1
Circ ~ Circ_lag_1
Coag ~ Blood + Coag_lag_1
Coag_lag_1 ~ Blood_lag_1
Colloids ~ GIS
Colloids_lag_1 ~ GIS_lag_1
Crystalloids ~ Ent
Crystalloids_lag_1 ~ Ent_lag_1
dobutamine ~ dobutamine_lag_1 + nor
dobutamine_lag_1 ~ nor_lag_1
Ent ~ Ent_lag_1 + Parent + MV_lag_1 + Renal
Ent_lag_1 ~ Coag_lag_1 + Renal_lag_1 + Parent_lag_1
erytromycine ~ erytromycine_lag_1 + GIS
erytromycine_lag_1 ~ GIS_lag_1
GIS ~ Ent + GIS_lag_1 + Parent
GIS_lag_1 ~ Ent_lag_1 + Parent_lag_1
hydrocortison ~ hydrocortison_lag_1
Liver ~ Coag + Liver_lag_1 + Parent
Liver_lag_1 ~ Coag_lag_1 + Parent_lag_1
midazolam ~ midazolam_lag_1 + nor
midazolam_lag_1 ~ nor_lag_1
MV ~ Circ + MV_lag_1
MV_lag_1 ~ Circ_lag_1
nor ~ Colloids + nor_lag_1
nor_lag_1 ~ Colloids_lag_1
propofol ~ MV
propofol_lag_1 ~ MV_lag_1
Resp ~ Resp_lag_1
Resp_lag_1 ~ Crystalloids_lag_1 + midazolam_lag_1
RRT ~ furosemide + Renal + RRT_lag_1
RRT_lag_1 ~ furosemide_lag_1 + Renal_lag_1
Resp ~ Crystalloids + midazolam + MV_lag_1
VP ~ nor + VP_lag_1
VP_lag_1 ~ nor_lag_1

# correlations
Blood ~~ Blood_lag_1
Blood_lag_1 ~~ midazolam_lag_1
Circ ~~ Crystalloids
Circ ~~ nor
Circ_lag_1 ~~ Crystalloids_lag_1
Circ_lag_1 ~~ nor_lag_1
Colloids ~~ Colloids_lag_1
Crystalloids ~~ Colloids
Crystalloids ~~ Crystalloids_lag_1
Crystalloids ~~ nor
Crystalloids ~~ propofol
Crystalloids_lag_1 ~~ Colloids_lag_1
Crystalloids_lag_1 ~~ nor_lag_1
Crystalloids_lag_1 ~~ propofol_lag_1
furosemide_lag_1 ~~ furosemide
hydrocortison_lag_1 ~~ Blood
midazolam ~~ Blood
opioids_lag_1 ~~ opioids
Parent_lag_1 ~~ Parent
propofol ~~ propofol_lag_1
Renal ~~ RRT_lag_1
Renal_lag_1 ~~ Renal
Resp ~~ MV
Resp_lag_1 ~~ MV_lag_1
'
## Note:
#Renal_lag_1 ~~ Renal (odot)
#hydrocortison_lag_1 ~~ hydrocortison (odot)
#Parent_lag_1 ~~ Parent (odot)
#hydrocortison_lag_1 ~~ Blood (odot)
#opioids_lag_1 ~~ opioids (odot)
#furosemide_lag_1 ~~ furosemide (odot)

fit <- sem(myModel, sample.cov = cov_matrix, sample.nobs = 9634,
           ordered = c('Circ', 'Coag', 'Liver', 'Renal', 'Resp', 'GIS',
               'MV', 'RRT', 'Circ_lag_1', 'Coag_lag_1',
               'Liver_lag_1', 'Renal_lag_1', 'Resp_lag_1',
               'GIS_lag_1', 'MV_lag_1', 'RRT_lag_1'))

summ <- summary(fit, standardized = TRUE)[[1]][c('lhs','rhs','std.all')]
write.table(summ, paste0(output_dir,'SEM.txt'), row.names=FALSE)

