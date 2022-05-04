###Moderation###
  # x = independent variable (CTQ)
  # m = moderator (group)
  # y = dependent variable (substance use disorder)

#compute the interaction terms (ie. the interaction between independent variable x and potential moderator m)
traumaxCHR <- MAPST1$T1_CTQ_total*MAPST1$SIPS_CHR #all CHR people

traumaxPTSD <- MAPST1$T1_CTQ_total*MAPST1$nCHRPTSD #PTSD only, no CHR dx

traumaxCHRnPTSD <- MAPST1$T1_CTQ_total*MAPST1$CHRnPTSD  #CHR only, no PTSD dx


#Define the regression model with x, m, and the interaction x*m predicting dependent variable, as well as any covariates
allCHR_cannabis <- glm(scid_p52 ~ T1_CTQ_total + SIPS_CHR + traumaxCHR + sex + intv_age, MAPST1, family = "quasibinomial")
allCHR_other <- glm(other_drug ~ T1_CTQ_total + SIPS_CHR + traumaxCHR + sex + intv_age, MAPST1, family = "quasibinomial")

PTSD_cannabis <- glm(scid_p52 ~ T1_CTQ_total + ncHRPTSD + traumaxPTSD + sex + intv_age, MAPST1, family = "quasibinomial")
PTSD_other <- glm(other_drug ~ T1_CTQ_total + nCHRPTSD  + traumaxPTSD + sex + intv_age, MAPST1, family = "quasibinomial")

CHR_cannabis <- glm(scid_p52 ~ T1_CTQ_total + CHRnPTSD + traumaxCHRnPTSD + sex + intv_age, MAPST1, family = "quasibinomial")
CHR_other <- glm(other_drug ~ T1_CTQ_total + CHRnPTSD + traumaxCHRnPTSD + sex + intv_age, MAPST1, family = "quasibinomial")


#view the results- check if the interaction term significantly predicts dependent variable (if it does, there is a moderation effect!)

summary(allCHR_cannabis)
summary(allCHR_other)

summary(PTSD_cannabis)
summary(PTSD_other)

summary(CHR_cannabis)
summary(CHR_other)

#plot the model to visualize results- you will need sjPlot package for this function 
plot_model()


#create nice looking regression table of results  #these did not work
apa.reg.table(allCHR_cannabis, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)

apa.reg.table(allCHR_other, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)

apa.reg.table(PTSD_cannabis, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)

apa.reg.table(PTSD_other, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)

apa.reg.table(CHR_cannabis, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)

apa.reg.table(CHR_cannabis, 
              filename = "allCHR_cannabis_SRP.doc", 
              table.number = NA, 
              prop.var.conf.level = 0.95)




