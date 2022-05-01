###Moderation###
  # x = independent variable (CTQ)
  # m = moderator (group)
  # y = dependent variable (substance use disorder)

#compute the interaction term (ie. the interactionn between independent variable x and potential moderator m)
traumaXgroup <- T1_CTQ*group

#Define the regression model with x, m, and the interaction x*m predicting dependent variable, as well as any covariates
modelname <- lm(y ~ T1_CTQ + group + traumaXgroup + covariate1 + covariate2, dataset)

#view the results- check if the interaction term significantly predicts dependent variable (if it does, there is a moderation effect!)
summary(modelname)

#plot the model to visualize results- you will need sjPlot package for this function 
plot_model(modelname)

#calculate simple slopes- this will probe the effect of x within specific levels of m (group)
simple_slopes(modelname)


#create nice looking regression table of results
apa.reg.table(modelname, 
              filename = "RegressionTable_SRP", 
              table.number = NA, 
              prop.var.conf.level = 0.95)