##Preliminary analyses to run before final model##

#Get descriptive statistics
 #you will need to report means and SD for each variable
 #variable is normally distributed if the skewness and kurtosis values are less than |2|
describe(MAPST1[c("scid_p52", "other_drug", "scid_p117", "T1_CTQ_total","SIPS_CHR")])

#you can also check visually by creating a histogram plot
#  hist(VariableName)

#Correlation matrix of variables of interest and any potential covariates 
apa.cor.table(
  MAPST1[c("other_drug", "scid_p52", "T1_CTQ_total", "scid_p117", "SIPS_CHR", "intv_age", "sex")],
  filename = "Correlation_SRP.doc",
  table.number = NA,
  show.conf.interval = FALSE,
  show.sig.stars = TRUE,
  landscape = TRUE
)
