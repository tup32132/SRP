# Clean it up
removeNa <- function() {
  #remove NAs
  MAPS <- MAPS %>% filter(!is.na(SIPS_CHR))
  MAPS <- MAPS %>% filter(!is.na(DUF_17))
  MAPS <- MAPS %>% filter(!is.na(scid_p22))
  MAPS <- MAPS %>% filter(!is.na(scid_p24)) 
}

#CTQ variable generation & reversal check
extractCTQ <- function() {
  #creating CTQ subset as its own df
  CTQ <- MAPS %>% as_tibble %>% select(starts_with("T2_CTQ"))
  psych::alpha(CTQ, cumulative = TRUE, check.keys = TRUE) # check to see reversals
  
}

#get summary variables
summarizeSCID <- function() {
  
  # Get counts for each group
  CD  <- MAPS %>% filter(SIPS_CHR == 1 & (scid_p21 == 3 | scid_p23 == 3))
  CnD <- MAPS %>% filter(SIPS_CHR == 1 & (scid_p21 != 3 | scid_p23 != 3))
  nCD <- MAPS %>% filter(SIPS_CHR != 1 & (scid_p21 == 3 | scid_p23 == 3))
  
  # get some supercontrols (zero scid dx lifetime)
  MAPS$scid_total <- rowSums((MAPS %>% select(scid_p8,scid_p9,scid_p11:scid_p12,scid_p14:scid_p18,scid_p19:scid_p20,scid_p21:scid_p29,
                                              scid_p30:scid_p31,scid_p32:scid_p43,scid_p44:scid_p45,scid_p46:scid_p80,scid_p81:scid_p82,
                                              scid_p83:scid_p96,scid_p97:scid_p98,scid_p102:scid_p122)))
  
  # create datqframe for clinical dx
  sumof99 <- 283
  dx <- MAPS %>% dplyr::filter(!is.na(scid_total)) %>% dplyr::filter(scid_total< sumof99)
  
  # filters
  sumOfNeverDiagnosed <- 94 # sum of all SCID items equals 94
  control <- dx %>% data.frame() %>% filter((scid_total== sumOfNeverDiagnosed))
  
}