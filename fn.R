# Clean it up
removeNa <- function() {
  #remove NAs
  MAPS <- MAPS %>% filter(!is.na(SIPS_CHR))
  MAPS <- MAPS %>% filter(!is.na(scid_p117))
  
#subset to remove '99s, subthresholds
  MAPS <- subset(MAPS, scid_p117 != 2) 
  MAPS <- subset(MAPS, scid_p117 != 99) 
  MAPS <- subset(MAPS, scid_p52 != 2) #no 99s, no need to filter the rest of way
  MAPS <- subset(MAPS, scid_p50 != 2) #sedative-anxiolytic
  MAPS <- subset(MAPS, scid_p54 != 2) #stimulant/cocaine
  MAPS <- subset(MAPS, scid_p56 != 2) #Opioids
  #PCP not included, no one with this dx in dataset
  MAPS <- subset(MAPS, scid_p60 != 2) #Hallucinogens
  MAPS <- subset(MAPS, scid_p60 !=99)
  MAPS <- subset(MAPS, scid_p62 != 2) #Inhalants
  MAPS <- subset(MAPS, scid_p64 != 2) #other drugs
  
  
  #group for all dx other than cannabis 
  MAPS$other_drug <- MAPS$scid_p50 + MAPS$scid_p54 + MAPS$scid_p56 + MAPS$scid_p60 + MAPS$scid_p62 + MAPS$scid_p64
  

            
  
}
    


recodeCTQ <- function() {
   #T2 CTQ includes all items
  #reverse scoring items items 2, 5, 7, 13, 19, 26, 28
  
  MAPS$T2_CTQ_2r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
  MAPS$T2_CTQ_5r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
  MAPS$T2_CTQ_7r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
  MAPS$T2_CTQ_13r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
  MAPS$T2_CTQ_19r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1")) 
  MAPS$T2_CTQ_26r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1")) 
  MAPS$T2_CTQ_28r <- mapvalues(as.numeric(MAPS$T2_CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1")) 
  
  #creating Emotional Abuse scale = Items 3, 8, 14, 18, 25*
    MAPS <- mutate(MAPS, T2_CTQ_EA_total= MAPS$T2_CTQ_3 +  MAPS$T2_CTQ_8 +  MAPS$T2_CTQ_14 +  MAPS$T2_CTQ_18 +  MAPS$T2_CTQ_25)
 
  #creating Physical Abuse scale = Items 9, 11, 12, 15, 17*
    MAPS <- mutate(MAPS, T2_CTQ_PA_total= T2_CTQ_9 + T2_CTQ_11 + T2_CTQ_12 + T2_CTQ_15 + T2_CTQ_17)

  #creating Sexual Abuse scale = Items 20, 21, 23, 24, 27*
    MAPS <- mutate(MAPS, T2_CTQ_SA_total= T2_CTQ_20 + T2_CTQ_21 + T2_CTQ_23 + T2_CTQ_24 + T2_CTQ_27)
 
  #creating Emotional Neglect scale = Items 5 (reverse scored), 7 (reverse scored), 13 (reverse scored), 19 (reverse scored), 28 (reverse scored)*
    MAPS <- mutate(MAPS, T2_CTQ_EN_total= as.numeric(T2_CTQ_5r) + as.numeric(T2_CTQ_7r) + as.numeric(T2_CTQ_13r) + as.numeric(T2_CTQ_19r) + as.numeric(T2_CTQ_28r))
  
  #creating Physical Neglect scale = Items 1, 2 (reverse scored), 4, 6, 26 (reverse scored)*
    MAPS <- mutate(MAPS, T2_CTQ_PN_total= T2_CTQ_1 + as.numeric(T2_CTQ_2r) + T2_CTQ_4 + T2_CTQ_6 + as.numeric(T2_CTQ_26r))
 
  #creating total score
    MAPS <- mutate(MAPS, T2_CTQ_total= T2_CTQ_EA_total + T2_CTQ_PA_total + T2_CTQ_SA_total+ T2_CTQ_EN_total + T2_CTQ_PN_total)
 

  #T1 CTQ 
  #T1 CTQ Omits Items: 4, 9, 11, 12, 15, 17, 20, 21, 23, 24, 25, 27****
  #Consequently, the Physical Abuse & Sexual Abuse subscales will be omitted also**
  #reverse scoring items items 2, 5, 7, 13, 19, 26, 28*
    MAPS$CTQ_2r <- mapvalues(as.numeric(MAPS$CTQ_2), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_5r <- mapvalues(as.numeric(MAPS$CTQ_5), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_7r <- mapvalues(as.numeric(MAPS$CTQ_7), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_13r <- mapvalues(as.numeric(MAPS$CTQ_13), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_19r <- mapvalues(as.numeric(MAPS$CTQ_19), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_26r <- mapvalues(as.numeric(MAPS$CTQ_26), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    MAPS$CTQ_28r <- mapvalues(as.numeric(MAPS$CTQ_28), from = c("1", "2", "3", "4", "5"), to = c("5", "4", "3", "2", "1"))
    #creating Emotional Abuse scale = Items 3, 8, 14, 18, (omit: 25)*
    MAPS <- mutate(MAPS, T1_CTQ_EA_total= CTQ_3 +  CTQ_8 +  CTQ_14 +  CTQ_18)

  #Physical Abuse scale = All items and scale omitted:( Items 9, 11, 12, 15, 17)*
  #Sexual Abuse scale = All items and scale omitted:( 20, 21, 23, 24, 27)*
  
  #creating Emotional Neglect scale = Items 5 (reverse scored), 7 (reverse scored), 13 (reverse scored), 19 (reverse scored), 28 (reverse scored)*
    MAPS <- mutate(MAPS, T1_CTQ_EN_total= as.numeric(CTQ_5r) + as.numeric(CTQ_7r) + as.numeric(CTQ_13r) + as.numeric(CTQ_19r) + as.numeric(CTQ_28r))
 
  #creating Physical Neglect scale = Items 1, 2 (reverse scored), 6, 26 (reverse scored), (OMIT item 4)*
    MAPS <- mutate(MAPS, T1_CTQ_PN_total= CTQ_1 + as.numeric(CTQ_2r) + CTQ_6 + as.numeric(CTQ_26r))
 
  #creating total score
    
    MAPS <- mutate(MAPS, T1_CTQ_total= T1_CTQ_EA_total + T1_CTQ_EN_total + T1_CTQ_PN_total)
    
  #remove 99s
    CTQmax <- 81
    MAPS <- subset(MAPS, T1_CTQ_total < CTQmax)

}

#subsetting MAPS for one to only include T1 and the other to only include T2, remove NAs

# get some supercontrols (zero scid dx lifetime)
MAPS$scid_total <- rowSums((MAPS %>% select(scid_p8,scid_p9,scid_p11:scid_p12,scid_p14:scid_p18,scid_p19:scid_p20,scid_p21:scid_p29,
                                            scid_p30:scid_p31,scid_p32:scid_p43,scid_p44:scid_p45,scid_p46:scid_p80,scid_p81:scid_p82,
                                            scid_p83:scid_p96,scid_p97:scid_p98,scid_p102:scid_p122)))

# create dataframe for clinical dx
sumof99 <- 283
dx <- MAPS %>% dplyr::filter(!is.na(scid_total)) %>% dplyr::filter(scid_total< sumof99)

# filters
sumOfNeverDiagnosed <- 94 # sum of all SCID items equals 94
control <- dx %>% data.frame() %>% filter((scid_total== sumOfNeverDiagnosed))

MAPS$control <- ifelse(MAPS$scid_total <= 94, 1, 0)

splitMAPS <- function() {
  MAPST1 <- MAPS %>% filter(!is.na(MAPS$T1_CTQ_total)) 
  MAPST2 <- MAPS %>% filter(!is.na(MAPS$T2_CTQ_total))
  MAPST1$other_drug <- MAPS$other_drug
  
 
  MAPST1$group_status <- as.factor(ifelse( MAPST1$PCHR == 4, "CHRPTSD",
                                           ifelse(MAPST1$PCHR == 3, "nCHRPTSD", 
                                                  ifelse(MAPST1$PCHR == 2, 'CHRnPTSD', 
                                                         ifelse(MAPST1$control == 1, 'control', 0)))))
  MAPST1$group_status <- as.numeric(MAPST1$group_status) > 0
  #recode for descriptive stat readability, 0s and 1s 
  MAPS$other_drug <- ifelse(MAPS$other_drug == 6, 0, 1)
  MAPS$scid_p52 <- ifelse(MAPS$scid_p52 == 1, 0, 1)
  MAPS$scid_p117 <- ifelse(MAPS$scid_p117 == 1, 0, 1)
  
  #create variables for moderation models
  MAPST1$CHRnPTSD <- MAPST1$SIPS_CHR == 1 & MAPST1$scid_p117 != 0
  MAPST1$nCHRPTSD<- MAPST1$SIPS_CHR != 1 & MAPST1$scid_p117 == 1
  
  



  
}

