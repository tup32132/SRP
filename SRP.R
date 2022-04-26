#Research Project MAPS

#Package loading
library(haven)
library(dplyr)
library(lavaan)
library(psych)

#Data loading
MAPS <- read_sav("S:/Ellman_Group/MAPstudy/Data/Master Data/CurrentMasterFile/MAPS_Multisite_Master_Data_Set_04.22.2022.sav")

#Clean it up
      #remove NAs
        MAPS <- MAPS %>% filter(!is.na(SIPS_CHR))
        MAPS <- MAPS %>% filter(!is.na(DUF_17))
        MAPS<- MAPS %>% filter(!is.na(scid_p22))
        MAPS <- MAPS %>% filter(!is.na(scid_p24))
        
      #CTQ 
        CTQ <- t2 %>% as_tibble %>% select(starts_with("T2_CTQ"))
        alpha(CTQ, cumulative = TRUE, check.keys = TRUE)
        
      #Get counts for each group
        CD <- MAPS %>% filter( SIPS_CHR == 1 & (scid_p21 == 3 | scid_p23 == 3))
        CnD <- MAPS %>% filter(SIPS_CHR == 1 & (scid_p21 != 3 | scid_p23 != 3))
        nCD <- MAPS %>% filter(SIPS_CHR != 1 & (scid_p21 == 3 | scid_p23 == 3))

      #get some supercontrols
        MAPS$scid_total <- rowSums((MAPS %>% select(scid_p8,scid_p9,scid_p11:scid_p12,scid_p14:scid_p18,scid_p19:scid_p20,scid_p21:scid_p29,
                                          scid_p30:scid_p31,scid_p32:scid_p43,scid_p44:scid_p45,scid_p46:scid_p80,scid_p81:scid_p82,
                                          scid_p83:scid_p96,scid_p97:scid_p98,scid_p102:scid_p122)))
        
        dx <- MAPS %>% dplyr::filter(!is.na(scid_total)) %>% dplyr::filter(scid_total< 283)
        
        control <- dx %>% data.frame() %>% filter((scid_total== 94))
        

        


