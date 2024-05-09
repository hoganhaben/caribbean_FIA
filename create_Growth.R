### create Tree Growth dataset

## FIA Major Outlying Islands and other "states", but mainly used for Puerto Rico and the Virgin Islands

## JAH  
## 08.29.2023
## revised 5.7.2024


## Based on FIA project with Lichstein Lab (but adapted to calculate growth for individual trees) -- see https://github.com/hoganhaben/FIA-forest-dynamics/blob/main/calculate_Growth.R

library(rFIA); library(dplyr); library(BIOMASS)

### directory for BIOMASS CACHE 
createCache(path = "C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/R_CaribbeanFIA/cache.BIOMASS")

## read in the data 

## read in FIA Species Table -- used downstream
library(readr)
FIA_sp_tbl <- read_csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/FIA_DB docs/Master Species_ver_2-0__9_2022_final_FAMILIES.csv")

## file name housekeeping
data_path <- "C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_DB_08.18.2023/"
save_object_path <- "C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/R_CaribbeanFIA/Growth_dataset/Rdata/"

## state_vec: vector of state abbreviations to loop over
#state_vec <- c("AS", "FM", "GU", "HI", "MP", "PR",  "PW", "VI")

state_vec <- c("PR", "VI")  ## if just working with PR data


## looping over the 48 continental US states
for (j in 1:length(state_vec)) {
  
  ## read in the data
  state <- readFIA(paste(data_path, state_vec[j], sep = ""))
  
  if (state_vec[j] == "PR") {
    
    ### read in the new 2021 data
    newCOND <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Cond_72_2021.csv")
    newPLOT <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Plots_72_2021.csv")
    newSUBPLOT <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Subplot_72_2021.csv")
    newTREE <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Tree_72_2021.csv")
    newTREE_GRM_BEGIN <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Tree_Grm_Begin_72_2021.csv")
    newTREE_GRM_COMPONENT <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Tree_Grm_Component_72_2021.csv")
    newTREE_GRM_MID <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Tree_Grm_Midpt_72_2021.csv")
    # new_REGIONALB <- read.csv("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_PR_2021data/IITF_ColaborationFolder_External/Tree_Regional_Biomass_72_2021.csv")
    
    ## combine with the existing FIA data 
    state$COND <- gtools::smartbind(state$COND, newCOND)
    state$PLOT <- gtools::smartbind(state$PLOT, newPLOT)
    state$SUBPLOT <- gtools::smartbind(state$SUBPLOT, newSUBPLOT)
    state$TREE <- gtools::smartbind(state$TREE, newTREE)
    state$TREE_GRM_BEGIN <- gtools::smartbind(state$TREE_GRM_BEGIN, newTREE_GRM_BEGIN)
    state$TREE_GRM_COMPONENT <- gtools::smartbind(state$TREE_GRM_COMPONENT, newTREE_GRM_COMPONENT)
    state$TREE_GRM_MIDPT <- gtools::smartbind(state$TREE_GRM_MIDPT, newTREE_GRM_MID)
    
  }
  
  ## convert units -- lbs dry biomass to Mg (metric tons)
  state$TREE$DRYBIO_AG_Mg <- state$TREE$DRYBIO_AG * 0.00045359237
  
  ## convert units - AGB_CARBON_AG)
  state$TREE$CARBON_AG_Mg <- (state$TREE$CARBON_AG) * 0.00045359237
  
  ## convert units - conversion of per acre to per hectare
  state$TREE$TPHa_UNADJ <-  state$TREE$TPA_UNADJ * 2.4710538147   ##TPA base level TPA
  
  ## convert units - tree diameters and heights to metric
  state$TREE$DIA_cm <- state$TREE$DIA * 2.54
  state$TREE$PREVDIA_cm <- state$TREE$PREVDIA * 2.54
  state$TREE$HT_m  <- state$TREE$HT / 3.28084
  state$TREE$ACTUALHT_m  <- state$TREE$ACTUALHT / 3.28084
  
   state$PLOT$ELEV_m <- state$PLOT$ELEV / 3.28084
  
  ##  create pltID - unique plot identifier string
  state$TREE$pltID <-  stringr::str_c(state$TREE$UNITCD, state$TREE$STATECD, state$TREE$COUNTYCD, state$TREE$PLOT, sep = '_')
  state$PLOT$pltID <-  stringr::str_c(state$PLOT$UNITCD, state$PLOT$STATECD, state$PLOT$COUNTYCD, state$PLOT$PLOT, sep = '_')
  
  ##  create treID - unique tree identifier string -- this identifier assumes that tree numbers do not repeat within plots
  state$TREE$treeID <-  stringr::str_c(state$TREE$UNITCD, state$TREE$STATECD, state$TREE$COUNTYCD, state$TREE$PLOT, state$TREE$SUBP, state$TREE$TREE, sep = '_')
  
  ## for each state -- get the FIA plot and tree tables as R dataframes
  tree_df <- state$TREE
  
  plot_df <- state$PLOT
  colnames(plot_df)[1] <- "PLT_CN"   ## change CN to PLT_CN
  
  tree_grm <- state$TREE_GRM_COMPONENT
  
  ## STEP 2 - Individual tree growth calculations
  
  ## make placeholder data fame - each row with a single tree record 
  ## selects 
  calc_g_df <-  tree_df  %>% select(PLT_CN, pltID, PLOT, SUBP, CN, PREV_TRE_CN, INVYR, CYCLE, TREE, treeID, STATUSCD, SPCD, SPGRPCD, PREVDIA_cm, DIA_cm, DIAHTCD, HT_m, ACTUALHT_m, TPHa_UNADJ, CR, CCLCD, CLIGHTCD, DIACHECK, DAMLOC1, DAMTYP1, DAMSEV1) %>% arrange(treeID, INVYR)
  
  ## change CN colname to TREE_CN
  colnames(calc_g_df)[5] <- "TRE_CN"
  
  ## add state column
  calc_g_df$STATE <- state$SURVEY$STATEAB[1]
  
  ## merge in the tree_grm (TREE_GRM_COMPONENT) table 
  calc_g_df <-  dplyr::left_join(calc_g_df, tree_grm[,c("TRE_CN", "PREV_TRE_CN", "PLT_CN", "STEM_COMPONENT")],
                                 by = c("TRE_CN", "PREV_TRE_CN", "PLT_CN"))
  
  ## merge in the plot table -- need this for REMPER and MEASTIMES etc. 
  calc_g_df <-  dplyr::left_join(calc_g_df, plot_df[, c("pltID", "PLT_CN", "SRV_CN", "PREV_PLT_CN", "INVYR", "MEASYEAR", "MEASMON", "MEASDAY", "REMPER", "LAT", "LON", "ELEV_m")],
                                by = c("pltID", "PLT_CN", "INVYR"))
  
  ## Create MEASTIME Variable -- Decimal year from MEASYEAR, MEASMON and MEASDAY
  calc_g_df$MEASTIME <- calc_g_df$MEASYEAR + ((calc_g_df$MEASMON + (calc_g_df$MEASDAY/30.437)) / 12)
  
  #####################################################################################################
  
  ## TREE CALCULATIONS -- vectorized
  
  #####################################################################################################
  
  # clean species records 
  ### calculate the number of UNIQUE SPECIES IDs per treeID
  calc_g_df <- calc_g_df %>% group_by(treeID) %>%  mutate(n_unique_SPCD = length(unique(SPCD))) %>% ungroup()
  ### take the most frequent SPCD 
  calc_g_df <- calc_g_df %>% group_by(treeID) %>%  mutate(SPCD_cleaned = names(which.max(table(SPCD)))) %>% ungroup()
  
  calc_g_df$SPCD_cleaned <-  as.numeric(calc_g_df$SPCD_cleaned)
  
  ### make as dataframe -- to get rid of tibble garbage
  calc_g_df <- as.data.frame(calc_g_df)
  
  #####################################################################################################
  
  if (j == "PR") {
  
    ### need to fix two minor issues with the SPCD_cleaned
    # see: https://www.fs.usda.gov/srsfia/data_acquisition/July%20SRS%20FINAL%207.0%20LEGAL%20reading%20version.pdf
  
    ### 1.  CODE 859 used to Citharexylum fructicosum, wich is an old synonym for Citharexylum spinosum
    calc_g_df[calc_g_df$SPCD_cleaned == 859,]$SPCD_cleaned <- 6567
  
    ### 2. Code 6106 (Anacardium sp.) needs to change to 6107 A. occidentale
    calc_g_df[calc_g_df$SPCD_cleaned == 6106,]$SPCD_cleaned <- 6107
  
  }
  
  #####################################################################################################
  
  ### assign family, genus and spcies -- from FIA species table
  calc_g_df$family <- FIA_sp_tbl[match(calc_g_df$SPCD_cleaned, FIA_sp_tbl$`FIA Code`),]$Family
  calc_g_df$genus <- FIA_sp_tbl[match(calc_g_df$SPCD_cleaned, FIA_sp_tbl$`FIA Code`),]$Genus
  calc_g_df$species <- FIA_sp_tbl[match(calc_g_df$SPCD_cleaned, FIA_sp_tbl$`FIA Code`),]$Species
  
  ## clean values for 998, 999, 299
  calc_g_df[calc_g_df$SPCD_cleaned %in% c(299, 998, 999),]$family <- "NA"
  calc_g_df[calc_g_df$SPCD_cleaned %in% c(299, 998, 999),]$genus <- "NA"
  calc_g_df[calc_g_df$SPCD_cleaned %in% c(299, 998, 999),]$species <- "NA"
  

  ######################################################################
    
  calc_g_df$AGR_DIA_mm <- ((calc_g_df$DIA_cm - calc_g_df$PREVDIA_cm)*10) / calc_g_df$REMPER
  calc_g_df$RGR_DIA_mm <- ((log(calc_g_df$DIA_cm) - log(calc_g_df$PREVDIA_cm))*10) / calc_g_df$REMPER
  
  #####################################################################################################
  #####################################################################################################
  
  ### FOR MISSING FIA TREE DATA
  calc_g_df$PREV_CR <-                  -9999
  calc_g_df$PREV_CCLCD <-               -9999
  calc_g_df$PREV_CLIGHTCD <-            -9999
  
  
  ##################################################################################

  ### GET PRVIOUS MEASUREMENTS AND CALCULATE AGR AND RGR
  
  for (t in 1:length(calc_g_df$TREE)) {
   
    ## this is for getting the previous CR (crown ratio) -- if it is missing at time t go back to t-1 and get it  (PREV_CR)
    if (is.na(calc_g_df[t,]$CR)) {  
      calc_g_df[t,]$PREV_CR <- calc_g_df[match(calc_g_df[t,]$PREV_TRE_CN, calc_g_df$TRE_CN),]$CR
      } else if (!is.na(calc_g_df[t,]$CR)) {
        calc_g_df[t,]$PREV_CR <-  calc_g_df[t,]$CR
        } else {
          calc_g_df[t,]$PREV_CR <-  NA
          }
    
    
    ### same for CCLCD
    if (is.na(calc_g_df[t,]$CCLCD)) {
      calc_g_df[t,]$PREV_CCLCD <- calc_g_df[match(calc_g_df[t,]$PREV_TRE_CN, calc_g_df$TRE_CN),]$CCLCD
      } else if (!is.na(calc_g_df[t,]$CCLCD)) {
        calc_g_df[t,]$PREV_CCLCD <-  calc_g_df[t,]$CCLCD
        } else {
          calc_g_df[t,]$PREV_CCLCD <-  NA
          }
    
    ### same for CLIGHTCD
    if (is.na(calc_g_df[t,]$CLIGHTCD)) {
      calc_g_df[t,]$PREV_CLIGHTCD <- calc_g_df[match(calc_g_df[t,]$PREV_TRE_CN, calc_g_df$TRE_CN),]$CLIGHTCD
      } else if (!is.na(calc_g_df[t,]$CLIGHTCD)) {
        calc_g_df[t,]$PREV_CLIGHTCD <-  calc_g_df[t,]$CLIGHTCD
        } else {
          calc_g_df[t,]$PREV_CLIGHTCD <-  NA
          }
    
  }
  
  ### add state prefix to Robject -- re-assign calc_df
  assign(paste(state_vec[j], "_calc_g_df", sep = ""), calc_g_df)
  
  ## save R object - by state
  saver <-  get(paste(state_vec[j], "_calc_g_df", sep = ""))
  saveRDS(saver, file = paste(save_object_path, state_vec[j], "_calc_G.Rds", sep = ""))
  
} ## end  state loop


Caribbean_G <- rbind(PR_calc_g_df, VI_calc_g_df)

## save result
saveRDS(Caribbean_G , file = paste(save_object_path, "Caribbean_G.Rds", sep = ""))


