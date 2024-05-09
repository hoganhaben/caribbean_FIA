### create Tree survival data set

## FIA Major Outlying Islands and other "states"

## JAH  
## 08.15.2023

library(tidyverse); library(rFIA)

## read in the data 

## file name housekeeping
data_path <- "C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/FIA_DB_08.18.2023/"
save_object_path <- "C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/R_CaribbeanFIA/Survival_dataset/Rdata/"

## state_vec: vector of state abbreviations to loop over
# state_vec <- c("AS", "FM", "GU", "HI", "MP", "PR",  "PW", "VI")
state_vec <- c("PR", "VI")  ## if just working with PR data


## looping over the FIA_db data for each "state / locality"
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
  
  
  ## convert units -- diameters inches to centimeters and heights from ft to m
  state$TREE$DIA_cm <- state$TREE$DIA * 2.54
  state$TREE$PREVDIA_cm <- state$TREE$PREVDIA * 2.54
  
  state$TREE$HT_m <- state$TREE$HT / 3.2808
  state$TREE$ACTUALHT_m <- state$TREE$ACTUALHT / 3.2808
  state$TREE$BOLEHT_m <- state$TREE$BOLEHT / 3.2808
  
 
  ## convert units - conversion of per acre to per hectare
  state$TREE$TPHa_UNADJ <-  state$TREE$TPA_UNADJ * 2.4710538147   ##TPA base level TPA
  state$TREE$TPHaGROW_UNADJ <-  state$TREE$TPAGROW_UNADJ * 2.4710538147   ##TPA base level TPA

  
  ##  create pltID - unique plot identifier string
  state$TREE$pltID <- stringr::str_c(state$TREE$UNITCD, state$TREE$STATECD, state$TREE$COUNTYCD, state$TREE$PLOT, sep = '_')
  state$PLOT$pltID <- stringr::str_c(state$PLOT$UNITCD, state$PLOT$STATECD, state$PLOT$COUNTYCD, state$PLOT$PLOT, sep = '_')
  state$COND$pltID <- stringr::str_c(state$COND$UNITCD, state$COND$STATECD, state$COND$COUNTYCD, state$COND$PLOT, sep = '_')
  
  ## create treeID
  state$TREE$treeID <- stringr::str_c(state$TREE$UNITCD, state$TREE$STATECD, state$TREE$COUNTYCD, state$TREE$PLOT, state$TREE$SUBP, state$TREE$TREE, sep = '_')
  
  
  ## make placeholder datafame - each row with a unique record for each plot - t1 to t2
  ## filters out plots with before 1995(implementation of FIA 2.0), year = 9999 (non-inventories), 
  
  plot_ref_df <- state$PLOT %>% filter(INVYR > 1995 & INVYR != 9999) %>% 
                 select(pltID, CN, SRV_CN, PREV_PLT_CN, MEASYEAR, MEASMON, INVYR, REMPER, LAT, LON, ELEV) %>% 
                 arrange(pltID, INVYR)
  
  ## rename CN as PLT_CN
  colnames(plot_ref_df)[2] <- "PLT_CN"
  
  ## caluculate MEASTIME
  plot_ref_df$MEASTIME <-  plot_ref_df$MEASYEAR + ((plot_ref_df$MEASMON - 0.5) / 12)
  
  ## add state column
  plot_ref_df$STATE <- state$SURVEY$STATEAB[1]
  
  #################################################
  ###### TREE DATA ################################
  ### tree_ref_df - from TREE table
  tree_ref_df <- state$TREE %>% 
                 select(pltID, treeID, CN, PLT_CN, INVYR, CYCLE, PREV_TRE_CN, TREE, SPCD, SPGRPCD, STATUSCD, 
                        PREVCOND, RECONCILECD, MORTCD, DIA_cm, PREVDIA_cm, DIACHECK,  
                        TPHa_UNADJ, TPHaGROW_UNADJ, HT_m, ACTUALHT_m, BOLEHT_m, CR, CCLCD, 
                        CPOSCD, CLIGHTCD, CDENCD, DAMLOC1, DAMTYP1, DAMSEV1) %>% 
                 arrange(pltID, INVYR, TREE)
  
  ## rename CN as TRE_CN
  colnames(tree_ref_df)[3] <- "TRE_CN"

  ## create placeholder columns for PREV variables -- when they are missing from current census -- 
  tree_ref_df$PREV_CR <-  -9999
  tree_ref_df$PREV_CCLCD <-  -9999
  tree_ref_df$PREV_CLIGHTCD <-  -9999
  tree_ref_df$CI  <-  -9999
  
  #### need to replace CR with previous value if possible
  for (t in 1:length(tree_ref_df$TRE_CN)) {
    if (is.na(tree_ref_df[t,]$CR)) {
      tree_ref_df[t,]$PREV_CR <- tree_ref_df[match(tree_ref_df[t,]$PREV_TRE_CN, tree_ref_df$TRE_CN),]$CR
      } else if (!is.na(tree_ref_df[t,]$CR)) {
        tree_ref_df[t,]$PREV_CR <-  tree_ref_df[t,]$CR
        } else {
          tree_ref_df[t,]$PREV_CR <-  NA
          }
  
    
    ### same for CCLCD
    if (is.na(tree_ref_df[t,]$CCLCD)) {
      tree_ref_df[t,]$PREV_CCLCD <- tree_ref_df[match(tree_ref_df[t,]$PREV_TRE_CN, tree_ref_df$TRE_CN),]$CCLCD
      } else if (!is.na(tree_ref_df[t,]$CCLCD)) {
        tree_ref_df[t,]$PREV_CCLCD <-  tree_ref_df[t,]$CCLCD
        } else {
          tree_ref_df[t,]$PREV_CCLCD <-  NA
          }
  
    ### same for CLIGHTCD
    if (is.na(tree_ref_df[t,]$CLIGHTCD)) {
      tree_ref_df[t,]$PREV_CLIGHTCD <- tree_ref_df[match(tree_ref_df[t,]$PREV_TRE_CN, tree_ref_df$TRE_CN),]$CLIGHTCD
      } else if (!is.na(tree_ref_df[t,]$CLIGHTCD)) {
        tree_ref_df[t,]$PREV_CLIGHTCD <-  tree_ref_df[t,]$CLIGHTCD
        } else {
          tree_ref_df[t,]$PREV_CLIGHTCD <-  NA
          }
  
  }
  
  
  if ("TREE_GRM_COMPONENT" %in% ls(state)) {
    
    ### GRM_ref_df 
    GRM_ref_df <- state$TREE_GRM_COMPONENT %>% select(TRE_CN, PREV_TRE_CN, PLT_CN, STEM_COMPONENT)
    
    ### merge tree_ref_df with GRM_ref_df
    merged_tree_df <- left_join(tree_ref_df, GRM_ref_df, by = c("TRE_CN", "PREV_TRE_CN", "PLT_CN"))  %>% 
      arrange(INVYR, TREE)
  
  } else {
    
    merged_tree_df <- tree_ref_df
  
  }
    
    
  #################################################
  ###### CONDITION, PLOT AND SURVEY DATA ##########
  ### Condition Table
  cond_ref_df <- state$COND %>% select(pltID, CN, PLT_CN, INVYR, PLOT, CONDID, COND_STATUS_CD, 
                                       CONDPROP_UNADJ, OWNCD, FORINDCD, FORTYPCD, STDSZCD, FLDSZCD, SITECLCD, 
                                       STDORGCD, SLOPE, ASPECT, PHYSCLCD, GSSTKCD, DSTRBCD1, DSTRBYR1, 
                                       DSTRBCD2, DSTRBYR2, TRTCD1, TRTYR1, TRTCD2, TRTYR2, BALIVE, FLDAGE)
    
  ## rename CN as COND_CN
  colnames(cond_ref_df)[2] <- "COND_CN"  
  
  ## make cond_ref_clean which assigns a single condition to each plot --
  ## it rbinds -- row binds the conditions from plots with conditions >= 0.95 with the conditions form the max COND_PROP_UNADJ for plots with conditions < 0.95.
  cond_ref_df_clean <-  rbind(cond_ref_df[cond_ref_df$CONDPROP_UNADJ >= 0.95,], 
                              cond_ref_df[cond_ref_df$CONDPROP_UNADJ < 0.95,] %>% 
                                group_by(pltID, PLT_CN, INVYR) %>% slice(which.max(CONDPROP_UNADJ)))
  
  ### need a second subsetting step here to remove the COND_PROP_UNADJ which are less than 0.05
  cond_ref_df_clean <- cond_ref_df_clean[!cond_ref_df_clean$CONDPROP_UNADJ < 0.05,]
  
  ### Survey Table 
  survey_ref_df <-  state$SURVEY %>% select(CN, INVYR, P3_OZONE_IND, ANN_INVENTORY, CYCLE)
  
  ## rename CN as COND_CN
  colnames(survey_ref_df)[1] <- "SRV_CN"  
  
  ### join up the result
  ### step 1: merge condition and plot tables  -- one to one matching
  cnd_plt <- merge(cond_ref_df_clean, plot_ref_df, by = c("pltID", "PLT_CN", "INVYR"))
  
  ## step 2: merge that table to the survey table -- many to one matching 
  cnd_plt_srv <- merge(cnd_plt, survey_ref_df,  by = c("SRV_CN", "INVYR")) %>% 
                 arrange(pltID, PLT_CN, INVYR) %>% 
                 select(pltID, PLT_CN, INVYR, everything())
  
  ## step 3: left join the tree data to result of step 2 (above)
  ds <- left_join(merged_tree_df, cnd_plt_srv, by = c("pltID", "PLT_CN", "INVYR", "CYCLE"), 
                  relationship = "many-to-one")

  ## LASTLY:  CALCULATE CROWN LENGTH ==== Height X compacted crown ratio (from T1 measurement ) / 100
  ## add 1 number to not have zeros -- log(0) is - Inf
  ds$CROWN_LENGTH <- (ds$HT_m * (ds$PREV_CR)/100) + 1  
  
  ## calculate growth of trees in tree_ref_df
  ds$AGR_DIA_mm <- ((ds$DIA_cm - ds$PREVDIA_cm)*10) / ds$REMPER
  ds$RGR_DIA_mm <- ((log(ds$DIA_cm) - log(ds$PREVDIA_cm))*10) / ds$REMPER
  
  
  ### add state prefix to Robject -- re-assign calc_df
  assign(paste(state_vec[j], "_ds", sep = ""), ds)
  
  ## saving
  saver <-  get(paste(state_vec[j], "_ds", sep = ""))
  saveRDS(saver, file = paste(save_object_path, state_vec[j], "_ds.Rds", sep = ""))
  
}  ## end state loop


### combine data -- PR and USVI 
Caribbean_ds <- rbind(PR_ds, VI_ds)
## save result
saveRDS(Caribbean_ds, file = paste(save_object_path, "Caribbean_ds.Rds", sep = ""))
