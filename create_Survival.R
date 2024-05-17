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

  
  state$TREE_GRM_COMPONENT$DIA_BEGIN_cm <- state$TREE_GRM_COMPONENT$DIA_BEGIN * 2.54
  state$TREE_GRM_COMPONENT$DIA_END_cm <- state$TREE_GRM_COMPONENT$DIA_END * 2.54
  
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
                        PREVCOND, RECONCILECD, MORTCD, DIA_cm, PREVDIA_cm, DIACHECK,  RECONCILECD,
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
    GRM_ref_df <- state$TREE_GRM_COMPONENT %>% select(TRE_CN, PREV_TRE_CN, DIA_BEGIN_cm, DIA_END_cm, PLT_CN, STEM_COMPONENT)
    
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
  ds$AGR_DIA_mm <- ifelse(!is.na(ds$PREVDIA_cm), 
                          ((ds$DIA_cm - ds$PREVDIA_cm)*10) / ds$REMPER, 
                          ((ds$DIA_cm - ds$DIA_BEGIN_cm)*10) / ds$REMPER)
  ds$RGR_DIA_mm <- ifelse(!is.na(ds$PREVDIA_cm), 
                          ((log(ds$DIA_cm) - log(ds$PREVDIA_cm))*10) / ds$REMPER, 
                          ((log(ds$DIA_cm) - log(ds$DIA_BEGIN_cm))*10) / ds$REMPER)
 
  ### add state prefix to Robject -- re-assign calc_df
  assign(paste(state_vec[j], "_ds", sep = ""), ds)
  
  ## saving
  saver <-  get(paste(state_vec[j], "_ds", sep = ""))
  saveRDS(saver, file = paste(save_object_path, state_vec[j], "_ds.Rds", sep = ""))
  
}  ## end state loop


### combine data -- PR and USVI 
Caribbean_ds <- rbind(PR_ds, VI_ds)


################################################################################################################################
################################################################################################################################
## note: to add climate and climate change variables (calculated at the subplot scale) to this dataset run lines 243-317 of Caribbean_FIA_Climate_plotting_and_mapping.Rmd

## read in the datasets
# ##### climate normals -- historic -- PRISM data 
clim_baseline <- readRDS("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/PRISM/clim_baseline.Rds")

### adjust ppt to monthly
### divide by 12 to get annual average to monthly average
clim_baseline$X30_yr_precip_normal_MONTHLY <- clim_baseline$X30_yr_precip_normal / 12

## timeseries of climate data -- TERRACLIM data
ppt_subplots <- readRDS("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/terraClimate/ppt_subplots_TerraClim_extraction.RDS")
tmax_subplots <- readRDS("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/terraClimate/tmax_subplots_TerraClim_extraction.RDS")

## read in the saved RDS data objects -- see "SUBP_temp_and_precip_change.Rmd"

tmax_subplots$subpID <-  as.factor(tmax_subplots$subpID)
ppt_subplots$subpID <-  as.factor(ppt_subplots$subpID)

## create a year-mon date variables in each dataset
tmax_subplots$year_mon <-  lubridate::parse_date_time(paste(tmax_subplots$month, tmax_subplots$year, sep = "-"), '%m-%Y')

ppt_subplots$year_mon <- lubridate::parse_date_time(paste(ppt_subplots$month, ppt_subplots$year, sep = "-"), '%m-%Y')

### need to create subpID for comparison to survival and growth datasets
Caribbean_ds$subpID <- gsub('.{2}$', '', Caribbean_ds$treeID)

### some lingering subpIDs have an extra "_" -- need to remove
Caribbean_ds$subpID <- ifelse(stringr::str_sub(Caribbean_ds$subpID,  -1) == "_", substr(Caribbean_ds$subpID, 1, nchar(Caribbean_ds$subpID) - 1), Caribbean_ds$subpID)


### create the dummy variables with -9999 values -- to be overwritten by for loop
Caribbean_ds$X_30yr_precip_normal_MONTHLY <- -9999
Caribbean_ds$X_30yr_temp_normal           <- -9999

Caribbean_ds$Avg_MaxTemp_CenInterval      <- -9999
Caribbean_ds$Avg_ppt_CenInterval          <- -9999

Caribbean_ds$diff_MaxTemp                 <- -9999
Caribbean_ds$diff_ppt                     <- -9999


ymd_function  <- function(x) {year <- floor(x);
                              month <- floor((x - year) * 12);
                              day <- ((x - year) * 12 - month) * 30.42;
                              return(list(year = year, month = month, day = day))
                              }

## use a for loop to calculate climate difference

for (j in 1:dim(Caribbean_ds)[1]) {

  ###  get the pltID and other important data from the Caribbean_ds data frame
  plt <- Caribbean_ds$subpID[j]
  mon <- ifelse(is.na(Caribbean_ds[j,"MEASMON"]), 6, Caribbean_ds[j,"MEASMON"])
  yr <-  ifelse(is.na(Caribbean_ds[j,"MEASYEAR"]), Caribbean_ds[j,"INVYR"], Caribbean_ds[j,"MEASYEAR"])
  REMPER <- ifelse(is.na(Caribbean_ds[j,"REMPER"]), 5.0, Caribbean_ds[j,"REMPER"])


  ### pull the precip and max temp normal data from the PRISM calculations and add to the dataset
  ## take mean (na.rm = T), in cases where you might have double extractions from the baseline -- not sure why... but they exist.
  Caribbean_ds$X_30yr_precip_normal_MONTHLY[j] <-  mean(clim_baseline[clim_baseline$subpID == Caribbean_ds$subpID[j],"X30_yr_precip_normal_MONTHLY"], na.rm = T)
  Caribbean_ds$X_30yr_temp_normal[j] <- mean(clim_baseline[clim_baseline$subpID == Caribbean_ds$subpID[j],"X30_yr_temp_normal"], na.rm = T)

  #### set up some things to help with the sub-setting
  #### END Date year_mon  is the measurement time at T2
  ENDMEAS_date <-  lubridate::parse_date_time(paste(mon, yr , sep = "-"), '%m-%Y')

  ## dealing with decimal REMPERS
  x <- ymd_function(REMPER)

  #### START Date year_mon is the END Date year_mon minus REMPER
  STARTMEAS_date <-  ENDMEAS_date - lubridate::period(c(x$year, x$month, round(x$day)), c("year", "months", "days"))

  RANGE_year_mon <- seq(from = lubridate::floor_date(STARTMEAS_date , unit = "months"), to = ENDMEAS_date, by = "month")  ## range

  ### subsetting
  tmax_data <- tmax_subplots[tmax_subplots$subpID == plt & tmax_subplots$year_mon %in% RANGE_year_mon, "tmax"]

  ppt_data <- ppt_subplots[ppt_subplots$subpID == plt & ppt_subplots$year_mon %in% RANGE_year_mon, "ppt"]

  ### get the average of the values over the census interval
  Caribbean_ds$Avg_MaxTemp_CenInterval[j] <- mean(tmax_data$tmax, na.rm = T)

  Caribbean_ds$Avg_ppt_CenInterval[j] <- mean(ppt_data$ppt, na.rm = T)

  ### calculate difference between census interval and climate norma
  Caribbean_ds$diff_MaxTemp[j] <- Caribbean_ds$Avg_MaxTemp_CenInterval[j] - Caribbean_ds$X_30yr_temp_normal[j]

   Caribbean_ds$diff_ppt[j] <- Caribbean_ds$Avg_ppt_CenInterval[j] -  Caribbean_ds$X_30yr_precip_normal[j]

}


################################################################################################################################
################################################################################################################################
## note: to add canopy height and canopy height change variables (also calculated at the subplot scale) to this dataset run lines 1073-1175 of Li-Dar_CanopyCover_and_TreeHeight_Regressions.Rmd

### load the dataset remote sensing dataset 
library(XLConnect); library(dplyr)

#### NEW DATASET
wb2 <- loadWorkbook("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/LiDarData_from_Tom_R/lidar_7p5m_rad_nd_pwd.xlsx", password = "PuertoRico")

setMissingValue(wb2, value = -9999)  ## set the missing data value to -9999

lidar_data2 <- readWorksheet(wb2, "lidar_7p5m_rad_nd")

### deal with small negative values in Lidar canopy heights
lidar_data2[!is.na(lidar_data2$CanopyHeight2016) & lidar_data2$CanopyHeight2016 < 0,]$CanopyHeight2016 <- 0

lidar_data2[!is.na(lidar_data2$CanopyHeight2018) & lidar_data2$CanopyHeight2018 < 0,]$CanopyHeight2018 <- 0


## compute the mean plot height and add at the data frame
Caribbean_ds <-  Caribbean_ds %>% group_by(subpID, INVYR) %>% mutate(mean_subp_HT_m = mean(HT_m, na.rm = T)) %>% ungroup()


## for the last / most recent inventory (2016 - 2021;  cycles 6-7) -- replace mean_subp_HT_m with the Lidar data ---
## all plot measurements before SEPTEMBER 2017 gets the 2016/17 USGS Lidar height data
## everything after

######## SURVIVAL DATASET
## first do 2016 -- overwrites values with Lidar values
Caribbean_ds[Caribbean_ds$INVYR == 2016,]$mean_subp_HT_m <- lidar_data2[match(Caribbean_ds[Caribbean_ds$INVYR == 2016,]$subpID, lidar_data2$subpID),]$CanopyHeight2016

## then do 2017 -- jan - sept
Caribbean_ds[Caribbean_ds$INVYR == 2017 & Caribbean_ds$MEASMON %in% c(1,2,3,4,5,6,7,8),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_ds[Caribbean_ds$INVYR == 2017 & Caribbean_ds$MEASMON %in% c(1,2,3,4,5,6,7,8),]$subpID, lidar_data2$subpID),]$CanopyHeight2016

## 2017 - after sept
Caribbean_ds[Caribbean_ds$INVYR == 2017 & Caribbean_ds$MEASMON %in% c(9,10,11,12),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_ds[Caribbean_ds$INVYR == 2017 & Caribbean_ds$MEASMON %in% c(9,10,11,12),]$subpID, lidar_data2$subpID),]$CanopyHeight2018

## 2018, 2019, 2021
Caribbean_ds[Caribbean_ds$INVYR %in% c("2018", "2019", "2021"),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_ds[Caribbean_ds$INVYR %in% c("2018", "2019", "2021"),]$subpID, lidar_data2$subpID),]$CanopyHeight2018


# CODE FOR CANOPY HEIGHT DIFFERENCE

## for loop-de-loop !!

Caribbean_ds$Delta_CanopyHeight <- -9999

###
for (i in 1:length(Caribbean_ds$pltID)) {

  Caribbean_ds[i,]$Delta_CanopyHeight <- unlist(if (Caribbean_ds$INVYR[i] %in% c(2001,2002,2003,2004)) {
                                               0   #if in first census interval then canopy change is zero
                                        } else {
                                            ifelse(nrow(Caribbean_ds[Caribbean_ds$treeID %in% Caribbean_ds$treeID[i] & Caribbean_ds$INVYR %in% (Caribbean_ds$INVYR[i] - 5), "mean_subp_HT_m"]) > 0,
                                                        Caribbean_ds[Caribbean_ds$treeID %in% Caribbean_ds$treeID[i] & Caribbean_ds$INVYR %in% Caribbean_ds$INVYR[i], "mean_subp_HT_m"] -
                                                        Caribbean_ds[Caribbean_ds$treeID %in% Caribbean_ds$treeID[i] & Caribbean_ds$INVYR %in% (Caribbean_ds$INVYR[i] - 5), "mean_subp_HT_m"], 0)
                                                        # if in second interval and missing data, then canopy change is zero
      } )
}


################################################################################################################################
################################################################################################################################
## save result
saveRDS(Caribbean_ds, file = paste(save_object_path, "Caribbean_ds.Rds", sep = ""))