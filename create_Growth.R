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
  
  state$TREE_GRM_COMPONENT$DIA_BEGIN_cm <- state$TREE_GRM_COMPONENT$DIA_BEGIN * 2.54
  state$TREE_GRM_COMPONENT$DIA_END_cm <- state$TREE_GRM_COMPONENT$DIA_END * 2.54
  
  
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
  calc_g_df <-  tree_df  %>% select(PLT_CN, pltID, PLOT, SUBP, CN, PREV_TRE_CN, INVYR, CYCLE, TREE, treeID, STATUSCD, SPCD, SPGRPCD, PREVDIA_cm, DIA_cm, DIAHTCD, HT_m, ACTUALHT_m, TPHa_UNADJ, CR, CCLCD, CLIGHTCD, DIACHECK, RECONCILECD, DAMLOC1, DAMTYP1, DAMSEV1) %>% arrange(treeID, INVYR)
  
  ## change CN colname to TREE_CN
  colnames(calc_g_df)[5] <- "TRE_CN"
  
  ## add state column
  calc_g_df$STATE <- state$SURVEY$STATEAB[1]
  
  ## merge in the tree_grm (TREE_GRM_COMPONENT) table 
  calc_g_df <-  dplyr::left_join(calc_g_df, tree_grm[,c("TRE_CN", "PREV_TRE_CN", "PLT_CN", "DIA_BEGIN_cm", "DIA_END_cm", "STEM_COMPONENT")],
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
    
  calc_g_df$AGR_DIA_mm <- ifelse(!is.na(calc_g_df$PREVDIA_cm), 
                                 ((calc_g_df$DIA_cm - calc_g_df$PREVDIA_cm)*10) / calc_g_df$REMPER, 
                                 ((calc_g_df$DIA_cm - calc_g_df$DIA_BEGIN_cm)*10) / calc_g_df$REMPER)
  
  calc_g_df$RGR_DIA_mm <- ifelse(!is.na(calc_g_df$PREVDIA_cm), 
                                 ((log(calc_g_df$DIA_cm) - log(calc_g_df$PREVDIA_cm))*10) / calc_g_df$REMPER, 
                                 ((log(calc_g_df$DIA_cm) - log(calc_g_df$DIA_BEGIN_cm))*10) / calc_g_df$REMPER)
  
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


################################################################################################################################
################################################################################################################################
## note: to add climate and climate change variables (calculated at the subplot scale) to this dataset run lines 324-394 of Caribbean_FIA_Climate_plotting_and_mapping.Rmd

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
Caribbean_G$subpID <- gsub('.{2}$', '', Caribbean_G$treeID)

### some lingering subpIDs have an extra "_" -- need to remove
Caribbean_G$subpID <- ifelse(stringr::str_sub(Caribbean_G$subpID,  -1) == "_", substr(Caribbean_G$subpID, 1, nchar(Caribbean_G$subpID) - 1), Caribbean_G$subpID)


### create the dummy variables with -9999 values -- to be overwritten by for loop
Caribbean_G$X_30yr_precip_normal_MONTHLY <- -9999
Caribbean_G$X_30yr_temp_normal           <- -9999

Caribbean_G$Avg_MaxTemp_CenInterval      <- -9999
Caribbean_G$Avg_ppt_CenInterval          <- -9999

Caribbean_G$diff_MaxTemp                 <- -9999
Caribbean_G$diff_ppt                     <- -9999


ymd_function  <- function(x) {year <- floor(x);
                              month <- floor((x - year) * 12);
                              day <- ((x - year) * 12 - month) * 30.42;
                              return(list(year = year, month = month, day = day))
                              }

## use a for loop to calculate climate difference

for (j in 1:dim(Caribbean_G)[1]) {

  ###  get the pltID and other important data from the Caribbean_G data frame
  plt <- Caribbean_G$subpID[j]
  mon <- ifelse(is.na(Caribbean_G[j,"MEASMON"]), 6, Caribbean_G[j,"MEASMON"])
  yr <-  ifelse(is.na(Caribbean_G[j,"MEASYEAR"]), Caribbean_G[j,"INVYR"], Caribbean_G[j,"MEASYEAR"])
  REMPER <- ifelse(is.na(Caribbean_G[j,"REMPER"]), 5.0, Caribbean_G[j,"REMPER"])


  ### pull the precip and max temp normal data from the PRISM calculations and add to the dataset
  ## take mean (na.rm = T), in cases where you might have double extractions from the baseline -- not sure why... but they exist.
  Caribbean_G$X_30yr_precip_normal_MONTHLY[j] <-  mean(clim_baseline[clim_baseline$subpID == Caribbean_G$subpID[j],"X30_yr_precip_normal_MONTHLY"], na.rm = T)
  Caribbean_G$X_30yr_temp_normal[j] <- mean(clim_baseline[clim_baseline$subpID == Caribbean_G$subpID[j],"X30_yr_temp_normal"], na.rm = T)

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
  Caribbean_G$Avg_MaxTemp_CenInterval[j] <- mean(tmax_data, na.rm = T)

  Caribbean_G$Avg_ppt_CenInterval[j] <- mean(ppt_data, na.rm = T)

  ### calculate difference between census interval and climate norma
  Caribbean_G$diff_MaxTemp[j] <- Caribbean_G$Avg_MaxTemp_CenInterval[j] - Caribbean_G$X_30yr_temp_normal[j]

  Caribbean_G$diff_ppt[j] <- Caribbean_G$Avg_ppt_CenInterval[j] -  Caribbean_G$X_30yr_precip_normal[j]

}

################################################################################################################################
################################################################################################################################
## note: to add canopy height and canopy height change variables (also calculated at the subplot scale) to this dataset run lines 1073-1175 of Li-Dar_CanopyCover_and_TreeHeight_Regressions.Rmd

### load the dataset remote sensing dataset 
library(XLConnect)

#### NEW DATASET
wb2 <- loadWorkbook("C:/Users/JamesHogan/Box/James.Hogan_Workspace/ORISE_PostDoc/data/LiDarData_from_Tom_R/lidar_7p5m_rad_nd_pwd.xlsx", password = "PuertoRico")

setMissingValue(wb2, value = -9999)  ## set the missing data value to -9999

lidar_data2 <- readWorksheet(wb2, "lidar_7p5m_rad_nd")

### deal with small negative values in Lidar canopy heights
lidar_data2[!is.na(lidar_data2$CanopyHeight2016) & lidar_data2$CanopyHeight2016 < 0,]$CanopyHeight2016 <- 0

lidar_data2[!is.na(lidar_data2$CanopyHeight2018) & lidar_data2$CanopyHeight2018 < 0,]$CanopyHeight2018 <- 0


# ## compute the mean plot height and add at the data frame
Caribbean_G <-  Caribbean_G %>% group_by(subpID, INVYR) %>% mutate(mean_subp_HT_m = mean(HT_m, na.rm = T)) %>% ungroup()

# for the last / most recent inventory (2016 - 2021;  cycles 6-7) -- replace mean_subp_HT_m with the Lidar data ---
# all plot measurements before SEPTEMBER 2017 gets the 2016/17 USGS Lidar height data
# everything after

## first do 2016 -- overwrites values with Lidar values
Caribbean_G[Caribbean_G$INVYR == 2016,]$mean_subp_HT_m <- lidar_data2[match(Caribbean_G[Caribbean_G$INVYR == 2016,]$subpID, lidar_data2$subpID),]$CanopyHeight2016

## then do 2017 -- jan - sept
Caribbean_G[Caribbean_G$INVYR == 2017 & Caribbean_G$MEASMON %in% c(1,2,3,4,5,6,7,8),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_G[Caribbean_G$INVYR == 2017 & Caribbean_G$MEASMON %in% c(1,2,3,4,5,6,7,8),]$subpID, lidar_data2$subpID),]$CanopyHeight2016

## 2017 - after sept
Caribbean_G[Caribbean_G$INVYR == 2017 & Caribbean_G$MEASMON %in% c(9,10,11,12),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_G[Caribbean_G$INVYR == 2017 & Caribbean_G$MEASMON %in% c(9,10,11,12),]$subpID, lidar_data2$subpID),]$CanopyHeight2018

## 2018, 2019, 2021
Caribbean_G[Caribbean_G$INVYR %in% c("2018", "2019", "2021"),]$mean_subp_HT_m <- lidar_data2[match(Caribbean_G[Caribbean_G$INVYR %in% c("2018", "2019", "2021"),]$subpID, lidar_data2$subpID),]$CanopyHeight2018

# CODE FOR CANOPY HEIGHT DIFFERENCE
## for loop-de-loop !!
xxx <- -9999

for (i in 1:length(Caribbean_G$pltID)) {
  
  xxx[i] <- unlist(if (Caribbean_G$INVYR[i] %in% c(2001,2002,2003,2004)) {
    0   #if in first census interval then canopy change is zero
  } else {
    ifelse(nrow(Caribbean_G[Caribbean_G$treeID %in% Caribbean_G$treeID[i] & Caribbean_G$INVYR %in% (Caribbean_G$INVYR[i] - 5), "mean_subp_HT_m"]) > 0,
           Caribbean_G[Caribbean_G$treeID %in% Caribbean_G$treeID[i] & Caribbean_G$INVYR %in% Caribbean_G$INVYR[i], "mean_subp_HT_m"] -
             Caribbean_G[Caribbean_G$treeID %in% Caribbean_G$treeID[i] & Caribbean_G$INVYR %in% (Caribbean_G$INVYR[i] - 5), "mean_subp_HT_m"], 0)
    # if in second interval and missing data, then canopy change is zero
  } )
}

Caribbean_G$Delta_CanopyHeight <- xxx

################################################################################################################################
################################################################################################################################
## save result
saveRDS(Caribbean_G , file = paste(save_object_path, "Caribbean_G.Rds", sep = ""))



