#07/01/2026

#This function adds a combined flag taking into account monitoring coverage and reliability estimations. The flag is the HEX code for a color on a bivariate palette, that can be used for plotting.
#We also add an additional descriptive column "flag_description" 
#The function is designed to work on the output from the function "reliability_estimation_p.R", and makes use of the "bpue_estimates" object 
#It take "cols" as an argument, to recover monitoring coverage from "bpue2" (output from "annotate_bpue.R") at the correct aggregation level. (annotate_bpue.R also takes the cols argument and can be run a different aggregation levels). 
#Currently, the reliability test function is only implemented at an ecoregion / métier level 4 scale (not métier level 5 or ICES area), so by default, cols = c("ecoregion", "metierl4")

add_color_flag <- function(bpues_estimates, bpue2, cols = c("ecoregion", "metierl4")) {
  #Recovering the monitoring coverage info annotated in bpue2, at the same level of aggregation ("cols") - most of the time, cols=c("ecoregion", "metierl4"))
  
  #This first part deals with difference in column names since we're working of an object in which column names were altered for printing. Not very clean, we might want to improve this in the future ?
  #There might be a faster way of doing this.
  
  original_colnames <- colnames(bpues_estimates)
  
  names_changes <- c(
    Ecoregion = "ecoregion",
    `metier L4`  = "metierl4",
    `metier L5`  = "metierl5",
    `ICES area`  = "areacode" #Not sure how ICES area column is called in table_print for estimates at the area scale. Is it implemented yet ?
  )
  
  # Compute which of the internal names are requested and present
  requested <- intersect(names(bpues_estimates), names(names_changes))
  to_rename <- requested[names_changes[requested] %in% cols]
  
  # Rename in place (only if there’s something to rename)
  
  if (length(to_rename)) {
    setnames(bpues_estimates, old = to_rename, new = unname(names_changes[to_rename]))
  }
  
  setnames(bpues_estimates, "Species", "species")
  
  bpues_estimates[bpue2,on = c("species", cols),`Monitoring coverage` := i.monitoring_coverage] #This column actually already exists at the moment, just set up the function like this if we ever want to go to different level of aggregations.
  
  colnames(bpues_estimates) <- c(original_colnames) # Going back to original names after join
  
  bpues_estimates[`Monitoring coverage` < 0.001 & reliability == FALSE, color := "#D25600"]
  bpues_estimates[`Monitoring coverage` < 0.001 & reliability == TRUE, color := "#D3D3D3"]
  
  bpues_estimates[`Monitoring coverage` >= 0.001 & `Monitoring coverage` <= 0.005 & reliability == FALSE, color := "#923500"]
  bpues_estimates[`Monitoring coverage` >= 0.001 & `Monitoring coverage` <= 0.005 & reliability == TRUE, color := "#9283AC"]
  
  bpues_estimates[`Monitoring coverage` > 0.005 & reliability == FALSE, color := "#541600"]
  bpues_estimates[`Monitoring coverage` > 0.005 & reliability == TRUE, color := "#553687"]
  
  #Add a qualitative description of the color coding:
  
  bpues_estimates[`Monitoring coverage` < 0.001 & reliability == FALSE, flag_description := "Uncertain estimate and poor monitoring coverage - values should not be used for advice"]
  bpues_estimates[`Monitoring coverage` < 0.001 & reliability == TRUE, flag_description := "Estimate more reliable but poor monitoring coverage - values should not be used for advice"]
  
  bpues_estimates[`Monitoring coverage` >= 0.001 & `Monitoring coverage` <= 0.005 & reliability == FALSE, flag_description := "Uncertain estimate and low monitoring coverage"]
  bpues_estimates[`Monitoring coverage` >= 0.001 & `Monitoring coverage` <= 0.005 & reliability == TRUE, flag_description := "More reliable estimate but low monitoring coverage"]
  
  bpues_estimates[`Monitoring coverage` > 0.005 & reliability == FALSE, flag_description := "Uncertain estimate and monitoring coverage above satisfactory threshold"]
  bpues_estimates[`Monitoring coverage` > 0.005 & reliability == TRUE, flag_description := "More reliable estimate and monitoring coverage above satisfactory threshold"]
  
  bpues_estimates
}


