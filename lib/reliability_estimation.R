## ---------------------------
##
## Script name: RMSE_bpues_V4.R
##
## Purpose of script: Calculat
##
## Authors: ToR C members
##
## ---------------------------


# 1) Mean BPUE + CV of estimates (emmeans)
# 2) Using LOO (=jacknife) so using ggpredict/emmeans we obtain a BPUE for the full model
#   and a BPUE for the jacknifed. Then, (BPUE_full-BPUE_jacknifed)^2 [we'll have 
#   as many values as rows of data for that unique combination of ecoregionxmetierxsps].
#   Then, sum all those rests and divide them by n-1 (cases) and make the square root

# Summary: fit full model + fit jacknifed models + ggpredict both + do (full_pred-jacknifed_pred)^2
# 

# FILTER WHEN BPUE !IS.NA

# ONLY FOR THOSE THAT WE HAVE A BPUE ESTIMATE

# pa filter, del bpue dataframe, cada fila ya tiene la combinacion unica de cada ecoxmetierxsps
# despues recuperar del full dataframe 

# Load data ----
full_data <- read.csv("data/obs3.csv")
bpues_estimates <- read.csv("results/bpue_table_print.csv")

library(glmmTMB)
library(ggeffects)
library(emmeans)
library(data.table)

# Preparing datasets
bpues_estimates$RMSE <- NA
# Remove NAs in BPUE estimates & 0 in obs3 DaS
bpues_estimates <- bpues_estimates[!is.na(bpues_estimates$BPUE), ]
# Reduce the size removing NAs in totalbycatch
bpues_estimates <- bpues_estimates[!is.na(bpues_estimates$total.bycatch.2024), ]
#complete_bpues$RMSE <- 0
full_data <- full_data[full_data$daysatsea > 0, ]
full_data <- full_data[!is.na(full_data$daysatsea), ]
full_data <- full_data[full_data$taxon_bycatch_monitor_ok == TRUE, ]
# Add log of monitoring (DaS)
full_data$logDAS <- log(full_data$daysatsea)

## this is a temporary fix introduced in 2025 to deal with a new request for which data will come
#for this particular case---no anguilla in full data
bpues_estimates<-bpues_estimates[bpues_estimates$Species!="anguilla anguilla",]
bpues_estimates<-bpues_estimates[bpues_estimates$Species!="salmo salar",]

bpue_estimates$country<-factor(bpue_estimates$country)
bpue_estimates$vessellength_group<-factor(bpue_estimates$vessellength_group)
bpue_estimates$areacode<-factor(bpue_estimates$areacode)
bpue_estimates$metierl5<-factor(bpue_estimates$metierl5)

#rmses <- vector()

# Function
rmse_estimator <- function(full_data, bpues_estimates){
  
  rmses <- numeric(nrow(bpues_estimates))
  # Row by row for unique combinations of ecoregionxmetierxsps
  lapply(1:nrow(bpues_estimates), function(i) {
    #rmses <- c()
    
    
    #for(i in 1:nrow(bpues_estimates)){  ##remove this later
    
    message("Processing case: ", i)
    # Subset obs3 accordingly
    full_data_subset <- full_data[full_data$ecoregion %in% bpues_estimates$Ecoregion[i] &
                               full_data$metierl4 %in% bpues_estimates$metier.L4[i] &
                               full_data$species %in% bpues_estimates$Species[i], ]
    
    # Extract the formula of the model
    formula <- bpues_estimates$BPUE.model[i]
    
    # Fit full model
    full_model <- glmmTMB(formula = as.formula(formula), 
                                   data = full_data_subset, 
                                   offset  = logDAS,
                                   family = nbinom2)
    
    # BPUE of the full model (predict) ----
    # Test for null model or with model + random effects
    form <- as.formula(formula)
    re <- lme4::findbars(form) # random effects part of model formulation (if any)
    re <- sapply(re, function(x) as.character(x[[3]]))
    re.n <- length(re) # number of random effects
    
    if(re.n == 0){
      full_pred <- as.data.frame(emmeans(full_model, ~1, type="response", offset = log(1)))
      full_pred_bpue <- full_pred$response
    }else{
      type_arg <- if (packageVersion("ggeffects") >= "2.0.0") "random" else "re"
      full_pred <- ggpredict(model = full_model, type =  type_arg, 
                             terms = c(re))
      # Order predictions by c(x, group, facet), so latter they can be compared 
      # with jacknifes equivalents
      full_pred <- as.data.table(full_pred)
      
      if(re.n==1|re.n==2)
      {full_pred <- full_pred[order(x, group), ]}
     
      if(re.n==3|re.n==4)# CREATE ANOTHER CASES WHERE WE HAVE MORE THAN 2 RANDOM EFFECTS (IS WHERE FACET APPEARS)
      {full_pred <- full_pred[order(x, group, facet), ]}
    } 
  #}
  
    
    # CREATE ANOTHER CASES WHERE WE HAVE MORE THAN 2 RANDOM EFFECTS (IS WHERE FACET APPEARS)
    
    # Jacknifed BPUES ----
    # Get number of rows
    ns <- nrow(full_data_subset)
    # Prepare storage of differences BPUE_full - BPUE_jacknife
    diffs <- numeric(nrow(full_data_subset))
    
    for(j in 1:nrow(full_data_subset)){
      #print(j)
      # Jacknife data
      full_data_jack <- full_data_subset[-j, ]
      # Fit model jacknifed
      jack_model <- glmmTMB(formula = as.formula(formula), 
                                     data = full_data_jack, 
                                     offset  = logDAS,
                                     family = nbinom2)
      # BPUE of the jacknifed model (predict) ----
      # Test for null model or with model + random effects
      form <- as.formula(formula)
      re <- lme4::findbars(form) # random effects part of model formulation (if any)
      re <- sapply(re, function(x) as.character(x[[3]]))
      re.n <- length(re) # number of random effects
      
      if(re.n == 0){
        jack_pred <- as.data.frame(emmeans(jack_model, ~1, type="response", 
                                           offset = log(1)))
        jack_pred_bpue <- jack_pred$response
        # Compute rest here (simplest result)
        #diffs[j] <- (full_pred_bpue-jack_pred_bpue)^2   #old version 
        diffs[j] <- (log1p(full_pred_bpue)-log1p(jack_pred_bpue))^2  #new version
        
      }else{
        jack_pred <- ggpredict(model = jack_model, type =  type_arg, 
                               terms = c(re))
        # Compute RMSE here for complex models, using ALL predictions
        jack_pred <- as.data.table(jack_pred)
        # Reorder levels as in the full model
        if(re.n==1|re.n==2)        {
          jack_pred$x <- factor(jack_pred$x, levels = levels(full_pred$x))
          jack_pred$group <- factor(jack_pred$group, levels(full_pred$group))
          jack_pred <- jack_pred[order(x, group), ]}
        if(re.n==3|re.n==4){ # CREATE ANOTHER CASES WHERE WE HAVE MORE THAN 2 RANDOM EFFECTS (IS WHERE FACET APPEARS)
          jack_pred$x <- factor(jack_pred$x, levels = levels(full_pred$x))
          jack_pred$group <- factor(jack_pred$group, levels(full_pred$group))
          jack_pred$facet <- factor(jack_pred$facet, levels(full_pred$facet))
          jack_pred <- jack_pred[order(x, group, facet), ]}
          
        # For this jack, calculate a value
        #diffs[j] <- sum(full_pred$predicted-jack_pred$predicted)^2, 
         #                  na.rm = T)/nrow(full_data_subset)
        diffs[j] <- sum((log1p(full_pred$predicted)-log1p(jack_pred$predicted))^2, 
                        na.rm = T)/nrow(full_pred)     #nrow(full_data_subset)
        
      }
    }
    # Compute RMSE once per model
    rmses[i] <<- sqrt(sum(diffs, na.rm=TRUE)/(nrow(full_data_subset)-1))
  })
  return(rmses)
}


# All species over the new obs3 and bpue_print_table
bpues_estimates$RMSE<-rmse_estimator(full_data = full_data,
                     bpues_estimates = bpues_estimates)


# Transform back from log scale
bpues_estimates$factor <- exp(bpues_estimates$RMSE)

# Calculate upper and lower limits

# Estimate delta - breadth of the confidence interval

bpues_estimates$TB.lower_log <- log10(bpues_estimates$TB.lower+1)
bpues_estimates$TB.upper_log<-log10(bpues_estimates$TB.upper+1)
bpues_estimates$delta<-bpues_estimates$TB.upper_log-bpues_estimates$TB.lower_log


#Factor=exp(RMSE) # RMSE here is the variable you have in your function but computed with the log modification above
# The lower limit of the 68% interval is: 100(1-1/factor) # in percentage
bpues_estimates$lower_factor_prop <- 100*(1-1/bpues_estimates$factor)
# The upper limit of the 68% interval is: 100(factor−1) # in percentage
bpues_estimates$upper_factor_prop <- 100*(bpues_estimates$factor-1)

# RELIABILITY LIMIT ON BOTH CONFIDENCE INTERVALS OF FACTOR
# Take the 25% limit of the lower % CI of factor (=80% similarity)
# Take the 25% limit of the upper % CIs of factor (=80% similarity)
# Which are below 25%

# Reliability
bpues_estimates$reliability <- bpues_estimates$lower_factor_prop <= 25 & bpues_estimates$upper_factor_prop <= 25

# # Plot rmse_log vs delta only for those that we are keeping
# reliable <- ggplot() +
  # geom_point(data = new.data[new.data$reliability == T, ], 
             # aes(RMSE_log, delta), col = "darkgreen") +
  # theme_bw()

# all_relvalues <- ggplot() +
  # geom_point(data = new.data, 
             # aes(RMSE_log, delta), col = "orange") +
  # theme_bw()

# # Before and after plot
# ggarrange(all_relvalues, reliable, nrow = 2)

# hist(new.data$delta[new.data$reliability == T], breaks = 200)

# SAVE SOME THINGS
fwrite(bpues_estimates, "results/bpue_table_print_reliability_subset.csv",na="NA")
