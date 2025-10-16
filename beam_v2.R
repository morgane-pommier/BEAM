
# imports all R packages that are needed for BEAM
library(data.table) # data wrangling
library(reshape2) # data wrangling
library(icesConnect) # downloading data from ICES servers
library(httr) # parsing downloaded data above
library(jsonlite) # parsing downloaded data
library(glmmTMB) # model fitting
library(metafor) # testing heterogeneity 
library(ggeffects) # BPUE estimation
library(emmeans) # BPUE estimation, don't go above emmeans  #1.11.0 or it explodes!!!
library(this.path) # for auto-detecting the dir a script is executing out of


library(doSNOW) # parallel computation
library(doParallel) # parallel computation
library(progress) # lotion for impatient minds
library(this.path)
library(stringdist) # data checking and cleaning (string manipulation)
library(stringi)
#library(taxize) # data cleaning
library(taxizedb) # data cleaning, devtools::install_github("ropensci/taxizedb")
library(countrycode) # to convert between 2 and 3- letter country codes 
library(tcltk)
library(ggplot2)
library(reshape2)
library(systemfonts)
library(flextable) #not quite well built
library(scales)
library(countrycode)
library(stringi)


# Set the current WD to the path to the folder that this script is saved in
# This lets us use relative paths throughout (rather than absolute paths).
setwd(this.path::this.dir())

source("lib/beam_lib.R")


### BEAN RUNNING SETTINGS
# username - this would usually be your email 
ices_username <- "davlu"

# ICES data access token -
# TODO: move this info into readme/instructions-for-use file?
##### request access to data (e.g. speak to Carlos, or email him at carlos@ices.dk)
##### once data access is accepted you will receive an email to login at
##### https://data.ices.dk/token
#####  there you will generate a token which you need to copy and paste in the function below 
##### or assign it to an object eg, token<-"ewioubreuicvepb" which you can call in the function
##### do note the username: it should be the email (eg louis.attack@dartmouth.edu) you used to 
##### make the access request
ices_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoiZGF2bHVAYXF1YS5kdHUuZGsiLCJqdGkiOiI0YmI3ODE1ZS0zMDM1LTRmOGEtYjQyOC0xNzdjNGQyOWJhMGIiLCJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9lbWFpbGFkZHJlc3MiOiJkYXZsdUBhcXVhLmR0dS5kayIsIlVzZXJFbWFpbCI6ImRhdmx1QGFxdWEuZHR1LmRrIiwiRW1haWwiOiJkYXZsdUBhcXVhLmR0dS5kayIsImV4cCI6MTc2MTU2NDA1MSwiaXNzIjoiaHR0cDovL3RhZi5pY2VzLmRrIiwiYXVkIjoiaHR0cDovL3RhZi5pY2VzLmRrIn0.fpbfSe7gyN5no4Wlx_ZpZLMuWwqhDTSD3hhTWFKgKGw"

# years to use for estimation of BPUEs
# by default, BEAM uses all years since 2017 and up to last year
# comment out and set manually if needed
years<-seq(2017,2023)
#years <- seq(2017, year(Sys.Date())-1)

# number of cores to use for tasks that can be parallelized
# use parallel::detectCores() to find the max for your system
# note that bigger is not always better/faster.
ncores <- 15

### BELOW this line, you shouldn't NEED to make any changes.

cl <- makeCluster(ncores)
registerDoSNOW(cl)

################################################################################
# STEP 1: Download WGBYC data from ICES server
# load raw data by downloading it from the ICES bycatch database or by loading
# them locally. Expect a download time < 10 secs per year of data
all1 <- beam_get_raw_data("data/all1.csv", years = years, force_download = FALSE)
obs1 <- beam_get_raw_data("data/obs1.csv", years = years, force_download = FALSE)
bycatch1 <- beam_get_raw_data("data/bycatch1.csv", years = years, force_download = FALSE)

################################################################################
# STEP 2: data cleaning and preparation
# gives us all2, obs2 and bycatch2 in global env (and saved on disk)
source("lib/clean_data.R")

# STEP 3: additional data cleaning and preparation
# generates obs3.csv (plus some additional files)
source("lib/generate_the_list.R")

################################################################################
# STEP 4.o1: calculate BPUE estimates for ecoregion * metierl4 * species combos,
# based on the list created above.
#obs3 <- fread("data/obs3.csv")

eco_m4_spec <- unique(obs3[, .(ecoregion, metierl4, species)])

bpue1 <- calc_bpue(eco_m4_spec,
                   cols = c("ecoregion", "metierl4", "species"),
                   dat = obs3[taxon_bycatch_monitor_ok==TRUE])
fwrite(bpue1, file = "data/bpue1.csv", sep = ";",na="NA")

################################################################################
# STEP 4.2: For cases where ecoregion-level estimation and prediction is considered 
# inappropriate, we can go to the areacode level.

#### we want you to think about it for this step, so we kept it manual

#obs3_area <- subset(obs3,(ecoregion=="aegean-levantine sea"|
#                             ecoregion=="baltic sea"|
#                             ecoregion=="bay of biscay and the iberian coast"|
#                             ecoregion=="celtic seas") &
#                       (taxon=="mammals"|taxon=="seabirds"))

#area_m4_spec <- unique(obs3_area[, .(ecoregion, areacode, metierl4, metierl5, species)])

#bpue1_area <- calc_bpue(needle = area_m4_spec, 
#                        cols = c("ecoregion", "areacode", "metierl4", "species"),
#                        dat = obs3_area[taxon_bycatch_monitor_ok==TRUE])
#fwrite(bpue1_area, file = "data/bpue1_area.csv", sep = ";",na="NA")

################################################################################
# STEP 4.3: Do some post-processing on the bpue1 object(s) to add some needed
# information, like n_ind, das, etc. See lib/annotate_bpue.R for more details.
bpue2 <- annotate_bpue(bpue1, cols = c("ecoregion", "metierl4"))
fwrite(bpue2, file = "data/bpue2.csv", sep = ";",na="NA")


#bpue2_area <- annotate_bpue(bpue1_area, cols = c("ecoregion", "areacode", "metierl4", "species"))
#fwrite(bpue2_area, file = "data/bpue2_area.csv", sep = ";",na="NA")

################################################################################
# STEP 5: estimate total bycatch based on BPUE table

tot1 <- vector(mode = "list", length = nrow(bpue1))

for (i in 1:nrow(bpue1)) {
    cat("\rProcessing row ", i, "/", nrow(bpue1), sep="")
    tot1[[i]] <- calc_total(bpue1[i],
                            obs = obs3[taxon_bycatch_monitor_ok==TRUE], 
                            all = all2, 
                            verbose = FALSE)
}
tot1 <- rbindlist(tot1)
fwrite(tot1, file = "data/tot1.csv", sep = ";",na="NA")

################################################################################
# STEP 6: estimate total bycatch based on BPUE area table

#tot1_area <- vector(mode = "list", length = nrow(bpue1))

#for (i in 1:nrow(bpue1_area)) {
#    cat("\rProcessing row ", i, "/", nrow(bpue1_area), sep="")
#    tot1[[i]] <- calc_total(bpue1_area[i],
#                            obs = obs3[taxon_bycatch_monitor_ok==TRUE], 
#                            all = all2, 
#                            cols = c("areacode", "metierl4", "species"),
#                            verbose = FALSE)
#}
#tot1_area <- rbindlist(tot1_area)
#fwrite(tot1_area, file = "data/tot1.csv", sep = ";",na="NA")
################################################################################
# STEP 6: Quality control, assign color codes to models according to rules set out in script
tot2<-tot1
source("lib/QCs_TotalBycatch.R")

################################################################################
# any further processing


fwrite(total_bycatch, "final_beam_table.csv",na="NA")

all.metier<-fread("data/ecoregion_metier_sps_risk_bycatch_2023.csv")

all.metier$species<-tolower(all.metier$species)
all.metier$ecoregion<-tolower(all.metier$ecoregion)
THEDATA<-obs3

total_bycatch$complete<-"no"
total_bycatch$missing<-0

whole.bycatch.df<-data.frame(ecoregion=total_bycatch$ecoregion[!duplicated(total_bycatch[,c(1,3)])],species=total_bycatch$species[!duplicated(total_bycatch[,c(1,3)])])

total_bycatch.sub<-subset(total_bycatch,!is.na(tot_mean))

for (i in 1:nrow(whole.bycatch.df)) {

fished<-all.metier$metierl4[all.metier$ecoregion==whole.bycatch.df$ecoregion[i] &all.metier$species==whole.bycatch.df$species[i]] 
tb<-total_bycatch.sub$metierl4[total_bycatch.sub$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch.sub$species==whole.bycatch.df$species[i]] 
fished<-fished[!is.na(fished)]

if (all(fished%in%tb)==TRUE) {
total_bycatch$complete[total_bycatch$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch$species==whole.bycatch.df$species[i]] <-"yes"

}
total_bycatch$missing[total_bycatch$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch$species==whole.bycatch.df$species[i]] <-sum((fished%in%tb)==FALSE)


}

# fished<-all.metier$metierL4[all.metier$ecoregion=="Bay of Biscay and the Iberian Coast" &all.metier$species=="delphinus delphis"] 
# tb<-total_bycatch$metierL4[total_bycatch$ecoregion=="Bay of Biscay and the Iberian Coast" &total_bycatch$species=="delphinus delphis"] 



complete_tb<-total_bycatch[!is.na(total_bycatch$tot_mean)&total_bycatch$complete=="yes",]

fwrite(complete_tb,"results/cases_with_complete_total_bycatch.csv",na="NA")
fwrite(total_bycatch,"results/cases_with_total_bycatch.csv",na="NA")


library(taxize)
library(tidyverse)


priority1<-c("phocoena phocoena","delphinus delphis","monachus monachus")
priority2<-"turtles"
priority3<-c("angel shark", "common skate", "guitarfish", "maltese
skate", "great white shark", "sand tiger shark", "smalltooth sand tiger shark",
"spiny butterfly ray", "sturgeon", "balearic shearwater","sterlet")

##################################################################
#######priority list strictly 
####### added 29 Sept 2025
##################################################################


priority1<-c("phocoena phocoena","delphinus delphis","monachus monachus")
priority2<-c("")
priority3<-c("")


taxon.df<-data.frame(species=unique(THEDATA$species),taxa=NA)
taxon.df$taxa<-THEDATA$taxon[match(taxon.df$species,THEDATA$species)]

commonnames<-read.csv("data/commonnames2024.csv")
commonnames<-commonnames[!duplicated(commonnames$species),]
commonnames$species<-tolower(commonnames$species)
commonnames$common<-tolower(commonnames$common)

#make table bpue first

total_bycatch$taxa<-taxon.df$taxa[match(total_bycatch$species,taxon.df$species)]
total_bycatch$common<-NA
total_bycatch$common<-commonnames$common[match(total_bycatch$species,commonnames$species)]

complete_tb$taxa<-taxon.df$taxa[match(complete_tb$species,taxon.df$species)]
complete_tb$common<-NA
complete_tb$common<-commonnames$common[match(complete_tb$species,commonnames$species)]


fwrite(complete_tb,"results/cases_with_complete_total_bycatch.csv",na="NA")
fwrite(total_bycatch,"results/cases_with_total_bycatch.csv",na="NA")


############################################################
##### 16 Sept DL
##### here we need to bring obs3 variables in total_bycatch
#############################################################

endyear<-2023
#summaryyear<-aggregate(cbind(n_ind,daysatsea)~ecoregion+metierl4+species,sum,data=obs3[taxon_bycatch_monitor_ok==TRUE&year==(year(Sys.Date())-1)] )
summaryyear<-aggregate(cbind(n_ind,daysatsea)~ecoregion+metierl4+species,sum,data=obs3[taxon_bycatch_monitor_ok==TRUE&year==endyear] )
summaryall<-aggregate(cbind(n_ind,daysatsea)~ecoregion+metierl4+species,sum,data=obs3[taxon_bycatch_monitor_ok==TRUE] )
names(summaryall)[4:5]<-c("n_ind_all","daysatsea_all")

#produce main table to print and bycatch estimate figures by taxon
source("lib/produce_table_and_overall_figures.R")


################################################################################################################
##### tables
#plot achievement rate the old way
source("lib/legacy_achievement_plots.R")



library(tidyverse)
library(EnvStats)
library(ggalluvial)
library(colorspace)

#Prepares a new dataset for data visualisation, capturing failed checks along the way instead of discarding the data. Saves as new files in /data
source("lib/Data_viz_data_prep.R")
#Produces a series of figures showing effort suitability, availability of bycatch records, and ability to generate a BPUE across ecoregion, metier and taxon, saved in /results
source("lib/Data_viz.R")

#Interactive flow chart for sample retention through BEAM

### !!!! The y axis is sqrt transformed for visualisation, so it's misleading !!! I wanted to add labels with actual % on each stratum, but the % get transformed as well, need to fix that #####
library(shiny)
runApp("lib/Sample_retention_BEAM_flow_chart_2025.R")




##########################################################################
#######################################################################
###reliability measures

## here we introduce reliability measures based on sensitiivty of model outcomes and breadth of confidence intervals
## those aid to guide end users of the estimates to judge when it is useful to consider the mean estimate for practical applications


#############DON'T RUN YET IT NEEDS PARALLELISATION TO HAVE A DECENT RUNTIME
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(data.table)

## this is slow at the moment but will be parallelised before WKBBEAM
source("lib/reliability_estimation_p.R")



bpues_estimates$reliability2<-FALSE
bpues_estimates$reliability2[bpues_estimates$delta<2&bpues_estimates$upper_factor_prop <= 25&bpues_estimates$lower_factor_prop <= 25]<-TRUE

temp<-merge(table.print,bpues_estimates,by.x=c("Ecoregion","metier L4","Species"),by.y=c("Ecoregion","metier.L4","Species"),all.x=TRUE)

table.print$reliability<-"uncertain"
table.print$reliability[which(temp$reliability2==TRUE)]<-"more reliable"

fwrite(table.print,file="results/bpue_table_print.csv",na="NA")

fwrite(subset(table.print,reliability=="more reliable"),file="results/table_reliable_estimates.csv",na="NA")


source("lib/vis_postreliability.R")

#### FIN #####
