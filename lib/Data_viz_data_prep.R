####Script to prepare the dataset for data visualisation
# In addition to the output of BEAM calc_bpue, it recovers some information on quality checkpoint failed for each species, ecoregion, metier 

#Loading required packages
library(data.table)
library(countrycode)
library(dplyr)

#Set working directory to BEAM-main to be able to call some of it's functions directly. Version of the code as of 07/07/2025. Any modification to fit the purpose of this script are detailed below.

#Input datasets 
#Present in the BEAM /data folder
annex01_species <- fread("data/ICES_Annex_1_WGBYC_2024.csv",
                          col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")
mediterranean <- fread("data/Med_Annex_1_WGBYC_2025.csv",
                        col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")

monitoring <- fread("data/obs3.csv")
fishing <- fread("data/fishing_all_years.csv")
bycatch <- fread("data/bycatch2.csv")
#monitoring <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/data/Bycatch_monitoring_effort_2024&2025Extraction_10092025.csv"))
#fishing <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/data/FishingEffort_2024&2025Extraction_10092025.csv"))
#bycatch <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/data/BycatchEvent_2024&2025Extraction_10092025.csv"))

# D2 <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/BEAM-main/data/D2_monitoringeffort_2017_2023.csv"))
# D2 <- D2[,-"V1"]
# D1 <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/BEAM-main/data/D1_fishingeffort_2017_2023.csv"))
# D1 <- D1[,-"V1"]
# D3 <- fread(file.path("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc 2025/BEAM-main/data/D3_bycatchevent_2017_2023.csv"))
# D3 <- D3[,-"V1"]
# # 
# # #Merging with data from previous years
# 
# #fishing had an extra column so I am removing it, I don't think it's necessary for beam
# fishing <- fishing[,-"TripsF_txt"]
# 
# #Columns are matching in the correct order but the case of the column names is different
# colnames(D1) <- colnames(fishing)
# fishing <- rbind(fishing,D1)
# 
# #Columns are matching in the correct order but the case of the column names is different
# colnames(D2) <- colnames(monitoring)
# monitoring <- rbind(monitoring, D2)
# 
# #bycatch had an extra "ID" column so I am removing it, I don't think it's necessary for beam
# bycatch <- bycatch[,-"ID"]
# #Columns are matching in the correct order but the case of the column names is different
# colnames(D3) <- colnames(bycatch)
# bycatch <- rbind(bycatch, D3)

#Correcting country codes to ISO 3 letters
# 
# all_objects <- list(monitoring, bycatch, fishing)
# 
# all_objects <- lapply(all_objects, function(df) {
#   df[Country == "SI", Country := "IS"] #Clean typo for Iceland
#   df[, country_ISO3 := countrycode(Country, origin = "iso2c", destination = "iso3c")] #Converts country code from ISO two letters to ISO three letters
#   print(table(df[, .(Country, is.na(country_ISO3))])) #Printing a check along the way to manually see where NAs are introduced if any. But they should be fixed automatically in the following line, this is just to visually assess what caused them in case it's an unusual reason.
#   df[is.na(country_ISO3), country_ISO3 := Country]#For instances where this introduced NAs (e.g. PT-20), replace by the original code
#   df[, Country := country_ISO3] #replace in the original column used in the code afterwards
#   df[, country_ISO3 := NULL] #removing the temporary column
#   df
# })
# 
# monitoring <- all_objects[[1]]
# bycatch <- all_objects[[2]]
# fishing <- all_objects[[3]]
# 
# if (c("NULL")%in%unique(bycatch$IncidentsWithPingers)) {
#   bycatch$IncidentsWithPingers[bycatch$IncidentsWithPingers=="NULL"]<-"0"
#   bycatch$IncidentsWithPingers<-as.numeric(bycatch$IncidentsWithPingers)
# }
# 
# 
# if (c("NULL")%in%unique(bycatch$IndividualsWithPingers)) {
#   bycatch$IndividualsWithPingers[bycatch$IndividualsWithPingers=="NULL"]<-"0"
#   bycatch$IndividualsWithPingers<-as.numeric(bycatch$IndividualsWithPingers)
# }
# 

#### Re-inserting some of the /clean_data.R steps, but not all because I want to keep track of instances where checks were not passed. 

#source("lib/clean_chars.R") #No change
#source("lib/add_vessel_length_groups.R") #No change

#colnames(fishing) <- tolower(colnames(fishing))
#clean_chars(fishing)
#add_vessel_length_groups(fishing)

#fishing <- fishing[!is.na(ecoregion) & !(ecoregion %in% c("north west atlantic"))]
endyear<-max(obs3$year,na.rm=TRUE)
# Grab 2023 which is the year we are using for the meta-analysis. Or should I make an average of fishing effort per year as well ?
fishing_2024 <- fishing[year == endyear]

#fishing_2024$metierl6[fishing_2024$metierl6 == "-"] <- NA

#fishing$metierl6[fishing$metierl6 == "-"] <- NA

###
#colnames(monitoring) <- tolower(colnames(monitoring))
#clean_chars(monitoring)
#add_vessel_length_groups(monitoring)

#monitoring <- monitoring[!is.na(ecoregion) & !(ecoregion %in% c("north west atlantic"))]

#This is where I start making changes, to keep track of monitoring methods that didn't pass the first quality checkpoint. 

#Instead of deleting those records, we add a flag monitoring_suitable YES/NO

# monitoring <- monitoring[!(monitoringmethod == "lb" & country != "pt")]
# monitoring <- monitoring[!(monitoringmethod == "oth" & country != "no")]
# monitoring <- monitoring[(monitoringmethod != "po")]
# monitoring <- monitoring[!(monitoringmethod == "vo" & country == "ee")]


monitoring[, monitoring_suitable := ifelse(
  (monitoringmethod == "lb" & country != "pt") |
    (monitoringmethod == "oth" & country != "no") |
    (monitoringmethod == "po") |
    (monitoringmethod == "vo" & country == "ee"),
  "no", "yes")]

###

#colnames(bycatch) <- tolower(colnames(bycatch))
#clean_chars(bycatch)
#add_vessel_length_groups(bycatch)

#bycatch <- bycatch[!is.na(ecoregion) & !(ecoregion %in% c("north west atlantic"))]

#Same modification as for monitoring effort, adding the flag monitoring_suitable YES/NO

bycatch[, monitoring_suitable := ifelse(
  (monitoringmethod == "lb" & country != "pt") |
    (monitoringmethod == "oth" & country != "no") |
    (monitoringmethod == "po") |
    (monitoringmethod == "vo" & country == "ee"),
  "no", "yes")]

# bycatch <- bycatch[!(monitoringmethod == "lb" & country != "pt")]
# bycatch <- bycatch[!(monitoringmethod == "oth" & country != "no")]
# bycatch <- bycatch[(monitoringmethod != "po")]
# bycatch <- bycatch[!(monitoringmethod == "vo" & country == "ee")]


bycatch[ , class := classname]
bycatch[superclass == "reptilia" & is.na(classname), class := "reptilia"]
bycatch[classname %in% c("actinopteri", "holocephali", "myxini", "petromyzonti"), class := "fish"]


bycatch[individualswithpingers < 0, individualswithpingers := NA] #minus values = NA
bycatch[incidentswithpingers < 0, incidentswithpingers := NA] #minus values = NA

bycatch[ , n_individ := rowSums(.SD, na.rm = TRUE),.SDcols=c("individualswithpingers","individualswithoutpingers")] #ignore NAs when summing
bycatch[ , n_incident := rowSums(.SD, na.rm = TRUE),.SDcols=c("incidentswithpingers","incidentswithoutpingers")] #ignore NAs when summing

#### Re-inserting some of the steps in the /generate_the_list.R script, but editing according to objectives.


monitoring = monitoring[!is.na(ecoregion) & !is.na(country), .(daysatsea = sum(daysatsea, na.rm = TRUE)),
            by = .(ecoregion, areacode, country, year,
                   metierl4, metierl5, vessellength_group,
                   samplingprotocol, monitoringmethod, monitoring_suitable)] #adding monitoring_suitable as a grouping variable to keep the DaS totals separate.

#Why do we sometimes have 0 days at sea reported in a line of monitoring ?

bycatch = bycatch[, .(n_ind = sum(n_individ, na.rm = TRUE)),
                    by = .(ecoregion, areacode, country, year,
                           metierl4, metierl5, vessellength_group,
                           samplingprotocol, monitoringmethod, species,monitoring_suitable)] #adding monitoring_suitable as a grouping variable to keep the n_ind totals separate.


# create list of relevant Ecoregion * species combination

ecoreg_species = rbindlist(list(annex01_species, mediterranean))
clean_chars(ecoreg_species) # fix misc. character issues
ecoreg_species[, aphiaid := unique(na.omit(aphiaid))[1], species] # fill in NAs
ecoreg_species <- unique(ecoreg_species) # remove duplicates

monitoring <- monitoring[ecoreg_species, on = "ecoregion", allow.cartesian = TRUE]

# Split the DaS per ecoregion summary table into two columns, monitoring suitable, monitoring not suitable.
das_per_ecoregion <- monitoring[, .(
  daysAtSea2017_2023_suitable_monitoring = sum(daysatsea[monitoring_suitable == "yes"], na.rm = TRUE),
  daysAtSea2017_2023_unsuitable_monitoring = sum(daysatsea[monitoring_suitable == "no"], na.rm = TRUE)
), by = ecoregion][order(ecoregion)]


#das_per_ecoregion <- monitoring[, .(daysAtSea2017_2023 = sum(daysatsea, na.rm=T)), ecoregion][order(ecoregion)]

fwrite(x = das_per_ecoregion,
       file = "results/DaS_ecoregion_2017_2024_data_vis.csv", sep = ";",na="NA")


##common name introduced by accident in obs3 somewhere
#Stellate sturgeon
#acipenser stellatus

monitoring[species == "stellate sturgeon", species := "acipenser stellatus"]


###
# add in bycatch counts
#That's again when I need to add a column for n_ind under suitable monitoring, and under unsuitable monitoring 

#Let's call this table obs
obs <- monitoring
obs$n_ind <- 0
obs[bycatch, on = .(ecoregion, areacode, country, year,
                      metierl4, metierl5, vessellength_group,
                      samplingprotocol, monitoringmethod, species, monitoring_suitable), #Again added a monitoring_suitable grouping factor
     n_ind := i.n_ind]

#round up number of individuals
obs[, n_ind := round(n_ind, 0)]


###
# add a flag (taxon_bycatch_monitor_ok), to indicate whether the species bycaught matches the observer protocol being used for the fishing activity
obs[ , taxa_monitored := samplingprotocol]
obs[samplingprotocol == "cetaceans", taxa_monitored := "mammals"]

# # get to taxonomic cluster
# taxizedb::db_download_ncbi()
# species_info <- taxizedb::classification(unique(obs$species))
# species_info <- rbindlist(lapply(species_info, function(x) {
#   if (class(x) == "logical") {
#     data.table(species = tolower(names(x)))
#   } else {
#     setNames(as.data.table(t(tolower(x$name))), x$rank)
#   }
# }), fill = TRUE)
# 
# species_info[class == "aves", taxon := "seabirds"]
# species_info[class == "mammalia", taxon := "mammals"]
# species_info[order == "testudines", taxon := "turtles"]

#fwrite(species_info, file="results/species_info.csv")

#There is already a taxon column in the monitoring (obs2) table. It doesn't have NAs, maybe we just use this one ? Just rename "birds" to "seabirds" and "marine turtles" to "turtles" ?
#"birds"          "fish"           "mammals"        "marine turtles"

obs[taxon == "birds", taxon := "seabirds"]
obs[taxon == "marine turtles", taxon := "turtles"]

obs[species_info, on = 'species', `:=`(order = i.order)]
obs[order %in% c("carcharhiniformes", "chimaeriformes", "hexanchiformes","lamniformes", "myliobatiformes", "rajiformes", "rhinopristiformes", "squaliformes", "torpediniformes"), taxon := "elasmobranchs"]

#Fix the remaining problems by looking at the species individually

obs[, species := stri_trim_both(species)] #Cleans invisible characters, needed for "centroselachus crepidater "

obs[species %in% c("apristurus", "centroselachus crepidater", "deania calceus", "glaucostegus", "mobula", "pomatomus saltatrix", "rhinobatidae", "sphyrnidae", "squatina aculeata", "squatina oculata", "squatina squatina"), taxon := "elasmobranchs"]

# join taxa to obs
#obs[species_info, on = .(species), taxon := i.taxon]

obs[, taxon_bycatch_monitor_ok := (taxa_monitored %in% c("all","elasmobranchs~seabirds~mammals", "protectedspecies")) | (taxa_monitored == taxon)
]

obs[, taxon_bycatch_monitor_ok := ifelse(taxon_bycatch_monitor_ok == TRUE, "yes", "no")]

#Removing Arctic Ocean because there is no info in the metierl4 column

obs <- obs[metierl4!='']

#Create a column with average monitoring effort per year
# Need to sum all the days at sea available per species x ecoregion x metierl4 x monitoring suitable x taxa monitored ok

#First let's clean up a bit and get rid of the columns we don't need anymore.

obs <- obs[,.(ecoregion, metierl4, monitoring_suitable, daysatsea, species, taxon, n_ind, taxon_bycatch_monitor_ok, year)]

#Bringing in the proportion of fishing monitored...

fishing_agg <- fishing[, .(daysatseaf = sum(daysatseaf)), by = .(ecoregion, metierl4, year)]

#Add this to the obs table

obs[fishing_agg, on = .(ecoregion, year,
                    metierl4), total_fishing := daysatseaf]

#Now, do I compute the % effort, and then average it, or do I aggregate it and then compute the % effort.
#Let's do it before and take the average

obs[,monitoring_coverage := daysatsea*100 / total_fishing]
#We have NAs for some cases where total fishing is not available for that year

#Create a new aggregated dataset where days at sea and n_ind are sums of multiple rows

obs_agg <- obs[, .(n_ind = sum(n_ind), daysatsea = sum(daysatsea), n_year = length(unique(year)), average_monitoring_coverage = mean(monitoring_coverage, na.rm=TRUE)), by = .(ecoregion, metierl4, monitoring_suitable, species, taxon_bycatch_monitor_ok)]
obs_agg[, average_monitoring_effort := as.numeric(daysatsea/n_year)] #Should I take the mean instead and remove NAs ? What if we only have monitoring for 3 years ? Should we still take the average over 7 years, or the average over those three years ? Same question for fishing.

 

#Adding some of these information again.
obs_agg[obs, on = "species", taxon := taxon]
 
#Now let's bring in... THE BPUE !!!

bpue <- fread("data/bpue1.csv") 
#Read the latest version of the bpue estimates.

#Join the bpue estimates, only for the lines for which they were calculated, so when monitoring suitable = yes. Thoughts for later: When we both have suitable and unsuitable monitoring for a given combination, should we consider bpue was achieved ?
#Should we keep the fail records only for ecoregion / metier / species for which we don't have a bpue ?

obs_agg[monitoring_suitable == "yes" & taxon_bycatch_monitor_ok == "yes", c("bpue", "lwr", "upr", "replicates", "model", "base_model_heterogeneity") := bpue[.SD, on = .(ecoregion, metierl4, species), .(bpue, lwr, upr, replicates, model, base_model_heterogeneity)]]
 

#Create a new column for BPUE status (estimated / NA)
obs_agg[, bpue_available := as.numeric(ifelse(is.na(bpue), "0", "1"))] #Using a numeric here in case we watn to use a analysis method based on matrices.

#Converting heterogeneity test to YES/NO for consistency across variables
obs_agg[, base_model_heterogeneity := ifelse(base_model_heterogeneity==TRUE, "yes", "no")] 


#For lines where monitoring and sampling is okay, unused DaS is the sum of the lines where one of the two wasn't okay

obs_agg[, usable_DaS := sum(daysatsea[monitoring_suitable == "yes" & taxon_bycatch_monitor_ok == "yes"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_DaS_monitoring := sum(daysatsea[monitoring_suitable == "no" & taxon_bycatch_monitor_ok == "yes"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_DaS_sampling := sum(daysatsea[monitoring_suitable == "yes" & taxon_bycatch_monitor_ok == "no"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_DaS_both := sum(daysatsea[monitoring_suitable == "no" & taxon_bycatch_monitor_ok == "no"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, usable_n_ind := sum(n_ind[monitoring_suitable == "yes" & taxon_bycatch_monitor_ok == "yes"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_n_ind_monitoring := sum(n_ind[monitoring_suitable == "no" & taxon_bycatch_monitor_ok == "yes"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_n_ind_sampling := sum(n_ind[monitoring_suitable == "yes" & taxon_bycatch_monitor_ok == "no"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_n_ind_both := sum(n_ind[monitoring_suitable == "no" & taxon_bycatch_monitor_ok == "no"], na.rm = TRUE),
        by = .(ecoregion, metierl4, species)]

# --- Totals ---
obs_agg[, unused_DaS := unused_DaS_monitoring + unused_DaS_sampling + unused_DaS_both,
        by = .(ecoregion, metierl4, species)]

obs_agg[, unused_n_ind := unused_n_ind_monitoring + unused_n_ind_sampling + unused_n_ind_both,
        by = .(ecoregion, metierl4, species)]



#Compute percentages
obs_agg[, pct_usable_effort :=
          fifelse((usable_DaS + unused_DaS) > 0,
                  daysatsea * 100 / (usable_DaS + unused_DaS), 0)]

obs_agg[, pct_usable_records :=
          fifelse((usable_n_ind + unused_n_ind) > 0,
                  n_ind * 100 / (usable_n_ind + unused_n_ind), 0)]

#Setting priority rules
obs_agg[, priority := fcase(
  bpue_available == "1", 1L,
  taxon_bycatch_monitor_ok == "yes", 2L,
  monitoring_suitable == "yes", 3L,
  default = 4L
)]

# Keep best row per group
obs_short <- obs_agg[, .SD[which.min(priority)], by = .(ecoregion, metierl4, species)]


obs_short[, priority := NULL]


obs_short[, c("monitoring_suitable", "taxon_bycatch_monitor_ok", "base_model_heterogeneity") :=
            lapply(.SD, as.factor),
          .SDcols = c("monitoring_suitable", "taxon_bycatch_monitor_ok", "base_model_heterogeneity")]


obs_short[, bycatch_reported := as.factor(
  ifelse(n_ind > 0 | unused_n_ind > 0, "yes", "no")
)]

#Add column for number of terms in the model (NA = no model or "none", 0 = "n_ind ~ 1" , and then count number of random effects (maybe counting the number of "+" sgins ?))

#For rows that have a bpue, I want to capture additional parameters: 
#How many random factors were included in the model, if any
#Was monitoring or sampling protocol retained as a random effect ?
#Was there some unexplained heterogeneity left ? I suppose these are the case when heterogeneity = TRUE but no random effect is retained in the model.
#Maybe having a column per candidate random effect and a yes/no Area_code, Vessel_length, MetierL5, Year, Country, Sampling_protocol, Monitoring_method


formulas <- obs_short[,c("model", "base_model_heterogeneity")]


formulas[model != "none" & model != "only one", model_formula := lapply(.SD[[1]], function(x) {
  tryCatch(as.formula(x), error = function(e) NA)
}), .SDcols = "model"]


formulas[, re := lapply(model_formula, lme4::findbars)]

formulas[, re := lapply(model_formula, function(f) {
  bars <- lme4::findbars(f)
  if (length(bars) == 0) return(NA_character_)
  sapply(bars, function(term) as.character(term[[3]]))
})]


formulas[, n_re := sapply(re, length)]
formulas[is.na(re), n_re := 0] # replacing by 0 when model was n_ind ~ 1
formulas[is.null(model_formula), n_re := NA] #Replaceing by NAs where there was no model
formulas[, unexplained_heterogeneity := ifelse(base_model_heterogeneity == "yes" & n_re == 0, "yes", "no")]


all_strings <- unique(unlist(formulas[!is.na(re), re]))

# Step 2: Create one column per unique string
for (s in all_strings) {
  formulas[, (s) := ifelse(is.na(re), "no", ifelse(sapply(re, function(x) s %in% x), "yes", "no"))]
}

obs_short <- cbind(obs_short, formulas[,-c("model", "base_model_heterogeneity", "re", "model_formula")])

obs_short <- data.table::as.data.table(obs_short)

obs_short[,bpue_usable := "Not applicable"]

obs_short$bpue_usable <- as.factor(obs_short$bpue_usable)

obs_short[bpue_available == "1", bpue_usable := "yes"]

obs_short[model == "only one", bpue_usable := "no"]

obs_short[samplingprotocol == "yes" | monitoringmethod == "yes", bpue_usable := "no"]

#Now need to pull in the fishing effort data to see which BPUE will be unusable due to some levels of random effect not being available.

total_bycatch <- fread("data/tot1.csv")

total_bycatch[, bpue_usable := ifelse(message=="OK", "yes", "no")]

total_bycatch[, bycatch_estimated := ifelse(is.na(tot_mean), "no", "yes")]

obs_short[bpue_available == "1", c("bpue_usable", "message", "bycatch_estimated") := total_bycatch[.SD, on = .(ecoregion, metierl4, species), .(bpue_usable, message, bycatch_estimated)]]

obs_short$message <- factor(obs_short$message)

obs_short[samplingprotocol == "yes" | monitoringmethod == "yes", message := "Random effect on monitoring or sampling protocol"]

obs_short[message == "levels for at least one random effect not ok", message := "Levels of at least one random effect not available"]

obs_short[message == "OK", message := "Fishing data available at all levels"]

obs_short$message <- factor(obs_short$message)

fwrite(obs_agg,"data/obs_agg_data_vis.csv",na="NA") 

fwrite(obs_short,"data/obs_short_data_vis.csv",na="NA")

