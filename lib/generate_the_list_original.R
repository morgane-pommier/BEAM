# This script generates the following files:
# 1. results/table0_DaS_ecoregion_2017_2023.csv     A table of total DAS per ecoregion
# 2. results/species_info.csv                       A table of species, class, subclass and taxon (fish/seabirds/mammals/turtles)
# 3. data/obs3.csv                                  Monitored effort data, but joined with species info

obs3 = obs2[!is.na(ecoregion) & !is.na(country), .(daysatsea = sum(daysatseaob, na.rm = TRUE)),
                   by = .(ecoregion, areacode, country, year,
                          metierl4, metierl5, vessellength_group,
                          samplingprotocol, monitoringmethod)]

bycatch3 = bycatch2[, .(n_ind = sum(n_individ, na.rm = TRUE)),
                       by = .(ecoregion, areacode, country, year,
                              metierl4, metierl5, vessellength_group,
                              samplingprotocol, monitoringmethod, species)]
###
# create list of relevant Ecoregion * species combinations  #########

#annex01_species <- fread("data/ICES_Annex_1_WGBYC_2024.csv",
                         #col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")
#mediterranean <- fread("data/Med_Annex_1_WGBYC_2024.csv",
                       #col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")
ecoreg_species = rbindlist(list(annex01_species, mediterranean))
clean_chars(ecoreg_species) # fix misc. character issues
ecoreg_species[, aphiaid := unique(na.omit(aphiaid))[1], species] # fill in NAs
ecoreg_species <- unique(ecoreg_species) # remove duplicates

obs3 <- obs3[ecoreg_species, on = "ecoregion", allow.cartesian = TRUE]

das_per_ecoregion <- obs3[, .(daysAtSea2017_2023 = sum(daysatsea, na.rm=T)), ecoregion][order(ecoregion)]

fwrite(x = das_per_ecoregion,
       file = "results/table0_DaS_ecoregion_2017_2023.csv", sep = ";")



### 
# make sure species names do not contain typos
# AM: I guess this step should probably be done manually

# first identify any species in bycatch data that aren't in the species list
themissing <- bycatch3[!obs3, on = "species", unique(species)] # anti-join
the_list_species <- unique(obs3$species)

## what distance makes sense?
discovery.df <- data.frame(distance=c(1:max(nchar(themissing))),NAs=0)
for (i in 1:nrow(discovery.df)) {
    closematch_missing <- the_list_species[amatch(themissing, the_list_species, maxDist = discovery.df$distance[i])]
    discovery.df$NAs[i] <- sum(is.na(closematch_missing))
}

#plot(NAs~distance,data=discovery.df)

missing_match <- data.frame(missing = themissing, match = the_list_species[amatch(themissing, the_list_species, maxDist = 7)])
#manually rationalising only keeping those that are indeed close (homonyms or typo)
missing_match <- missing_match[c(32,96),]

##common name introduced by accident in obs3 somewhere
#Stellate sturgeon
#acipenser stellatus

obs3[species == "stellate sturgeon", species := "acipenser stellatus"]


###
# add in bycatch counts
obs3$n_ind <- 0
obs3[bycatch3, on = .(ecoregion, areacode, country, year,
                      metierl4, metierl5, vessellength_group,
                      samplingprotocol, monitoringmethod, species),
     n_ind := i.n_ind]

#round up number of individuals
obs3[, n_ind := round(n_ind, 0)]


###
# add a flag (taxon_bycatch_monitor_ok), to indicate whether the species bycaught
# matches the observer protocol being used for the fishing activity
obs3[ , taxa_monitored := samplingprotocol]
obs3[samplingprotocol == "cetaceans", taxa_monitored := "mammals"]

# get to taxonomic cluster
taxizedb::db_download_ncbi()
species_info <- taxizedb::classification(unique(obs3$species))
species_info <- rbindlist(lapply(species_info, function(x) {
    if (class(x) == "logical") {
        data.table(species = tolower(names(x)))
    } else {
        setNames(as.data.table(t(tolower(x$name))), x$rank)
    }
}), fill = TRUE)

species_info[class == "aves", taxon := "seabirds"] #MP:Replaced class name with lower case
species_info[class == "mammalia", taxon := "mammals"] #MP:Replaced class name with lower case
species_info[order == "testudines", taxon := "turtles"] #MP: Replaced class = "Reptilia" by order == "testudines" because there was no reptilia class in the table ?

######################################################


#############################################################
fwrite(species_info, file="results/species_info.csv")

#MP: those edits fix the NAs, but we still miss the fish category and some species in obs3 are not in the species_info list. So I just skipped the species_info object altogether and used the code below. But maybe it's worth checking, surely there was a reason to the use of the species_info and taxonomic list.

#MP: There is already a taxon column in the obs2/obs3 table. It doesn't have NAs, so maybe we just use this one ? Just rename "birds" to "seabirds" and "marine turtles" to "turtles" ?

###################################
#### let's go this way 15 Sep DL
obs3[taxon == "birds", taxon := "seabirds"]
obs3[taxon == "marine turtles", taxon := "turtles"]
obs3[species_info, on = 'species', `:=`(order = i.order)] #adds order to the obs3 table 
obs3[order %in% c("carcharhiniformes", "chimaeriformes", "hexanchiformes","lamniformes", "myliobatiformes", "rajiformes", "rhinopristiformes", "squaliformes", "torpediniformes"), taxon := "elasmobranchs"]

#Fix the remaining problems by looking at the species individually

obs3[, species := stri_trim_both(species)] #Cleans invisible characters, needed for "centroselachus crepidater " #extra space for this one.....

obs3[species %in% c("apristurus", "centroselachus crepidater", "deania calceus", "glaucostegus", "mobula", "pomatomus saltatrix", "rhinobatidae", "sphyrnidae", "squatina aculeata", "squatina oculata", "squatina squatina"), taxon := "elasmobranchs"]


# join taxa to obs3 #MP: Skip this line since we use the original taxon column present in obs2/obs3
#obs3[species_info, on = .(species), taxon := i.taxon] 

#MP: Just run this one (already existed, just generating the taxon column differently)

obs3[,
    taxon_bycatch_monitor_ok := (taxa_monitored %in% c("all","elasmobranchs~seabirds~mammals", "protectedspecies")) | (taxa_monitored == taxon)
]


fwrite(obs3, "data/obs3.csv") # previously monitor_effort_bycatch_ecoregion_area_species.csv

########################################## 15Sept2025 DL