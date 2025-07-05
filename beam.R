# BEAM vX.Y

# imports all R packages that are needed for BEAM
library(data.table) # data wrangling
library(reshape2) # data wrangling
library(icesConnect) # downloading data from ICES servers
library(httr) # parsing downloaded data above
library(jsonlite) # parsing downloaded data
library(glmmTMB) # model fitting
library(metafor) # testing heterogeneity 
library(ggeffects) # BPUE estimation
library(emmeans) # BPUE estimation
library(doParallel) # parallel computation
library(stringdist) # data checking and cleaning (string manipulation)
#library(taxize) # data cleaning
library(taxizedb) # data cleaning, devtools::install_github("ropensci/taxizedb")

#### Jun 2025
library(tcltk)
library(ggplot2)
library(reshape2)
library(systemfonts)
library(flextable) #not quite well built
library(scales)
library(countrycode)

######################################
### initialise the work environment
######################################

select_directory <- function() {
  
  dir_path <- (tk_choose.dir(caption = "choose the directory where you have downloaded the BEAM code (folder BEAM_main)"))
  
  if (is.na(dir_path) || dir_path == "") {
    message("No directory selected.")
    return(NULL)
  } else {
    message("Selected directory: ", dir_path)
    return(dir_path)
  }
  return(dir_path)
}

set_WE<-function() {
path<-select_directory()
##### set the working environment and createa data folder
setwd(path)
if (!file.exists("data")) {
dir.create(file.path(path, "data"))
}

if (!file.exists("results")) {
dir.create(file.path(path, "results"))
}


## obtain THISYEAR as the year of the assessment
THISYEAR<<-as.numeric(readline(prompt="what is the year of the assessment (typically year of the data call in yyyy format):"))
#naughty a bit
while (is.na(THISYEAR)) {
THISYEAR<<-as.numeric(readline(prompt="the year value you have entered is not a number, try again:"))
}

cores<<-as.numeric(readline(prompt="we will use parallel computing approaches, how many cores would you like to dedicate to the BEAM task:"))
while (is.na(cores)) {
cores<<-as.numeric(readline(prompt="the number of cores value you have entered is not a number, try again:"))
}


annex01_species <<- fread(file.path(path,"data/ICES_Annex_1_WGBYC_2024.csv"),
                         col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")
mediterranean <<- fread(file.path(path,"data/Med_Annex_1_WGBYC_2025.csv"),
                       col.names = c("aphiaid", "species", "ecoregion", "taxon"), encoding = "Latin-1")

}

set_WE()

#### Jun 2025



# beam_lib.R imports all BEAM internal functionality
source("lib/beam_lib.R")

# uncomment to enable parallel execution
# pick a reasonable number of workers for your system
registerDoParallel(cores) # change Jun 2025

# BEAM WORKFLOW


################################################################################
# STEP 1: Download WGBYC data from ICES server
# This gives us all1, obs1 and bycatch1 (and D4 and D5) in the global env (and saved on disk)
#beam_download(years = 2017:2023, token = "access_token_goes_here")
obs1 <- fread(file.path("data/D2_monitoringeffort_2017_2023.csv"))
all1 <- fread(file.path("data/D1_fishingeffort_2017_2023.csv"))
bycatch1 <- fread(file.path("data/D3_bycatchevent_2017_2023.csv"))

####################################################################################
#### added Jun 2025
#### dealing with the issue that some 2-letter ISO country code are homonyms of functions ("de" and "se")
all_objects <- list(obs1, all1, bycatch1)

all_objects <- lapply(all_objects, function(df) {
  df[country == "SI", country := "IS"] #Clean typo for Iceland
  df[, country_ISO3 := countrycode(country, origin = "iso2c", destination = "iso3c")] #Converts country code from ISO two letters to ISO three letters
  print(table(df[, .(country, is.na(country_ISO3))])) #Printing a check along the way to manually see where NAs are introduced if any. But they should be fixed automatically in the following line, this is just to visually assess what caused them in case it's an unusual reason.
  df[is.na(country_ISO3), country_ISO3 := country]#For instances where this introduced NAs (e.g. PT-20), replace by the original code
  df[, country := country_ISO3] #replace in the original column used in the code afterwards
  df[, country_ISO3 := NULL] #removing the temporary column
  df
})

obs1 <- all_objects[[1]]
all1 <- all_objects[[2]]
bycatch1 <- all_objects[[3]]

####################################################################################

################################################################################
# STEP 2: data cleaning and preparation
# gives us all2, obs2 and bycatch2 in global env (and saved on disk)
source("lib/clean_data.R")

################################################################################
# STEP 3: additional data cleaning and preparation
# generates obs3.csv (plus some additional files)
source("lib/generate_the_list.R")

################################################################################
# STEP 4: calculate BPUE estimates for ecoregion * metierl4 * species combos,
# based on the list created above.
eco_m4_spec <- unique(obs3[, .(ecoregion, metierl4, species)])
system.time(bpue1 <- calc_bpue(eco_m4_spec, dat = obs3)) # 237 secs per 1000 row with 10 workers
fwrite(bpue1, file = "data/bpue1.csv", sep = ";")

################################################################################

# Are there any other intermediary steps here?

################################################################################
# STEP 5: estimate total bycatch based on BPUE table
tot1 <- vector(mode = "list", length = nrow(bpue1))
for (i in 1:nrow(bpue1)) {
    cat("\rProcessing row", i)
    tot1[[i]] <- calc_total(bpue1[i], obs = obs3, all = all2, verbose = FALSE)
}
tot1 <- rbindlist(tot1)
fwrite(tot1, file = "data/tot1.csv", sep = ";")

################################################################################
# For any ecoregion * metierl4 * species combos where we can't predict total
# bycatch due to mismatches between r.e. levels in monitored and total effort data,
# try refitting those, but on the area * metierl4 * species level instead and 
# predict using those finer-scale models.
# Step 2: Extract unique ecoregion, metierL4, and species combinations


# source("lib/add_finer_scale_models_when_needed.R")
# bpue2 <- rbindlist(list(bpue1, bpue_fine_scale))
# tot2 <- rbindlist(list(tot1, tot_fine_scale))
tot2<-tot1
################################################################################
# final QC, assign color codes to models according to rules set out in script
source("lib/QCs_TotalBycatch.R")

################################################################################
# any final steps, e.g. final formatting, table generation and visualisation

#here we need to bring bpue1 and total_bycatch together

fwrite(total_bycatch, "final_beam_table.csv")

all.metier<-fread("data/ecoregion_metier_sps_risk_bycatch_2023.csv")

all.metier$species<-tolower(all.metier$species)
THEDATA<-obs3

total_bycatch$complete<-"no"
total_bycatch$missing<-0

whole.bycatch.df<-data.frame(ecoregion=total_bycatch$ecoregion[!duplicated(total_bycatch[,c(1,3)])],species=total_bycatch$species[!duplicated(total_bycatch[,c(1,3)])])

total_bycatch.sub<-subset(total_bycatch,!is.na(mean))

for (i in 1:nrow(whole.bycatch.df)) {

fished<-all.metier$metierL4[all.metier$ecoregion==whole.bycatch.df$ecoregion[i] &all.metier$species==whole.bycatch.df$species[i]] 
tb<-total_bycatch.sub$metierl4[total_bycatch.sub$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch.sub$species==whole.bycatch.df$species[i]] 
fished<-fished[!is.na(fished)]

if (all(fished%in%tb)==TRUE) {
total_bycatch$complete[total_bycatch$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch$species==whole.bycatch.df$species[i]] <-"yes"

}
total_bycatch$missing[total_bycatch$ecoregion==whole.bycatch.df$ecoregion[i] &total_bycatch$species==whole.bycatch.df$species[i]] <-sum((fished%in%tb)==FALSE)


}

# fished<-all.metier$metierL4[all.metier$ecoregion=="Bay of Biscay and the Iberian Coast" &all.metier$species=="delphinus delphis"] 
# tb<-total_bycatch$metierL4[total_bycatch$ecoregion=="Bay of Biscay and the Iberian Coast" &total_bycatch$species=="delphinus delphis"] 



complete_tb<-total_bycatch[!is.na(total_bycatch$mean)&total_bycatch$complete=="yes",]

fwrite(complete_tb,"results/cases_with_complete_total_bycatch.csv")
fwrite(total_bycatch,"results/cases_with_total_bycatch.csv")


library(taxize)
library(tidyverse)


priority1<-c("phocoena phocoena","delphinus delphis","monachus monachus")
priority2<-"Turtles"
priority3<-c("angel shark", "common skate", "guitarfish", "maltese
skate", "great white shark", "sand tiger shark", "smalltooth sand tiger shark",
"spiny butterfly ray", "sturgeon", "balearic shearwater")




taxon.df<-data.frame(species=unique(THEDATA$species),taxa=NA)
taxon.df$taxa<-THEDATA$taxa[match(taxon.df$species,THEDATA$species)]

commonnames<-read.csv("data/commonnames2024.csv")
commonnames<-commonnames[!duplicated(commonnames$Species),]
commonnames$species<-tolower(commonnames$Species)

#make table bpue first

total_bycatch$taxa<-taxon.df$taxa[match(total_bycatch$species,taxon.df$species)]
total_bycatch$common<-NA
total_bycatch$common<-commonnames$common[match(total_bycatch$species,commonnames$species)]


table.print<-total_bycatch[,c("ecoregion","metierl4","taxa","species","common","n_ind","daysAtSea","final.fished.daysAtSea","model","bpue","lwr","upr","tbfinal.mean","tbfinal.lwr","tbfinal.upr")]


table.print$interannual<-"none apparent"
table.print$interannual[grep("year",table.print$model)]<-"there is between-year variability in BPUE"

table.print$key.representability<-"a constant BPUE appears to be representative"
table.print$key.representability[grep("metierL5",table.print$model)]<-"there is between-metier level 5 variability in BPUE"
table.print$key.representability[grep("vesselLength_group",table.print$model)]<-"there is between-vessel length category variability in BPUE"
table.print$key.representability[grep("areaCode",table.print$model)]<-"there is spatial variability in BPUE"
table.print$key.representability[grep("country",table.print$model)]<-"there is spatial variability in BPUE"
table.print$key.representability[grep("monitoringMethod",table.print$model)]<-"there is variability in BPUE depending on monitoring protocols"
table.print$key.representability[grep("samplingProtocol",table.print$model)]<-"there is variability in BPUE depending on monitoring protocols"

table.print$n_ind<-round(table.print$n_ind,0)
table.print$daysAtSea<-round(table.print$daysAtSea,0)
table.print$final.fished.daysAtSea<-round(table.print$final.fished.daysAtSea,0)
table.print$bpue<-round(table.print$bpue,6)
table.print$lwr<-round(table.print$lwr,7)
table.print$upr<-round(table.print$upr,7)
table.print$tbfinal.mean<-round(table.print$tbfinal.mean,0)
table.print$tbfinal.lwr<-round(table.print$tbfinal.lwr,1)
table.print$tbfinal.upr<-round(table.print$tbfinal.upr,1)
table.print$taxa<-factor(table.print$taxa,levels=c("Mammals","Seabirds","Turtles","Fish"))

colnames(table.print)<-c("Ecoregion", "metier L4", "Taxon", "Species", "Common name", 
							"# individuals 2017-2023", "monitoring effort (DaS) 2017-2023", 
							"Fishing effort (DaS) 2023", "BPUE model", "BPUE", "lower", "upper",
							"total bycatch 2023", "TB lower", "TB upper", "interannual", "key variability in BPUE")
table.print$"# individuals 2017-2023"<-round(table.print$"# individuals 2017-2023",0)

table.print$interannual[table.print$interannual=="none apparent"]<-"none"
table.print$interannual[table.print$interannual!="none"]<-"present"
table.print$BPUE<-round(table.print$BPUE,4)
table.print$lower<-round(table.print$lower,4)
table.print$upper<-round(table.print$upper,4)

fwrite(table.print,file="results/bpue_table_print.csv")


##### render the table for report
ft <- flextable(table.print[order(table.print$Ecoregion,table.print$Taxon,table.print$"metier L4",table.print$Species),])
ft <-theme_vanilla(ft)
set_table_properties(ft, width = 1, layout = "autofit")
save_as_docx(
  ft, 
  path = "results/BPUE_table1.docx")



CTB_sum = complete.tb[
  ,
  .(n_ind = sum(n_ind),
	monitoring_DaS=sum(daysAtSea),
	fishing_DaS=sum(final.fished.daysAtSea),
	TB=sum(tbfinal.mean),
	lower=sum(tbfinal.lwr),
	upper=sum(tbfinal.upr),
	taxa=unique(taxa),
	metier=length(common),
	common=unique(common)
	),
  keyby = .(ecoregion, species)]


CTB_sum$n_ind<-round(CTB_sum$n_ind,0)
CTB_sum$monitoring_DaS<-round(CTB_sum$monitoring_DaS,0)
CTB_sum$fishing_DaS<-round(CTB_sum$fishing_DaS,0)
CTB_sum$TB<-round(CTB_sum$TB,0)
CTB_sum$lower<-round(CTB_sum$lower,1)
CTB_sum$upper<-round(CTB_sum$upper,1)
CTB_sum$taxa<-factor(CTB_sum$taxa,levels=c("Mammals","Seabirds","Turtles","Fish"))


colnames(CTB_sum)<-c("Ecoregion", "species","# individuals 2017-2023","monitoring effort (DaS) 2017-2023", 
"Fishing effort (DaS) 2023","total bycatch 2023", "TB lower", "TB upper","Taxon", "# metiers","Common name")

CTB_sum<-CTB_sum[,c(1,9,2,11,3,4,5,6,7,8,10)]



ftc <- flextable(CTB_sum[order(CTB_sum$Ecoregion,CTB_sum$Taxon,CTB_sum$species),])
ftc <-theme_vanilla(ftc)
set_table_properties(ftc, width = 1, layout = "autofit")
save_as_docx(
  ftc, 
  path = "results/Total_bycatch_table1.docx")



#####


priority3<-paste(priority3,collapse="|")
grep(priority3,tolower(table.print$"Common name"))

priorty1_tb<-table.print[table.print$Species%in%priority1&!is.na(table.print$"total bycatch 2023"),]
priorty2_tb<-table.print[table.print$Taxon=="Turtles"&!is.na(table.print$"total bycatch 2023"),]
priorty3_tb<-table.print[grep(priority3,tolower(table.print$"Common name")),]
priorty3_tb<-priorty3_tb[!is.na(priorty3_tb$"total bycatch 2023"),]

priority_list<-rbind(priorty1_tb,priorty2_tb,priorty3_tb)
priority_list<-priority_list[,-c(9,16)]


ftp <- flextable(priority_list[order(priority_list$Ecoregion,priority_list$Taxon,priority_list$"metier L4",priority_list$Species),])
ftp <-theme_vanilla(ftp)
set_table_properties(ftp, width = 1, layout = "autofit")
save_as_docx(
  ftp, 
  path = "results/priority_table1.docx")



#################################################################################################################
#### rate of achievement

achieved<-subset(total_bycatch,!is.na(tbfinal.mean))
achieved$label<-paste(achieved$common,achieved$ecoregion,achieved$metierL4,sep=" and ")


mammals<-ggplot(subset(achieved,taxa=="Mammals"& label!="Short-beaked Common Dolphin and Bay of Biscay and the Iberian Coast and PTB"), #removing the few with unsensible CI
  aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  ylim(0,4000)

birds<-ggplot(subset(achieved,taxa=="Seabirds"& label!="European Shag and Adriatic Sea and OTB"), 
   aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()


reptiles<-ggplot(subset(achieved,taxa=="Turtles"& label!="Loggerhead and Adriatic Sea and OTB"), 
    aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()

 fish<-ggplot(subset(achieved,taxa=="Fish"&tbfinal.upr<100000), 
   aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()
 

	
png("results/totalbycatch_mammals.png",width=20,height=35,units="cm",res=200)
mammals
dev.off()


png("results/totalbycatch_turtles.png",width=20,height=10,units="cm",res=200)
reptiles
dev.off()


png("results/totalbycatch_birds.png",width=20,height=35,units="cm",res=200)
birds
dev.off()


################################################################################################################
##### tables
tried<-aggregate(daysAtSea~ecoregion+metierL4+species,data=subset(THEDATA,n_ind>0),"sum")

tried$achieved<-0
for (i in 1:nrow(achieved)) {
tried$achieved[which(tried$ecoregion==achieved$ecoregion[i] & tried$metierL4==achieved$metierL4[i] & tried$species==achieved$species[i])]<-1

}

tried$ecoregion<-factor(tried$ecoregion)
tried$metierL4<-factor(tried$metierL4)
tried$species<-factor(tried$species)

tab.E.M<-table(tried[tried$achieved==1,]$ecoregion,tried[tried$achieved==1,]$metierL4)/table(tried$ecoregion,tried$metierL4)
tab.E.M[which(table(tried$ecoregion,tried$metierL4)==0)]<-NA

po=melt(tab.E.M)
head(po)
names(po)<-c("Ecoregion","MetierL4","value")

gg.E.M<-ggplot(po, aes(MetierL4,Ecoregion)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = po$value, label = round(po$value, 2))) + # write the values
  scale_fill_gradient2(name="Species proportion",
					   low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0.5) + 
	xlab("Metier level 4")+
	theme_minimal()
	
png("results/summary_table_prop_Eco_metier_2024.png",width=30,height=10,units="cm",res=200)
gg.E.M	
dev.off()
##################################################################


tab.S.M<-table(tried[tried$achieved==1,]$species,tried[tried$achieved==1,]$metierL4)/table(tried$species,tried$metierL4)
tab.S.M[which(table(tried$species,tried$metierL4)==0)]<-NA

po=melt(tab.S.M)
head(po)
names(po)<-c("Species","MetierL4","value")

gg.S.M<-ggplot(po, aes(MetierL4,Species)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(fill = po$value, label = round(po$value, 2))) + # write the values
  scale_fill_gradient2(name="Ecoregion proportion",
					   low = muted("darkred"), 
                       mid = "white", 
                       high = muted("midnightblue"), 
                       midpoint = 0.5) + 
	xlab("Metier level 4")+
	theme_minimal()
	
png("results/summary_table_prop_Species_metier_2024.png",width=25,height=32,units="cm",res=200)
gg.S.M	
dev.off()




table.print<-fread("data/bpue1.csv")
table.print<-table.print[,-c(1,7:10,13:16)]
table.print$n_ind<-round(table.print$n_ind,0)
table.print$daysAtSea<-round(table.print$daysAtSea,0)

ft <- flextable(table.print[order(table.print$ecoregion,table.print$species,table.print$metierL4),])
ft <-theme_vanilla(ft)
set_table_properties(ft, width = 1, layout = "autofit")
save_as_docx(
  ft, 
  path = "results/Appendix1.docx")



