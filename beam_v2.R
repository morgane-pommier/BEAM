
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


source("lib/beam_lib.R")

# Set the current WD to the path to the folder that this script is saved in
# This lets us use relative paths throughout (rather than absolute paths).
setwd(this.path::this.dir())

### BEAN RUNNING SETTINGS
# username - this would usually be your email 
ices_username <- ""

# ICES data access token -
# TODO: move this info into readme/instructions-for-use file?
##### request access to data (e.g. speak to Carlos, or email him at carlos@ices.dk)
##### once data access is accepted you will receive an email to login at
##### https://data.ices.dk/token
#####  there you will generate a token which you need to copy and paste in the function below 
##### or assign it to an object eg, token<-"ewioubreuicvepb" which you can call in the function
##### do note the username: it should be the email (eg louis.attack@dartmouth.edu) you used to 
##### make the access request
ices_token <- ""

# years to use for estimation of BPUEs
# by default, BEAM uses all years since 2017 and up to last year
# comment out and set manually if needed
years <- seq(2017, year(Sys.Date())-1)

# number of cores to use for tasks that can be parallelized
# use parallel::detectCores() to find the max for your system
# note that bigger is not always better/faster.
ncores <- 10

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
# STEP 4.1: calculate BPUE estimates for ecoregion * metierl4 * species combos,
# based on the list created above.
#obs3 <- fread("data/obs3.csv")

eco_m4_spec <- unique(obs3[, .(ecoregion, metierl4, species)])

bpue1 <- calc_bpue(eco_m4_spec[1:1000],
                   cols = c("ecoregion", "metierl4", "species"),
                   dat = obs3)
fwrite(bpue1, file = "data/bpue1.csv", sep = ";")

################################################################################
# STEP 4.2: For cases where ecoregion-level estimation and prediction is considered 
# inappropriate, we can go to the areacode level.

obs3_area <- subset(obs3,(ecoregion=="aegean-levantine sea"|
                             ecoregion=="baltic sea"|
                             ecoregion=="bay of biscay and the iberian coast"|
                             ecoregion=="celtic seas") &
                       (taxon=="mammals"|taxon=="seabirds"))

area_m4_spec <- unique(obs3area[, .(ecoregion, areacode, metierl4, metierl5, species)])

bpue1_area <- calc_bpue(needle = area_m4_spec, 
                        cols = c("ecoregion", "areacode", "metierl4", "species"),
                        dat = obs3_area[taxon_bycatch_monitor_ok==TRUE])
fwrite(bpue1_area, file = "data/bpue1_area.csv", sep = ";")

################################################################################
# STEP 4.3: Do some post-processing on the bpue1 object(s) to add some needed
# information, like n_ind, das, etc. See lib/annotate_bpue.R for more details.
bpue2 <- annotate_bpue(bpue1, cols = c("ecoregion", "metierl4", "species"))
fwrite(bpue2, file = "data/bpue2.csv", sep = ";")

bpue2_area <- annotate_bpue(bpue1_area, cols = c("ecoregion", "areacode", "metierl4", "species"))
fwrite(bpue2_area, file = "data/bpue2_area.csv", sep = ";")

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
fwrite(tot1, file = "data/tot1.csv", sep = ";")

################################################################################
# STEP 6: estimate total bycatch based on BPUE area table

tot1_area <- vector(mode = "list", length = nrow(bpue1))

for (i in 1:nrow(bpue1_area)) {
    cat("\rProcessing row ", i, "/", nrow(bpue1_area), sep="")
    tot1[[i]] <- calc_total(bpue1_area[i],
                            obs = obs3[taxon_bycatch_monitor_ok==TRUE], 
                            all = all2, 
                            cols = c("areacode", "metierl4", "species"),
                            verbose = FALSE)
}
tot1_area <- rbindlist(tot1_area)
fwrite(tot1_area, file = "data/tot1.csv", sep = ";")
################################################################################
# STEP 6: Quality control, assign color codes to models according to rules set out in script
source("lib/QCs_TotalBycatch.R")

################################################################################
# any further processing


fwrite(total_bycatch, "final_beam_table.csv")

all.metier<-fread("data/ecoregion_metier_sps_risk_bycatch_2023.csv")

all.metier$species<-tolower(all.metier$species)
all.metier$ecoregion<-tolower(all.metier$ecoregion)
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
priority2<-"turtles"
priority3<-c("angel shark", "common skate", "guitarfish", "maltese
skate", "great white shark", "sand tiger shark", "smalltooth sand tiger shark",
"spiny butterfly ray", "sturgeon", "balearic shearwater")




taxon.df<-data.frame(species=unique(THEDATA$species),taxa=NA)
taxon.df$taxa<-THEDATA$taxon[match(taxon.df$species,THEDATA$species)]

commonnames<-read.csv("data/commonnames2024.csv")
commonnames<-commonnames[!duplicated(commonnames$species),]
commonnames$species<-tolower(commonnames$species)

#make table bpue first

total_bycatch$taxa<-taxon.df$taxa[match(total_bycatch$species,taxon.df$species)]
total_bycatch$common<-NA
total_bycatch$common<-commonnames$common[match(total_bycatch$species,commonnames$species)]

complete_tb$taxa<-taxon.df$taxa[match(complete_tb$species,taxon.df$species)]
complete_tb$common<-NA
complete_tb$common<-commonnames$common[match(complete_tb$species,commonnames$species)]


fwrite(complete_tb,"results/cases_with_complete_total_bycatch.csv")
fwrite(total_bycatch,"results/cases_with_total_bycatch.csv")


############################################################
##### 16 Sept DL
##### here we need to bring obs3 variables in total_bycatch
#############################################################

# total_bycatch
#effort is fishing effort in assessment year
# bpue1
# obs3[taxon_bycatch_monitor_ok==TRUE]
#let's just grab 2024 n_ind and DaS monitoring from here

summaryyear<-aggregate(cbind(n_ind,daysatsea)~ecoregion+metierl4+species,sum,data=obs3[taxon_bycatch_monitor_ok==TRUE&year==(year(Sys.Date())-1)] )
summaryall<-aggregate(cbind(n_ind,daysatsea)~ecoregion+metierl4+species,sum,data=obs3[taxon_bycatch_monitor_ok==TRUE] )

names(summaryall)[4:5]<-c("n_ind_all","daysatsea_all")

table2print<-total_bycatch
names(table2print)[5:7]<-c("tbfinal.mean","tbfinal.lwr","tbfinal.upr")

table2print<-merge(table2print,bpue1,by=c("ecoregion","metierl4","species"),all=FALSE)
table2print<-merge(table2print,summaryyear,by=c("ecoregion","metierl4","species"),all.x=TRUE)
table2print<-merge(table2print,summaryall,by=c("ecoregion","metierl4","species"),all.x=TRUE)


#dat$n_ind / dat$daysatsea

table.print<-table2print[,c("ecoregion","metierl4","taxa","species","common","n_ind","daysatsea","effort","n_ind_all","daysatsea_all","model.x","bpue","lwr","upr","tbfinal.mean","tbfinal.lwr","tbfinal.upr")]


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
table.print$daysatsea<-round(table.print$daysatsea,0)
table.print$n_ind<-round(table.print$n_ind_all,0)
table.print$daysatsea_all<-round(table.print$daysatsea_all,0)
table.print$effort<-round(table.print$effort,0)
table.print$bpue<-round(table.print$bpue,6)
table.print$lwr<-round(table.print$lwr,7)
table.print$upr<-round(table.print$upr,7)
table.print$tbfinal.mean<-round(table.print$tbfinal.mean,0)
table.print$tbfinal.lwr<-round(table.print$tbfinal.lwr,1)
table.print$tbfinal.upr<-round(table.print$tbfinal.upr,1)
table.print$taxa<-factor(table.print$taxa,levels=c("mammals","seabirds","turtles","elasmobranchs","fish"))

colnames(table.print)<-c("Ecoregion", "metier L4", "Taxon", "Species", "Common name", 
							"# individuals 2024", "monitoring effort (DaS) 2024", 
							"Fishing effort (DaS) 2024","# individuals 2017-2024", "monitoring effort (DaS) 2017-2024", 
							"BPUE model", "BPUE", "lower", "upper",
							"total bycatch 2024", "TB lower", "TB upper", "interannual", "key variability in BPUE")
							
table.print$"# individuals 2017-2024"<-round(table.print$"# individuals 2017-2024",0)
table.print$"# individuals 2024"<-round(table.print$"# individuals 2024",0)

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

save_as_html(
  ft, 
  path = "results/BPUE_table1.html")

complete_tb2print<-complete_tb
names(complete_tb2print)[5:7]<-c("tbfinal.mean","tbfinal.lwr","tbfinal.upr")

complete_tb2print<-merge(complete_tb2print,bpue1,c("ecoregion","metierl4","species"),all.x=TRUE)
complete_tb2print<-merge(complete_tb2print,summaryyear,c("ecoregion","metierl4","species"),all.x=TRUE)
complete_tb2print<-merge(complete_tb2print,summaryall,c("ecoregion","metierl4","species"),all.x=TRUE)

CTB_sum = complete_tb2print[
  ,
  .(n_ind = sum(n_ind),
	monitoring_DaS=sum(daysatsea),
	n_ind_all = sum(n_ind_all),
	monitoring_DaS_all=sum(daysatsea_all),
	fishing_DaS=sum(effort),
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
CTB_sum$n_ind_all<-round(CTB_sum$n_ind_all,0)
CTB_sum$monitoring_DaS_all<-round(CTB_sum$monitoring_DaS_all,0)
CTB_sum$fishing_DaS<-round(CTB_sum$fishing_DaS,0)
CTB_sum$TB<-round(CTB_sum$TB,0)
CTB_sum$lower<-round(CTB_sum$lower,1)
CTB_sum$upper<-round(CTB_sum$upper,1)
CTB_sum$taxa<-factor(CTB_sum$taxa,levels=c("mammals","seabirds","turtles","elasmobranchs","fish"))


colnames(CTB_sum)<-c("Ecoregion", "species","# individuals 2024","monitoring effort (DaS) 2024",
"# individuals 2017-2023","monitoring effort (DaS) 2017-2023", 
"Fishing effort (DaS) 2024","total bycatch 2024", "TB lower", "TB upper","Taxon", "# metiers","Common name")

CTB_sum<-CTB_sum[,c(1,9,2,11,3,4,5,6,7,8,10)]



ftc <- flextable(CTB_sum[order(CTB_sum$Ecoregion,CTB_sum$Taxon,CTB_sum$species),])
ftc <-theme_vanilla(ftc)
set_table_properties(ftc, width = 1, layout = "autofit")
save_as_docx(
  ftc, 
  path = "results/Total_bycatch_table1.docx")

save_as_html(
  ftc, 
  path = "results/Total_bycatch_table1.html")


#####


priority3<-paste(priority3,collapse="|")
grep(priority3,tolower(table.print$"Common name"))

priorty1_tb<-table.print[table.print$Species%in%priority1&!is.na(table.print$"total bycatch 2024"),]
priorty2_tb<-table.print[table.print$Taxon=="Turtles"&!is.na(table.print$"total bycatch 2024"),]
priorty3_tb<-table.print[grep(priority3,tolower(table.print$"Common name")),]
priorty3_tb<-priorty3_tb[!is.na(priorty3_tb$"total bycatch 2024"),]

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

achieved<-subset(total_bycatch,!is.na(mean))
achieved$label<-paste(achieved$common,achieved$ecoregion,achieved$metierl4,sep=" and ")


mammals<-ggplot(subset(achieved,taxa=="mammals"& 
								label!="Short-beaked Common Dolphin and bay of biscay and the iberian coast and ptb" &
								label!="Harbor Porpoise and bay of biscay and the iberian coast and gtr" &
								label!="Gray Seal and celtic seas and otm" &
								label!="Gray Seal and baltic sea and gtr"), #removing the few with unsensible CI
  aes(x = label, y=(mean)),colour="white") +
  geom_crossbar(aes(ymin = (lwr), ymax = (upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  ylim(0,4000)

birds<-ggplot(subset(achieved,taxa=="seabirds"& 
							  label!="European Shag and adriatic sea and otb" &
							  label!="Northern Fulmar and celtic seas and gns" &
							  label!="Herring Gull and bay of biscay and the iberian coast and otb" &
							  label!="Northern Gannet and greater north sea and gns" &
							  label!="European Shag and adriatic sea and otb" 
							  ), 
   aes(x = label, y=(mean)),colour="white") +
  geom_crossbar(aes(ymin = (lwr), ymax = (upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals) - note that the axis is on a log scale")+
  theme_minimal()+
  scale_y_continuous(trans = "log10")


reptiles<-ggplot(subset(achieved,taxa=="turtles"& label!="Loggerhead and Adriatic Sea and OTB"), 
    aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()


elasmo<-ggplot(subset(achieved,taxa=="elasmobranchs"), 
    aes(x = label, y=(tbfinal.mean)),colour="white") +
  geom_crossbar(aes(ymin = (tbfinal.lwr), ymax = (tbfinal.upr)), width = 0.5, fill = "light blue",fatten=0) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()


 fish<-ggplot(subset(achieved,taxa=="fish"&tbfinal.upr<100000), 
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



#Prepares a new dataset for data visualisation, capturing failed checks along the way instead of discarding the data. Saves as new files in /data
source("lib/Data_viz_data_prep.R")
#Produces a series of figures showing effort suitability, availability of bycatch records, and ability to generate a BPUE across ecoregion, metier and taxon, saved in /results
source("lib/Data_viz_MP.R")

#Interactive flow chart for sample retention through BEAM

### !!!! The y axis is sqrt transformed for visualisation, so it's misleading !!! I wanted to add labels with actual % on each stratum, but the % get transformed as well, need to fix that #####
library(shiny)
runApp("lib/Sample_retention_BEAM_flow_chart_2025.R")




