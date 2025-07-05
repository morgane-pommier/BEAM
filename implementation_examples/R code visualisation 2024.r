######################################################################################
###### R code post meeting visualiation and tabulation
###### 25 Sept 2024
#######################################################################################

source("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/code/calc_total_2024_ggpredict.r")
source("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/code/calc_bpue_2024.r")

library(ggplot2)
library(reshape2)
library(data.table)

complete.tb<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/complete_total_bycatch_ecoregion_species_Sept29.csv")
total.bycatch<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/total_bycatch_final_Sept29.csv")

all.metier<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/ecoregion_metier_sps_risk_bycatch_2023.csv")
all.metier$species<-tolower(all.metier$species)
THEDATA<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/clean/monitor_effort_bycatch_ecoregion_area_species_v3_29Sept.csv")

taxon.df<-data.frame(species=unique(THEDATA$species),taxa=NA)
taxon.df$taxa<-THEDATA$taxa[match(taxon.df$species,THEDATA$species)]
commonnames<-read.csv("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/commonnames2023.csv")

commonnames<-commonnames[!duplicated(commonnames$Species),]
commonnames$species<-tolower(commonnames$Species)

#make table bpue first

total.bycatch$taxa<-taxon.df$taxa[match(total.bycatch$species,taxon.df$species)]
total.bycatch$common<-NA
total.bycatch$common<-commonnames$common[match(total.bycatch$species,commonnames$species)]
missingcommon.df<-data.frame(missing=total.bycatch$species[is.na(total.bycatch$common)],common=NA)
missingcommon.df<-missingcommon.df[!duplicated(missingcommon.df$missing),]
library(taxize)

commonnames.missing<-sci2comm(missingcommon.df$missing,db="itis",simplify=FALSE)
library(tidyverse)

commonnames.missing.df<-commonnames.missing %>% 
  enframe() %>% 
  unnest_wider(value) %>% 
  unnest(cols = c(commonName, language))

commonnames.missing.df<-subset(commonnames.missing.df,language=="English")
commonnames.missing.df<-as.data.frame(commonnames.missing.df)
commonnames.missing.df<-commonnames.missing.df[-2,]
commonnames.missing.df<-commonnames.missing.df[!duplicated(commonnames.missing.df$name),c("name","commonName")]
colnames(commonnames.missing.df)<-c("species","common")
commonnames<-rbind(commonnames[,c("species","common")],commonnames.missing.df)
commonnames$taxon<-total.bycatch$taxa[match(commonnames$species,total.bycatch$species)]

fwrite(commonnames,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/commonnames2024.csv")

total.bycatch$common<-commonnames$common[match(total.bycatch$species,commonnames$species)]

stillmissing.df<-data.frame(species=unique(total.bycatch$species[is.na(total.bycatch$common)]),common=NA)

stillmissing.df$common<-c("Longnose velvet dogfish","Birdbeak dogfish","Common dentex","Mouse catshark","hydrolagus mirabilis","Maltese skate")
stillmissing.df$taxon<-"Fish"
commonnames<-rbind(commonnames,stillmissing.df)

fwrite(commonnames,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/commonnames2024.csv")


commonnames<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/commonnames2024.csv")

total.bycatch$common<-commonnames$common[match(total.bycatch$species,commonnames$species)]



table.print<-total.bycatch[,c("ecoregion","metierL4","taxa","species","common","n_ind","daysAtSea","final.fished.daysAtSea","model","bpue","lwr","upr","tbfinal.mean","tbfinal.lwr","tbfinal.upr")]


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

table.print[table.print$species=="pagophilus groenlandicus",]


#### well that's problematic
## fixed! typo in none final version of D3 (SI instead of IS, but somehow rightly allocated to GAS area 17 instead of IS ICES area?)
# D3<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/D3_bycatchevent_2017_2023.csv")
# D3[D3$species=="Pagophilus groenlandicus",]
# ## so it is in monitor construction THEDATA
# THEDATA[THEDATA$species=="Pagophilus groenlandicus",]



#table.print$BPUE<-paste0(formatC(table.print$bpue,digits=5,format="f")," [",table.print$lwr," ; ",table.print$upr,"]")
#table.print$upr_TB<-formatC(as.numeric(table.print$upr_TB),format="d")


 head(table.print[order(table.print$ecoregion,table.print$taxa,table.print$metierL4,table.print$species),],20)

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

fwrite(table.print,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/bpue_table_print.csv")

 head(table.print[order(table.print$Ecoregion,table.print$Taxon,table.print$"metier L4",table.print$Species),],20)

library(systemfonts)
library(flextable) #not quite well built




ft <- flextable(table.print[order(table.print$Ecoregion,table.print$Taxon,table.print$"metier L4",table.print$Species),])
ft <-theme_vanilla(ft)
set_table_properties(ft, width = 1, layout = "autofit")
save_as_docx(
  ft, 
  path = "C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/BPUE_table1.docx")


sum(!is.na(total.bycatch$tbfinal.mean))



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
  path = "C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/TB_table1.docx")



#####

priority1<-c("phocoena phocoena","delphinus delphis","monachus monachus")
priority2<-"Turtles"
priority3<-c("angel shark", "common skate", "guitarfish", "maltese
skate", "great white shark", "sand tiger shark", "smalltooth sand tiger shark",
"spiny butterfly ray", "sturgeon", "balearic shearwater")

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
  path = "C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/priority_table1.docx")



#################################################################################################################
#### rate of achievement

library(ggplot2)


achieved<-subset(total.bycatch,!is.na(tbfinal.mean))
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
 

	
png("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/totalbycatch_mammals.png",width=20,height=35,units="cm",res=200)
mammals
dev.off()


png("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/totalbycatch_turtles.png",width=20,height=10,units="cm",res=200)
reptiles
dev.off()


png("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/totalbycatch_birds.png",width=20,height=35,units="cm",res=200)
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

library(reshape2)
po=melt(tab.E.M)
head(po)
names(po)<-c("Ecoregion","MetierL4","value")

library(ggplot2)
library(scales) # for muted function
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
	
png("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/summary_table_prop_Eco_metier_2024.png",width=30,height=10,units="cm",res=200)
gg.E.M	
dev.off()
##################################################################


tab.S.M<-table(tried[tried$achieved==1,]$species,tried[tried$achieved==1,]$metierL4)/table(tried$species,tried$metierL4)
tab.S.M[which(table(tried$species,tried$metierL4)==0)]<-NA

library(reshape2)
po=melt(tab.S.M)
head(po)
names(po)<-c("Species","MetierL4","value")

library(ggplot2)
library(scales) # for muted function
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
	
png("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/summary_table_prop_Species_metier_2024.png",width=25,height=32,units="cm",res=200)
gg.S.M	
dev.off()



#######################################################
library(systemfonts)
library(flextable) #not quite well built
library(data.table)

table.print<-fread("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/data/clean/Sept29/BPUE_est_2024_Sept29.csv")
table.print<-table.print[,-c(1,7:10,13:16)]
table.print$n_ind<-round(table.print$n_ind,0)
table.print$daysAtSea<-round(table.print$daysAtSea,0)

ft <- flextable(table.print[order(table.print$ecoregion,table.print$species,table.print$metierL4),])
ft <-theme_vanilla(ft)
set_table_properties(ft, width = 1, layout = "autofit")
save_as_docx(
  ft, 
  path = "C:/Users/David/OneDrive - Danmarks Tekniske Universitet/WGBYC/wgbyc2024/results/Appendix1.docx")



### what about those zeros
#probability to observed a rare bpue given monitoring and given that monitoring coverage is 'good'
#very rare bpue: 1/10000
#rare bpue 1/1000

