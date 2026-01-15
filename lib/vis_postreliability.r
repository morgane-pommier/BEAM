############################################################
##### produces plot post reliability check
#####################################################

#just to be sure
library(ggplot2)

achievedpostQC<-table.print
#let's deal with pesky variable names needed for printing
names(achievedpostQC)[grepl("total bycatch",names(achievedpostQC))]<-"TB"
names(achievedpostQC)[grepl("TB lower",names(achievedpostQC))]<-"TB_lower"
names(achievedpostQC)[grepl("TB upper",names(achievedpostQC))]<-"TB_upper"

achievedpostQC<-subset(achievedpostQC,!is.na(TB))

achievedpostQC$label<-paste(str_to_title(achievedpostQC$"Common name"),str_to_title(achievedpostQC$ecoregion),toupper(achievedpostQC$"metierl4"),sep=" and ")



mammals_all<-ggplot(subset(achievedpostQC,Taxon=="mammals"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper,fill = reliability), width = 0.5,fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_log10()
  
mammals_reliable<-ggplot(subset(achievedpostQC,Taxon=="mammals"&reliability=="more reliable"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

df<-subset(achievedpostQC, Taxon == "mammals"&reliability=="more reliable")%>%
mutate(label = fct_reorder(label, TB, .desc = TRUE))

mammals_sorted<-ggplot(df, aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

labs <- table.print %>% distinct(flag_description, color)

mammals_bivariate_scale<-ggplot(subset(achievedpostQC,Taxon=="mammals"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper, fill=color), width = 0.5,fatten=0.5) +
  coord_flip()+
  scale_fill_identity(
    name   = NULL, 
    guide  = "legend",
    breaks = labs$color,          
    labels = labs$flag_description 
  ) +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()+theme(legend.margin = margin(t = 0, r = 0, b = 0, l = -300, unit = "pt"),
                        legend.justification = c(0,1), legend.position = "bottom",
  )+guides(fill = guide_legend(nrow = 6))

##################################################
### seabirds

seabirds_all<-ggplot(subset(achievedpostQC,Taxon=="seabirds"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper,fill = reliability), width = 0.5,fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_log10()
  
seabirds_reliable<-ggplot(subset(achievedpostQC,Taxon=="seabirds"&reliability=="more reliable"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

df<-subset(achievedpostQC, Taxon == "seabirds"&reliability=="more reliable")%>%
mutate(label = fct_reorder(label, TB, .desc = TRUE))

seabirds_sorted<-ggplot(df, aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

labs <- table.print %>% distinct(flag_description, color)

seabirds_bivariate_scale<-ggplot(subset(achievedpostQC,Taxon=="seabirds"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper, fill=color), width = 0.5,fatten=0.5) +
  coord_flip()+
  scale_fill_identity(
    name   = NULL, 
    guide  = "legend",
    breaks = labs$color,          
    labels = labs$flag_description 
  ) +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()+theme(legend.margin = margin(t = 0, r = 0, b = 0, l = -300, unit = "pt"),
                        legend.justification = c(0,1), legend.position = "bottom",
  )+guides(fill = guide_legend(nrow = 6))

##################################################
### turtles

turtles_all<-ggplot(subset(achievedpostQC,Taxon=="turtles"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper,fill = reliability), width = 0.5,fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_log10()
  
turtles_reliable<-ggplot(subset(achievedpostQC,Taxon=="turtles"&reliability=="more reliable"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

df<-subset(achievedpostQC, Taxon == "turtles"&reliability=="more reliable")%>%
mutate(label = fct_reorder(label, TB, .desc = TRUE))

turtles_sorted<-ggplot(df, aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

labs <- table.print %>% distinct(flag_description, color)

turtles_bivariate_scale<-ggplot(subset(achievedpostQC,Taxon=="turtles"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper, fill=color), width = 0.5,fatten=0.5) +
  coord_flip()+
  scale_fill_identity(
    name   = NULL, 
    guide  = "legend",
    breaks = labs$color,          
    labels = labs$flag_description 
  ) +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()+theme(legend.margin = margin(t = 0, r = 0, b = 0, l = -300, unit = "pt"),
                        legend.justification = c(0,1), legend.position = "bottom",
  )+guides(fill = guide_legend(nrow = 6))


###################################################
### elasmobranchs

elasmobranchs_all<-ggplot(subset(achievedpostQC,Taxon=="elasmobranchs"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper,fill = reliability), width = 0.5,fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_log10()
  
elasmobranchs_reliable<-ggplot(subset(achievedpostQC,Taxon=="elasmobranchs"&reliability=="more reliable"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

df<-subset(achievedpostQC, Taxon == "elasmobranchs"&reliability=="more reliable")%>%
mutate(label = fct_reorder(label, TB, .desc = TRUE))

elasmobranchs_sorted<-ggplot(df, aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

labs <- table.print %>% distinct(flag_description, color)

elasmobranchs_bivariate_scale<-ggplot(subset(achievedpostQC,Taxon=="elasmobranchs"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper, fill=color), width = 0.5,fatten=0.5) +
  coord_flip()+
  scale_fill_identity(
    name   = NULL, 
    guide  = "legend",
    breaks = labs$color,          
    labels = labs$flag_description 
  ) +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()+theme(legend.margin = margin(t = 0, r = 0, b = 0, l = -300, unit = "pt"),
                        legend.justification = c(0,1), legend.position = "bottom",
  )+guides(fill = guide_legend(nrow = 6))


###################################################
#### fish

fish_all<-ggplot(subset(achievedpostQC,Taxon=="fish"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper,fill = reliability), width = 0.5,fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_fill_manual(values=c("lightblue","orange"))+
  scale_y_log10()
  
fish_reliable<-ggplot(subset(achievedpostQC,Taxon=="fish"&reliability=="more reliable"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

df<-subset(achievedpostQC, Taxon == "fish"&reliability=="more reliable")%>%
mutate(label = fct_reorder(label, TB, .desc = TRUE))

fish_sorted<-ggplot(df, aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper), width = 0.5,fill = "lightblue",fatten=0.5) +
  coord_flip() +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()

 labs <- table.print %>% distinct(flag_description, color)

fish_bivariate_scale<-ggplot(subset(achievedpostQC,Taxon=="fish"), aes(x = label, y=TB)) +
  geom_crossbar(aes(ymin =TB_lower, ymax = TB_upper, fill=color), width = 0.5,fatten=0.5) +
  coord_flip()+
  scale_fill_identity(
    name   = NULL, 
    guide  = "legend",
    breaks = labs$color,          
    labels = labs$flag_description 
  ) +
  xlab("Ecoregion, Metier level 4, Species") + 
  ylab("Total Bycatch (individuals)")+
  theme_minimal()+
  scale_y_log10()+theme(legend.margin = margin(t = 0, r = 0, b = 0, l = -300, unit = "pt"),
                        legend.justification = c(0,1), legend.position = "bottom",
  )+guides(fill = guide_legend(nrow = 6))

	
png("results/totalbycatch_mammals_reliability_labelled.png",width=22,height=35,units="cm",res=200)
mammals_all
dev.off()

png("results/totalbycatch_mammals_reliability_check.png",width=22,height=35,units="cm",res=200)
mammals_reliable
dev.off()

png("results/totalbycatch_mammals_reliability_check_ordered.png",width=22,height=35,units="cm",res=200)
mammals_sorted
dev.off()

png("results/totalbycatch_mammals_reliability_bivariate.png",width=22,height=35,units="cm",res=200)
mammals_bivariate_scale
dev.off()
##################	
png("results/totalbycatch_seabirds_reliability_labelled.png",width=22,height=35,units="cm",res=200)
seabirds_all
dev.off()

png("results/totalbycatch_seabirds_reliability_check.png",width=22,height=35,units="cm",res=200)
seabirds_reliable
dev.off()

png("results/totalbycatch_seabirds_reliability_check_ordered.png",width=22,height=35,units="cm",res=200)
seabirds_sorted
dev.off()

png("results/totalbycatch_seabirds_reliability_bivariate.png",width=22,height=35,units="cm",res=200)
seabirds_bivariate_scale
dev.off()

##############	
png("results/totalbycatch_turtles_reliability_labelled.png",width=22,height=35,units="cm",res=200)
turtles_all
dev.off()

png("results/totalbycatch_turtles_reliability_check.png",width=22,height=35,units="cm",res=200)
turtles_reliable
dev.off()

png("results/totalbycatch_turtles_reliability_check_ordered.png",width=22,height=35,units="cm",res=200)
turtles_sorted
dev.off()

png("results/totalbycatch_turtles_reliability_bivariate.png",width=22,height=35,units="cm",res=200)
turtles_bivariate_scale
dev.off()


##############	
png("results/totalbycatch_elasmobranchs_reliability_labelled.png",width=22,height=35,units="cm",res=200)
elasmobranchs_all
dev.off()

png("results/totalbycatch_elasmobranchs_reliability_check.png",width=22,height=35,units="cm",res=200)
elasmobranchs_reliable
dev.off()

png("results/totalbycatch_elasmobranchs_reliability_check_ordered.png",width=22,height=35,units="cm",res=200)
elasmobranchs_sorted
dev.off()

png("results/totalbycatch_elasmobranchs_reliability_bivariate.png",width=22,height=35,units="cm",res=200)
elasmobranchs_bivariate_scale
dev.off()

##############	
png("results/totalbycatch_fish_reliability_labelled.png",width=22,height=35,units="cm",res=200)
fish_all
dev.off()

png("results/totalbycatch_fish_reliability_check.png",width=22,height=35,units="cm",res=200)
fish_reliable
dev.off()

png("results/totalbycatch_fish_reliability_check_ordered.png",width=22,height=35,units="cm",res=200)
fish_sorted
dev.off()

png("results/totalbycatch_fish_reliability_bivariate.png",width=22,height=35,units="cm",res=200)
fish_bivariate_scale
dev.off()

