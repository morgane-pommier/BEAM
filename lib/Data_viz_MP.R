
#Data manipulation
library(data.table)
library(dplyr)
library(tidyverse)
library(reshape2) #To manipulate data frames and create matrices
library(scales)


#Data visualisation
library(ggplot2)
library(stringr) #To add uppercase on the plots
library(ggpubr) #To combine plots
library(EnvStats) #To add sample size to ggplots
library(ggalluvial) #For flow chart


### Reading data ###

data_all <- fread("data/obs_agg_data_vis.csv")
data <- fread("data/obs_short_data_vis.csv")

data[, (
  c(
    "monitoring_suitable",
    "taxon_bycatch_monitor_ok",
    "base_model_heterogeneity",
    "ecoregion",
    "taxon",
    "metierl4",
    "bycatch_reported",
    "species",
    "unexplained_heterogeneity",
    "vessellength_group" ,
    "country" ,
    "samplingprotocol" ,
    "year" ,
    "areacode",
    "monitoringmethod" ,
    "metierl5", 
    "bpue_usable", 
    "message", 
    "bycatch_estimated"
  )
) := lapply(.SD, as.factor), .SDcols = c(
  c(
    "monitoring_suitable",
    "taxon_bycatch_monitor_ok",
    "base_model_heterogeneity",
    "ecoregion",
    "taxon",
    "metierl4",
    "bycatch_reported",
    "species",
    "unexplained_heterogeneity",
    "vessellength_group" ,
    "country" ,
    "samplingprotocol" ,
    "year" ,
    "areacode",
    "monitoringmethod" ,
    "metierl5", 
    "bpue_usable", 
    "message", 
    "bycatch_estimated"
  )
)]

#Re-ordering in a south/north order (more or less)
data$ecoregion <- factor(data$ecoregion, levels = c("black sea", "aegean-levantine sea","ionian sea and the central mediterranean sea","adriatic sea", "western mediterranean sea", "bay of biscay and the iberian coast", "azores", "oceanic northeast atlantic", "celtic seas", "greater north sea", "baltic sea", "faroes", "icelandic waters", "norwegian sea", "greenland sea", "barents sea"))


####Data Visualisation####

#Relative effort suitability


Effort <- data[, .(ecoregion, metierl4, taxon,
                   daysatsea, usable_DaS,
                   unused_DaS_monitoring, unused_DaS_sampling, unused_DaS_both,
                   unused_DaS)]

#Compute proportions by group

Effort[, Total_DaS := usable_DaS + unused_DaS]

Effort[, Prop_suitable := fifelse(Total_DaS > 0, usable_DaS * 100 / Total_DaS, 0)]
Effort[, Prop_unsuitable_monitoring := fifelse(Total_DaS > 0, unused_DaS_monitoring * 100 / Total_DaS, 0)]
Effort[, Prop_unsuitable_sampling   := fifelse(Total_DaS > 0, unused_DaS_sampling   * 100 / Total_DaS, 0)]
Effort[, Prop_unsuitable_both      := fifelse(Total_DaS > 0, unused_DaS_both       * 100 / Total_DaS, 0)]

Effort_long <- Effort %>%
  pivot_longer(
    cols = starts_with("Prop_"),
    names_to = "suitability",
    values_to = "proportion"
  )

Effort_long$suitability <- dplyr::recode_factor(
  Effort_long$suitability,
  "Prop_unsuitable_monitoring" = "Unsuitable monitoring",
  "Prop_unsuitable_sampling"   = "Unsuitable sampling protocol",
  "Prop_unsuitable_both"       = "Unsuitable monitoring & sampling protocol",
  "Prop_suitable"              = "Suitable"
)


#Per ecoregion, across all metierl4 and taxa 
Effort_ecoregion <- ggplot(Effort_long, aes(x = ecoregion, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of effort (total DaS)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_title)

ggsave(Effort_ecoregion,file="results/Effort_suitability_ecoregion.png", width = 210, height = 145, units = "mm")

#Per métierL4, across all ecoregions and taxa 
Effort_metier <- ggplot(Effort_long, aes(x = metierl4, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of effort (total DaS)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_upper)

ggsave(Effort_metier,file="results/Effort_suitability_metier.png", width = 210, height = 145, units = "mm")

#Per taxa, across all ecoregions and métier L4 
Effort_taxon <- ggplot(Effort_long, aes(x = taxon, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of effort (total DaS)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_title)

ggsave(Effort_taxon,file="results/Effort_suitability_taxon.png", width = 210, height = 145, units = "mm")

#Function to put upper case in the facet labels
capitalize_labeller <- function(variable, value) {
  str_to_upper(value)
}


Effort <- setDT(Effort)

#Facetted plot
Facet_Effort <- ggplot(Effort_long, aes(x = ecoregion, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width = .7) +
  labs(x = "", y = "Relative proportion of suitable vs. unsuitable effort (total DaS)",fill=NULL)  +coord_flip() + scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                                                                                                                                     guide = guide_legend(reverse = TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1), labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(Facet_Effort,file="results/Effort_suitability.png", width = 297, height = 210, units = "mm")

#Number of individuals bycaught

bycatch <- data[, .(n_ind = sum(n_ind)), by = .(ecoregion, metierl4,taxon)]

Facet_bycatch <- ggplot(bycatch, aes(x = ecoregion, fill = n_ind, y= 1)) +
  geom_tile(color = "white", width=.7)+
  scale_fill_continuous_sequential(palette = "OrYel", trans = "log10", na.value= "grey90",labels = trans_format("log10", math_format(10^.x)))+
  facet_grid(taxon~metierl4)+
  labs(x = "", y = "", fill = "")+coord_flip()+theme(legend.position = "top", strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_blank())+facet_grid(taxon~metierl4, labeller = capitalize_labeller)+ scale_x_discrete(labels=str_to_title)+
  geom_point(data = bycatch, aes(size="0"), shape = NA, colour = "grey95")+
  guides(size=guide_legend("Number of individuals bycaught", override.aes=list(shape=15, size = 10)))

ggsave(Facet_bycatch,file="results/Bycatch_Records.png", width = 297, height = 210, units = "mm")

#Amount of bycatch records discarded 

N_ind <- data[n_ind > 0 | unused_n_ind > 0, .(ecoregion, metierl4, taxon,
                   usable_n_ind,
                   unused_n_ind_monitoring, unused_n_ind_sampling, unused_n_ind_both,
                   unused_n_ind)]

#Compute proportions by group

N_ind[, Total_n_ind := usable_n_ind + unused_n_ind]

N_ind[, Prop_suitable := fifelse(Total_n_ind > 0, usable_n_ind * 100 / Total_n_ind, 0)]
N_ind[, Prop_unsuitable_monitoring := fifelse(Total_n_ind > 0, unused_n_ind_monitoring * 100 / Total_n_ind, 0)]
N_ind[, Prop_unsuitable_sampling   := fifelse(Total_n_ind > 0, unused_n_ind_sampling   * 100 / Total_n_ind, 0)]
N_ind[, Prop_unsuitable_both      := fifelse(Total_n_ind > 0, unused_n_ind_both       * 100 / Total_n_ind, 0)]

N_ind_long <- N_ind %>%
  pivot_longer(
    cols = starts_with("Prop_"),
    names_to = "suitability",
    values_to = "proportion"
  )

N_ind_long$suitability <- dplyr::recode_factor(
  N_ind_long$suitability,
  "Prop_unsuitable_monitoring" = "Discarded records - monitoring",
  "Prop_unsuitable_sampling"   = "Discarded records -  sampling protocol",
  "Prop_unsuitable_both"       = "Discarded records -  monitoring & sampling protocol",
  "Prop_suitable"              = "Records used in BEAM"
)


#Per ecoregion, across all metierl4 and taxa 
Records_ecoregion <- ggplot(N_ind_long, aes(x = ecoregion, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of N_ind (total n_ind)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_title)

ggsave(Records_ecoregion,file="results/Discarded_records_ecoregion.png", width = 210, height = 145, units = "mm")

#Per métierL4, across all ecoregions and taxa 
Records_metier <- ggplot(N_ind_long, aes(x = metierl4, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of N_ind (total n_ind)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_upper)

ggsave(Records_metier,file="results/Discarded_records_metier.png", width = 210, height = 145, units = "mm")

#Per taxa, across all ecoregions and métier L4 
Records_taxon <- ggplot(N_ind_long, aes(x = taxon, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of N_ind (total n_ind)", fill = NULL) +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                    guide = guide_legend(reverse = TRUE)) +
  theme(legend.position = "top",
        axis.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 12, hjust = 1, face = "bold")) +
  scale_x_discrete(labels = str_to_title)

ggsave(Records_taxon,file="results/Discarded_records_taxon.png", width = 210, height = 145, units = "mm")

#Function to put upper case in the facet labels
capitalize_labeller <- function(variable, value) {
  str_to_upper(value)
}


N_ind <- setDT(N_ind)

#Facetted plot
Facet_n_ind <- ggplot(N_ind_long, aes(x = ecoregion, y = proportion, fill = suitability)) +
  geom_bar(stat = "identity", position = "fill", width=.7) +
  labs(x = "", y = "Relative proportion of suitable vs. unsuitable N_ind (total n_ind)",fill=NULL)  +coord_flip() + scale_fill_manual(values = c("#f48c06", "#ffba08", "#dc2f02", "#80b918"),
                                                                                                                                     guide = guide_legend(reverse = TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1),labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(Facet_n_ind,file="results/Discarded_records.png", width = 297, height = 210, units = "mm")

## BPUE availability

BPUE_availability_all <- ggplot(data, aes(x = ecoregion, fill = as.factor(bpue_available))) +
  geom_bar(position = "fill", col="grey80", width=.7) + coord_flip() + labs(y="Proportion of species", x="", fill="BPUE estimate available")+coord_flip() + scale_fill_manual(values = c( "#ef233c","#80b918"), labels = c("No", "Yes"),guide = guide_legend(reverse=TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4,drop=FALSE, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1),labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(BPUE_availability_all,file="results/BPUE_availability_all.png", width = 297, height = 210, units = "mm")

#Proportion of species for which bycatch is reported

BPUE_availability_bycatch_reported <-ggplot(data[bycatch_reported=="yes"], aes(x = ecoregion, fill = as.factor(bpue_available))) +
  geom_bar(position = "fill", col="grey80", width=.7) + coord_flip() + labs(y="Proportion of species for which bycatch has been recorded in the WGBYC dataset", x="", fill="BPUE estimate available")+coord_flip() + scale_fill_manual(values = c( "#ef233c","#80b918"), labels = c("No", "Yes"),guide = guide_legend(reverse=TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4,drop=FALSE, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1),labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(BPUE_availability_bycatch_reported,file="results/BPUE_availability_bycatch_reported.png", width = 297, height = 210, units = "mm")

#Total bycatch

#When bycatch is reported

data[bycatch_estimated != "no" & bycatch_estimated != "yes" & bpue_available == 0, bycatch_estimated := "no"  ]


Total_bycatch_estimated_bycatch_reported <-ggplot(data[bycatch_reported=="yes"], aes(x = ecoregion, fill = as.factor(bycatch_estimated))) +
  geom_bar(position = "fill", col="grey80", width=.7) + coord_flip() + labs(y="Proportion of species for which bycatch has been recorded in the WGBYC dataset", x="", fill="Total bycatch estimate available")+coord_flip() + scale_fill_manual(values = c( "#ef233c","#80b918"), labels = c("No", "Yes"),guide = guide_legend(reverse=TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4,drop=FALSE, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1),labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(Total_bycatch_estimated_bycatch_reported,file="results/Total_bycatch_estimated.png", width = 297, height = 210, units = "mm")

#When a BPUE is available

Total_bycatch_estimated_BPUE_available <-ggplot(data[bpue_available==1], aes(x = ecoregion, fill = as.factor(bycatch_estimated))) +
  geom_bar(position = "fill", col="grey80", width=.7) + coord_flip() + labs(y="Proportion of species for which a BPUE is available", x="", fill="Total bycatch estimate available")+coord_flip() + scale_fill_manual(values = c( "#ef233c","#80b918"), labels = c("No", "Yes"),guide = guide_legend(reverse=TRUE))+theme(legend.position = "top")+theme(strip.background = element_rect(fill="white"),panel.background = element_rect(fill = "grey95"), axis.title=element_text(size=10), axis.text = element_text(size=7),axis.text.x = element_text(size=6))+facet_grid(taxon~metierl4,drop=FALSE, labeller = capitalize_labeller)+scale_y_continuous(breaks = c(0,0.5,1),labels = c(0,0.5,1))+ scale_x_discrete(labels=str_to_title)

ggsave(Total_bycatch_estimated_BPUE_available,file="results/Total_bycatch_estimated_BPUE_available.png", width = 297, height = 210, units = "mm")


# Heterogeneity

bpue <- data[bpue_available=="1", ]

bpue$base_model_heterogeneity <- factor(bpue$base_model_heterogeneity)

levels(bpue$base_model_heterogeneity) <- c("NA", "No", "Yes")

pie_data <- data.frame(
  group=levels(bpue$base_model_heterogeneity),
  value=table(bpue$base_model_heterogeneity)
)

pie_data$group <- as.factor(pie_data$group)
levels(pie_data$group) <- c("NA", "No", "Yes")

pie <- ggplot(pie_data, aes(x="", y=value.Freq, fill=group)) +
  geom_bar(stat="identity", width=1, color="white", alpha = .7) +
  coord_polar("y", start=0) +
  geom_text(aes(label = c("1%", "82.5%","16.5%" )),color=c("black", "white","white"),size=6,position = position_stack(vjust = 0.5))+
  theme_void() +labs(fill="Estimate Heterogeneity")+ scale_fill_manual(values=c("grey95","#3a86ff", "orange"))+theme(legend.position = "top")


n_re <- ggplot(bpue) + geom_bar(aes(x=base_model_heterogeneity, fill=as.factor(n_re)), position = "fill", col="white", alpha=.7) + theme_minimal() +labs(x= "Base model heterogeneity", y="Relative frequency", fill= "Number of random effect")+theme(legend.position = "right") + scale_fill_manual(values=c("#3a86ff", "#ffba08", "#faa307", "#f48c06", "#e85d04", "#dc2f02", "#d00000"))+
  guides(fill = guide_legend(title.position = "top"))


combined <- ggarrange(pie, n_re)

ggsave(combined,file="results/Heterogeneity_and_random_effects.png", width = 210, height = 145, units = "mm")

Re_ecoregion <- ggplot(bpue) + geom_bar(aes(x=ecoregion, fill=as.factor(n_re)), position = "stack", col="white", alpha=.7, width=.7) + theme_minimal() +labs(x= "", y="Relative frequency", fill= "Number of random effect")+theme(legend.position = "right") + scale_fill_manual(values=c("#3a86ff", "#ffba08", "#faa307", "#f48c06", "#e85d04", "#dc2f02", "#d00000"))+
  guides(fill = guide_legend(title.position = "top"))+coord_flip()+ scale_x_discrete(labels=str_to_title)

ggsave(Re_ecoregion,file="results/N_random_effects_ecoregion.png", width = 210, height = 145, units = "mm")


Re_metier <-ggplot(bpue) + geom_bar(aes(x=metierl4, fill=as.factor(n_re)), position = "stack", col="white", alpha=.7, width=.7) + theme_minimal() +labs(x= "", y="Relative frequency", fill= "Number of random effect")+theme(legend.position = "right") + scale_fill_manual(values=c("#3a86ff", "#ffba08", "#faa307", "#f48c06", "#e85d04", "#dc2f02", "#d00000"))+
  guides(fill = guide_legend(title.position = "top"))+coord_flip()+ scale_x_discrete(labels=str_to_upper)

ggsave(Re_metier,file="results/N_random_effects_metier.png", width = 210, height = 145, units = "mm")

Re_taxon <-ggplot(bpue) + geom_bar(aes(x=taxon, fill=as.factor(n_re)), position = "stack", col="white", alpha=.7, width=.7) + theme_minimal() +labs(x= "", y="Relative frequency", fill= "Number of random effect")+theme(legend.position = "right") + scale_fill_manual(values=c("#3a86ff", "#ffba08", "#faa307", "#f48c06", "#e85d04", "#dc2f02", "#d00000"))+
  guides(fill = guide_legend(title.position = "top"))+coord_flip()+ scale_x_discrete(labels=str_to_title)

ggsave(Re_taxon,file="results/N_random_effects_taxon.png", width = 210, height = 145, units = "mm")



# Barplot of variables being used as random effect, when base model showed heterogeneity or not

long_data <- bpue %>%
  pivot_longer(cols = c(vessellength_group, year, monitoringmethod, samplingprotocol, areacode, country, metierl5),names_to = "x_variable",values_to = "x_value")

# New facet label names for supp variable
supp.labs <- c("Base model heterogeneity = NA",  "Base model heterogeneity = No","Base model heterogeneity = Yes")
names(supp.labs) <- c("NA", "No", "Yes")

Random_effects <- ggplot(long_data, aes(x = x_variable, fill = x_value)) +
  geom_bar(position="fill", alpha=.7) +
  facet_wrap(~ base_model_heterogeneity, labeller = labeller(base_model_heterogeneity = supp.labs), nrow =3) +
  labs(x = "", y = "Relative frequency", fill = "Variable used as a random effect in the final model") +
  theme_minimal() + scale_fill_manual(values=c("#3a86ff", "orange"), labels = c("No", "Yes"))+theme(legend.position = "top")+ scale_x_discrete(labels=c('ICES Area Code', 'Country', 'Metier L5', "Monitoring method", "Sampling protocol", "Vessel Length Group", "Year"))

ggsave(Random_effects,file="results/Random_effect_prevalence.png", width = 210, height = 145, units = "mm")
