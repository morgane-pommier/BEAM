###legacy plot used in the first 2 implementations of BEAM replaced in 2025

tried<-aggregate(daysatsea~ecoregion+metierl4+species,data=subset(THEDATA,n_ind>0),"sum")

tried$achieved<-0
for (i in 1:nrow(achieved)) {
tried$achieved[which(tried$ecoregion==achieved$ecoregion[i] & tried$metierl4==achieved$metierl4[i] & tried$species==achieved$species[i])]<-1

}

tried$ecoregion<-factor(tried$ecoregion)
tried$metierl4<-factor(tried$metierl4)
tried$species<-factor(tried$species)

tab.E.M<-table(tried[tried$achieved==1,]$ecoregion,tried[tried$achieved==1,]$metierl4)/table(tried$ecoregion,tried$metierl4)
tab.E.M[which(table(tried$ecoregion,tried$metierl4)==0)]<-NA

po=melt(tab.E.M)
head(po)
names(po)<-c("Ecoregion","metierl4","value")

gg.E.M<-ggplot(po, aes(metierl4,Ecoregion)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(label = round(value, 2))) + # write the values
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


tab.S.M<-table(tried[tried$achieved==1,]$species,tried[tried$achieved==1,]$metierl4)/table(tried$species,tried$metierl4)
tab.S.M[which(table(tried$species,tried$metierl4)==0)]<-NA

po=melt(tab.S.M)
head(po)
names(po)<-c("Species","metierl4","value")

gg.S.M<-ggplot(po, aes(metierl4,Species)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  geom_text(aes(label = round(value, 2))) + # write the values
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

