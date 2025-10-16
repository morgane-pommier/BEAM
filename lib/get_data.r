########################################################
##### request access to data
##### once data access is accepted you will receive an email to login at
##### https://data.ices.dk/token
#####  there you will generate a token which you need to copy and paste in the function below 
##### or assign it to an object eg, token<-"ewioubreuicvepb" which you can call in the function
##### do note the username: it should be the email (eg louis.attack@dartmouth.edu) you used to 
##### make the access request


get_data<-function(username="",token="",assessmentyear=THISYEAR) {

require(icesConnect)
require(httr)
require(jsonlite)
icesConnect::set_username(username)
observedeffort<-THISYEAR-1
linkD1<-paste0("https://bycatch.ices.dk/api/GetD1_Fishing_effort?Year=",observedeffort)
linkD2<-"https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort"
linkD3<-"https://bycatch.ices.dk/api/GetD3_BycatchEvent"
linkD4<-paste0("https://bycatch.ices.dk/api/GetOverviewSubmissionTable/",THISYEAR)
linkD5<-"https://bycatch.ices.dk/api/GetByCatchRoadMapListSpecies"
# we download it at the moment even though we create our own list. however, in the future D5 will
# be the list we generate once it stabilises

resp1<-ices_get_jwt(linkD1,username=username,jwt=token)
D1<-content(resp1,"text")
D1<-fromJSON(D1)

resp2<-ices_get_jwt(linkD2,username=username,jwt=token)
D2<-content(resp1,"text")
D2<-fromJSON(D2)

resp3<-ices_get_jwt(linkD3,username=username,jwt=token)
D3<-content(resp3,"text")
D3<-fromJSON(D3)

resp4<-ices_get_jwt(linkD4,username=username,jwt=token)
D4<-content(resp4,"text")
D4<-fromJSON(D4)

rm(list(resp1,resp2,resp3,resp4))
fwrite(D1,file="data/D1_fishingeffort.csv",na="NA")
fwrite(subset(D2,year>2016),file="data/D2_monitoringeffort.csv",na="NA")
fwrite(subset(D3,year>2016),file="data/D3_bycatchevent.csv",na="NA")
fwrite(D4,file="data/D4_overviewsubmission.csv",na="NA")

}
