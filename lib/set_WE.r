###########################
#### initialise the work environment
#### set a few global variables from prompt
#### download the list
###############################

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

## obtain THISYEAR as the year of the assessment
THISYEAR<<-as.numeric(readline(prompt="what is the year of the assessment (typically year of the data call in yyyy format):"))
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

# mediterranean$aphiaid[is.na(mediterranean$aphiaid)]<-annex01_species$aphiaid[
# match(mediterranean$species[is.na(mediterranean$aphiaid)],tolower(annex01_species$species))]

# mediterranean$aphiaid[mediterranean$species=="monachus monachus"]<-137009
# mediterranean$aphiaid[mediterranean$species=="puffinus yelkouan"]<-137204

# write.csv(mediterranean,file=file.path(path,"data/Med_Annex_1_WGBYC_2025.csv"))


