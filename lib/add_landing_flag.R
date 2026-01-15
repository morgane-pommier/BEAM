#New function, add_landings_flag.R to flag which fish species are currently dealt with in WGBYC but are also landed commercially in some ecoregion.

add_landings_flag <- function(table.print) {
 
  fish_list <- fread("data/ADGFISHLIST_all.csv")
  fish_list <- clean_chars(fish_list) # Deal with upper case / lowercase mistmatch
  colnames(fish_list)<- tolower(colnames(fish_list))
  colnames(fish_list)[2] <- "species"
  colnames(fish_list)[3] <- "common name"
  colnames(fish_list)[5] <- "species_of_bycatch_relevance_commercially_landed_in_substantial_amount"
  colnames(fish_list)[6] <- "5_consecutive_years_landings_within_10y"
  
  table.print <- table.print[fish_list, on=c("ecoregion", "species"), landed := i.species_of_bycatch_relevance_commercially_landed_in_substantial_amount]
  
  table.print$landed <- ifelse(table.print$landed == "*", "yes", "no")
  
  table.print
  
}