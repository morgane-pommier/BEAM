
annotate_bpue <- function(bpue, cols) {
    #Reading total fishing effort to later compute monitoring coverage
    all_fishing_effort <- fread("data/fishing_all_years.csv")
    
    #Aggregating daysatsea across same levels used in calc_bpue
    all_fishing_effort <- all_fishing_effort[, .(
        daysatseaf = sum(daysatseaf, na.rm = TRUE)), by = cols]
    
    #Retrieve n_ind and daysatsea information (model inputs) from obs3, aggregated at the same level used in calc_bpue
    annotations <- obs3[taxon_bycatch_monitor_ok==TRUE, .(
        daysatsea = sum(daysatsea, na.rm = TRUE), n_ind = sum(n_ind, na.rm=T)), by =c("species", cols)]
    
    #Joining annotations to original bpue model
    
    bpue2 <- copy(bpue)

    bpue2[annotations, on = c("species", cols), c("daysatsea", "n_ind") := list(i.daysatsea, i.n_ind)]
    
    bpue2[all_fishing_effort, on = cols, daysatseaf := i.daysatseaf]
    
    bpue2[,monitoring_coverage := daysatsea / daysatseaf]
    bpue2
}





