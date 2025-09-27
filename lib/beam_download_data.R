########################################################
##### request access to data (e.g. speak to Carlos, or email him at carlos@ices.dk)
##### once data access is accepted you will receive an email to login at
##### https://data.ices.dk/token
#####  there you will generate a token which you need to copy and paste in the function below 
##### or assign it to an object eg, token<-"ewioubreuicvepb" which you can call in the function
##### do note the username: it should be the email (eg louis.attack@dartmouth.edu) you used to 
##### make the access request


beam_download_data <- function(username, token, years, overwrite = FALSE, verbose = TRUE) {

    ices_api_urls <- list(
        D1 = "https://bycatch.ices.dk/api/GetD1_Fishing_effort?Year=%s",
        D2 = "https://bycatch.ices.dk/api/GetD2_Bycatch_monitoring_effort?Year=%s",
        D3 = "https://bycatch.ices.dk/api/GetD3_BycatchEvent?Year=%s",
        D4 = "https://bycatch.ices.dk/api/GetOverviewSubmissionTable/%d"
    )
    
    grab_and_save <- function(api, file, report = TRUE) {
        
        if (file.exists(file) & overwrite == FALSE) {
            if (verbose == TRUE) {
                cat(sprintf("BEAM: %s already exists, %s skipped! (rerun with overwrite=TRUE to overwrite)\n", 
                            file, api))
            }
            return()
        }
        
        if (verbose == TRUE) {
            cat(sprintf("BEAM: Downloading data for %s-%s from ICES database %s...", 
                        min(years), max(years), api))
        }
        
        url <- ices_api_urls[[api]]
        
        if (api %in% c("D1", "D2", "D3")) {
            url <- sprintf(url, paste0(years, collapse = ","))
            
            
        } else if (api %in% "D4") {
            url <- sprintf(url, max(years))
        }
        
        resp <- icesConnect::ices_get_jwt(url, username = username, jwt = token, quiet = FALSE)
        dat <- jsonlite::fromJSON(httr::content(resp, as = "text"))
        fwrite(dat, file = file, sep = ";")
        
        if (verbose == TRUE) {
            cat(sprintf("\rBEAM: Download complete, data from %s saved as %s\n", api, file))
        }
    }

    grab_and_save("D1", "data/all1.csv")
    grab_and_save("D2", "data/obs1.csv")
    grab_and_save("D3", "data/bycatch1.csv")
    grab_and_save("D4", "data/D4_overviewsubmission.csv")
    
}
