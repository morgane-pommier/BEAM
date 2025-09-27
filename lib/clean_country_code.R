
#' @import countrycode
#' Converts country code from ISO two letters to ISO three letters,
#' and fixes known data errors
#' 
#' @description
#' Takes a character vector of two-letter country codes, and finds the 
#' corresponding three-letter country code, in a conservative way, so that if
#' there are any cases where it can't find a 3-letter country code, the original
#' country code is left intact. This is necessary becaus eome 2-letter ISO 
#' country code are homonyms of R functions ("de" and "se").
#' 
#' Addditionally, there may be some data specific issues that are taken care of
#' here, but these are not documented beyond their appearance in the function body.
#'
#' @details
#' The object `x` is not changed.
#' 
#' @param x A character vector
#' @returns A new character vector, of the same length as the input.
#' @export
# 
clean_country_code <- function(x) {
    # manual correction(s):
    x[x == "SI"] <- "IS"
    
    # to be robust to repeated calls, only process codes that are actually 2 letters
    which_iso2c <- which(nchar(x) == 2)
    
    # Converts country code from ISO two letters to ISO three letters
    iso3c <- countrycode::countrycode(x[which_iso2c], origin = "iso2c", destination = "iso3c")
    which_na <- which(is.na(iso3c))
    iso3c[which_na] <- x[which_na]
    
    x[which_iso2c] <- iso3c
    x
} 
