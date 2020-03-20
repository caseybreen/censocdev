#' Create a dataset of matched census and socsec data
#'
#' @param census census data frame Must have clean_key and n_clean_key columns.
#' @param socsec socsec data frame Must have clean_key and n_clean_key columns.
#' @return a dataframe with matched census and socsec data
#' @keywords internal
#' @export


create_censoc <- function(census,
                          socsec){

  # remove dupes
  socsec_uniq <- socsec[n_clean_key == 1,]
  census_uniq <- census[n_clean_key == 1,]

  # remove "n_clean_key" variable
  socsec_uniq[, n_clean_key := NULL]
  census_uniq[, n_clean_key := NULL]

  # get dupes in each dataset
  socsec_nonuniq_keys <- socsec[n_clean_key > 1,clean_key]
  census_nonuniq_keys <- census[n_clean_key > 1,clean_key]

  # remove non-unique keys from other database
  socsec_uniq <- socsec_uniq[!(clean_key %in% census_nonuniq_keys),]
  census_uniq <- census_uniq[!(clean_key %in% socsec_nonuniq_keys),]

  # reset keys
  setkey(socsec_uniq, clean_key)
  setkey(census_uniq, clean_key)

  # merge on unique keys
  censoc <- merge(census_uniq, socsec_uniq, on = clean_key)


  return(censoc)
}
