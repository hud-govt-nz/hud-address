#=============================================================================#
# Address extraction
#=============================================================================#
# Addresses are much more complicated and need a lot of regex parsing. We do
# not want to be doing all the regex in R, so we're importing the library from
# Python.
#=============================================================================#
find_path <- function() {
  for (lp in .libPaths()) {
    p <- paste0(lp, "/hud.address")
    if (dir.exists(p)) return(p)
  }
  stop("Cannot find library path!")
}


#' Unabbreviate street types
#'
#' Takes a list of abbreviated street types and returns a list of full street
#' types. Mostly a wrapper to make Python import easier.
#' @name unabbreviate_street_types
#' @param abbrv_street_types List of abbreviated street types
#' @export
unabbreviate_street_types <- function(abbrv_street_types) {
  find_path() %>%
    paste0("/extract_addresses.py") %>%
    reticulate::py_run_file()
  abbrv_street_types %>%
    map(reticulate::py$unabbreviate_street_type) %>%
    unlist()
}


#' Extract street address components
#'
#' Takes a list of street addresses and returns a list of structured address
#' components.
#' @name extract_street_addresses
#' @param street_addresses List of street addresses
#' @param dump_path Location for dumping list of failed extractions
#' @export
#' @examples
#' source_df %>%
#'   left_join(
#'     extract_street_addresses(.$raw_street_addresses, dump_path = "/dump"),
#'     by = "raw_street_addresses",
#'     relationship = "one-to-many") # Raw addresses are often describing many addresses
#'
extract_street_addresses <- function(street_addresses, dump_path = NULL) {
  message(Sys.time(), ": Extracting street addresses...")
  find_path() %>%
    paste0("/extract_addresses.py") %>%
    reticulate::py_run_file()
  res <-
    street_addresses %>%
    unique() %>%
    lapply(function(sa) { # DO NOT use map() here, as errors don't get raised
      if (is.na(sa)) return(NULL)
      extracted_row <-
        sa %>%
        str_replace("\\s{2,}", " ") %>% # Remove extra spaces
        str_replace("\xff", "") %>% # Remove special characters - they cause segfaults bc reasons
        reticulate::py$extract_all_addresses() %>%
        map(function(r) {
          lapply(r, function(x) ifelse(is.null(x), NA, x)) %>%
          unlist()
        }) %>%
        do.call(rbind, .)
      if (is.null(extracted_row)) return(NULL)
      extracted_row %>%
        as_tibble(.name_repair = "minimal") %>%
        setNames(c(
          "unit", "number", "alpha",
          "street_name", "street_type", "street_direction",
          "block_extract_type",
          "unit_extract_type", "unit_extract_error",
          "number_extract_type", "number_extract_error")) %>%
        mutate(
          raw_street_addresses = sa,
          number = as.integer(number))
    }) %>%
    do.call(rbind, .) %>%
    mutate_if(is.character, ~ na_if(., "")) # Clean empty strings

  if (!is.null(dump_path)) {
    unparsed_df <-
      street_addresses %>%
      as_tibble() %>%
      unique() %>%
      filter(
        !is.na(value) &
        !value %in% res$raw_street_addresses) %>%
      arrange(value) %>%
      write_csv(stringr::str_glue("{dump_path}/address_extract_unparsed.csv"))
    message(Sys.time(), stringr::str_glue(": {nrow(unparsed_df)} unparsed addresses."))
  }

  message(Sys.time(), stringr::str_glue(": {nrow(res)} addresses extracted."))
  return(res)
}
