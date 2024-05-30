#' Match NZAD addresses
#'
#' Takes a list of partially/fully structured addresses and return matching row
#' from NZAD (or something that looks like the NZAD) dataset.
#'
#' Unstructured columns can also be used to add messy suburb/town_city/ta/regc
#' data.
#'
#' @name match_nzad_addresses
#' @param targ_df Records we're trying to match.
#' @param structured_cols List of structured data (e.g. Street number/street
#'   name) columns which exist in both frames.
#' @param unstructured_cols List of comma-separated unstructured data (e.g.
#'   Address blocks in the form of "Willis Street, Wellington Central,
#'   Wellington") in targ_df.
#' @param guess_cols List of structured data in candidates_df which we're
#'   trying to match the unstructured_cols to.
#' @param nzad_df NZAD data from [Source].[NZPost_NZAD_Address] (you can
#'   preprocess this, e.g. Only subset a TA).
#' @param dump_path Path for validation output.
#' @param drop_multi_results If more than one result is returned for a given
#'   address, return a single blank placeholder instead. This is useful if you
#'   expected a one- to-one join.
#' @export
#' @examples
#' match_nzad_addresses(
#'   targ_df = sites_df,
#'   nzad_df = nzad_df,
#'   structured_cols = c("street_name", "street_type", "street_direction"), # Exists in both
#'   unstructured_cols = c("prj_raw_area"), # Unstructured area string (e.g. "Island Bay, Wellington")
#'   guess_cols = c("suburb_name", "town_city_name", "ta_name", "regc_name"))
#' 
match_nzad_addresses <- function(targ_df, structured_cols, unstructured_cols, guess_cols,
                                 nzad_df, dump_path = NULL, drop_multi_results = TRUE) {
  message(Sys.time(), stringr::str_glue(": Matching {nrow(targ_df)} records to NZAD addresses..."))  
  # Check that targ_df has the expected columns
  tryCatch(
    targ_df %>% select(all_of(c("unit", "number", "alpha", "street_name", "street_type"))) -> m,
    error = function(e) {
      message("\033[1;33mtarg_df needs `unit`, `number`, `alpha`, `street_name`, `street_type` to match to NZAD addresses!\033[0m")
      message("\033[1;33mUse extract_street_addresses() to break address strings into components.\033[0m")
      stop(e)
    })

  # Prepare addresses
  streets_df <-
    nzad_df %>%
    select(
      street_id, street_name, street_type, street_direction,
      suburb_id, suburb_name, town_city_id, town_city_name,
      ta_id, ta_name, regc_id, regc_name) %>%
    unique()

  # Clean street type which are unrecognised by NZAD are considered part of the
  # street name, e.g.:
  #   From : {street_name: "Hazelhurst", street_type: "Pass"}
  #   To   : {street_name: "Hazelhurst Pass", "street_type": NA}
  #
  # Definitive street names like "The Terrace", where the street type are
  # recognised should be combined, e.g.:
  #   From : {street_name: "The", street_type: "Terrace"}
  #   To   : {street_name: "The Terrace", street_type: NA}
  prepped_df <-
    targ_df %>%
    mutate(
      to_combine =
        !is.na(street_name) & !is.na(street_type) &
        (!street_type %in% unique(streets_df$street_type) | street_name == "The"),
      street_name =
        ifelse(to_combine, paste(street_name, street_type), street_name),
      street_type =
        ifelse(to_combine, NA, street_type)) %>%
    select(-to_combine)

  # Do a best-effort street-level (as opposed to address-level) match first.
  # This is a much less expensive match, but street + suburb/town is very
  # effective at filtering out false positives. The catch is that suburb/town
  # are often ambiguous, so we need to deploy a buch of fuzzy match techniques
  # to use them properly.
  #
  # col_weights and min_raw_score are structured to strongly weight
  # street_name and street_type. If those do not match, it will not be
  # possible to get over the min_raw_score. The role of the other values is
  # to act as tiebreakers (e.g. If multiple streets have the same name and
  # type, then use the other values to decide)
  street_match_df <-
    prepped_df %>%
    mutate(
      hud.address::match_address_piles(
        .,
        streets_df,
        structured_cols, unstructured_cols, guess_cols,
        col_weights = c(
          street_name = 200,
          street_type = 150,
          street_direction = 12,
          suburb_id = 14,
          suburb_name = 14,
          town_city_id = 13,
          town_city_name = 13,
          ta_id = 12,
          ta_name = 12,
          regc_id = 11,
          regc_name = 11),
        min_raw_score = 251,
        dump_path = dump_path) %>%
      rename(
        street_match_type = match_type,
        street_match_score = score))

  # If these columns are identical, it's a match
  match_cols <- c(
    "unit", "number", "alpha",
    "street_id", "street_name", "street_type", "street_direction")

  # Create a shortlist of address match candidates (same street/number)
  message(Sys.time(), ": Creating NZAD match shortlist...")
  shortlist_df <-
    street_match_df %>% filter(!is.na(number) & !is.na(street_name)) %>% # Viable targets only
    select(all_of(match_cols)) %>%
    unique() %>%
    left_join(
      nzad_df %>%
        filter(!is.na(number) & !is.na(street_name)) %>% # Viable addresses only
        mutate(shortlist_match = 1), # Tracking count in case NZAD doesn't have UID
      by = match_cols[!match_cols %in% c("unit", "alpha")], # Ignore unit/alpha for now
      relationship = "many-to-many",
      suffix = c("", ".nzad"))

  # Score shortlist candidates on unit/alpha
  message(Sys.time(), ": Calculating NZAD match winners...")
  winners_df <-
    shortlist_df %>%
    mutate(
      match_score =
        # ifelse(!is.na(qpid), 0.4, 0) + # Has a matched QPID
        ifelse(!is.na(floor), -0.2, 0) + # Addresses with specified floors are probably not what we want
        case_when(
          unit == unit.nzad ~ 1, # Exact match
          is.na(unit) & is.na(unit.nzad) ~ 1, # Exact NA match
          is.na(unit.nzad) ~ 0.4, # Target has a unit, but match candidate doesn't (i.e. Will match 1/23 Bob Street to 23 Bob Street, if there are no better options)
          TRUE ~ 0) +
        case_when(
          alpha == alpha.nzad ~ 1, # Exact match
          is.na(alpha) & is.na(alpha.nzad) ~ 1, # Exact NA match
          is.na(alpha.nzad) ~ 0.4, # Target has a alpha, but match candidate doesn't (i.e. Will match 23A Bob Street to 23 Bob Street, if there are no better options)
          TRUE ~ 0)) %>%
    # Determine winners and count them
    group_by(!!!syms(match_cols)) %>%
    filter(match_score == max(match_score)) %>%
    mutate(
      match_count = sum(shortlist_match, na.rm = TRUE),
      .keep = "unused") %>%
    ungroup() %>%
    # Interpret score/count    
    mutate(match_type = case_when(
      match_count == 0 ~ "no match",
      match_count == 1 & match_score == 2 ~ "full", # Perfect!
      match_count == 1 & match_score > 0 ~ "partial", # Some mismatch, but one match is closest
      match_count > 1 & match_score == 2 ~ "conflicting matches", # Details are identical, but still conflicted
      match_count > 1 & match_score < 2 ~ "ambiguous unit/alpha", # Unclear, can probably do consensus match
      TRUE ~ "error (can't interpret score)"))

  if (drop_multi_results) {
    # Ambiguous/conflicting have to be filtered out since they return multiple
    # rows for a single address; create placeholders for them instead
    message(Sys.time(), ": Dropping conflicting/ambiguous results...")
    winners_df <-
      winners_df %>%
      filter(!match_type %in% c("ambiguous unit/alpha", "conflicting matches")) %>%
      rbind(
        winners_df %>%
          filter(match_type %in% c("ambiguous unit/alpha", "conflicting matches")) %>%
          mutate(across(
            .cols =
              colnames(winners_df) %>%
                .[!. %in% match_cols] %>% # Don't wipe match cols
                .[!. %in% c("match_type", "match_count", "match_score")], # Don't wipe match validation
            .fns = ~ NA)) %>%
          unique())
  }

  # Return nzad_matches back into res
  # If a nzad_winner is not found, we still want the streets
  message(Sys.time(), ": Recombining and cleaning...")
  res_df <-
    street_match_df %>%
    left_join(
      winners_df %>%
        select(
          -suburb_id, -suburb_name, -town_city_id, -town_city_name,
          -ta_id, -ta_name, -regc_id, -regc_name),
      by = match_cols,
      relationship = ifelse(
        drop_multi_results, "many-to-one", # There should never be multiple winners if we already dropped multi-results
        "many-to-many")) %>%
    mutate(
      # It is possible to accept matches where the unit/alpha are different
      # Join using the original unit/alpha, then replace with the matched unit/alpha
      unit = unit.nzad,
      alpha = alpha.nzad,
      match_type = case_when(
        # NA match type means it was filtered from winners list
        is.na(match_type) ~ case_when(
          is.na(street_name) ~ "no street",
          is.na(number) ~ "no number",
          TRUE ~ "error (not in winners list)"),
        # "No match" match type means match count == 0
        match_type == "no match" ~ case_when(
          is.na(street_id) ~ "unmatched street", # Blame this on unmatched street
          grepl("number-range-.*-(middle|end)", number_extract_type) ~ "range address", # Non-starting parts of address ranges might exist as addresses, this is normal
          is.na(dpid) ~ "unmatched number", # Generic match fail
          TRUE ~ "error (no match)"), # Should be impossible to have "no match" but for DPID to exist
        TRUE ~ match_type)) %>%
    # Clean names
    select(-unit.nzad, -alpha.nzad) %>%
    rename(
      nzad_match_type = match_type,
      nzad_match_score = match_score,
      nzad_match_count = match_count)

  # Match summary
  summary_df <-
    res_df %>%
    summarize(.by = nzad_match_type, count = n()) %>% 
    arrange(-count) %>%
    print()

  if (!is.null(dump_path)) {
    summary_df %>%
      write_csv(stringr::str_glue("{dump_path}/address_nzad_summary.csv"))

    unmatched_df <-
      res_df %>%
      filter(!nzad_match_type %in% c("full", "no street", "no number", "range address", "partial")) %>%
      select(
        nzad_match_type, dpid, street_id, raw_street_addresses,
        unit, number, alpha, street_name, street_type, street_direction,
        suburb_name, town_city_name, ta_name) %>%
      unique() %>%
      write_csv(stringr::str_glue("{dump_path}/address_nzad_unmatched.csv"))
    message(Sys.time(), stringr::str_glue(": {nrow(unmatched_df)} unmatched addresses."))
  }

  return(res_df)
}