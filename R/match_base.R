# Pre-generate a lookup table of match scores based on stringdistmatrix
# This is much faster than doing it one-by-one
make_match_lookup <- function(x, y, edit_weights, nthread, max_dist) {
  if (length(y) == 0) y <- c(".NA") # Placeholder
  # Do full string dist matrix match if either candidate or target is a string
  if (is.character(x) | is.character(y)) {
    # Match all against all
    raw_dist <-
      stringdist::stringdistmatrix(x, y, weight = edit_weights, nthread = nthread, useNames = "strings") %>%
      as_tibble()

    # Convert distances to scores
    raw_scores <-
      raw_dist %>%
      transmute(across(
        .cols = everything(),
        .fns = function(raw_dist) {
          case_when(raw_dist <= max_dist ~ 1 / (raw_dist + 1), # Anything below the threshold should return 1/(n+1)
                    raw_dist > max_dist ~ 0) # Anything above the threshold should return a 0
        })) %>%
      as.data.frame()
  # For non-string comparisons, do pure equality matches
  # CAUTION: Doing a stringdist match on a number is a very bad idea!
  } else {
    raw_scores <-
      outer(x, y, "==") %>%
      as.data.frame()
    raw_scores <- raw_scores * 1
    colnames(raw_scores) <- y
  }
  # Add NA match
  raw_scores <-
    raw_scores %>%
    mutate(.NA = ifelse(is.na(x), 1, 0))
  return(raw_scores)
}


# Reduce a dataframe to consensus values
# i.e. the values that are agreed upon by [threshold] proportion of rows
get_consensus <- function(candidates_df, threshold = 0.8) {
  candidates_df %>% 
    mutate(across(
      .cols = everything(),
      .fns = function(targ_col, threshold = 0.8) {
        map(targ_col, function(v) {
          if (is.na(v)) return(NA)
          commonness <- sum((targ_col == v) / length(targ_col), na.rm = TRUE)
          if (commonness > threshold) return(v)
          else return(NA)
        }) %>%
        unlist()
      })) %>%
    unique()
}


scrub_place <- function(place_col) {
  if (!is.character(place_col)) return(place_col)
  place_col %>%
    tolower() %>%
    sub(" (district|region|city|central)", "", .) %>%
    # sub("(^pt\\b", "point", .) %>%
    # sub("(^mt\\b", "mount", .) %>%
    sub("(near)", "", .) %>%
    gsub("[']", "", .) %>%
    str_trim()
}


#' Match address piles
#' 
#' Takes a frame of street/area names with structured and unstructured data and
#' returns matching row from candidates table.
#' 
#' Designed to be used for street or suburb matching, which can be used to
#' generate a shortlist (i.e. A list of all the addresses on a street) which
#' you can then do rule-based matching on.
#' 
#' In theory you can use this for complete addresses, but the stringdistmatrix
#' will be very big.
#' 
#' @name match_address_pile
#' @param targ_df Records we're trying to match.
#' @param candidates_df Definitive records.
#' @param structured_cols List of structured data (e.g. Street number/street name)
#'   columns which exist in both frames.
#' @param unstructured_cols List of comma-separated unstructured data (e.g. Address
#'   blocks in the form of "Willis Street, Wellington Central, Wellington") in
#'   targ_df.
#' @param guess_cols List of structured data in candidates_df which we're trying to
#'   match the unstructured_cols to.
#' @param col_weights Weight to apply to each structured/guess column. This is
#'   mostly helpful in the event of tiebreakers.
#' @param min_raw_score Minimum threshold before adjustments; raw_scores are
#'   determined by col_weights. So you can either use this to require a minimum
#'   number of matches to consider a row a real match, or you can make one
#'   col_weight bigger than the rest combined to it becomes mandatory for getting
#'   over the threshold.
#' @param max_dist Matches are slightly fuzzy, using a string distance algorithm,
#'   and discounted on a 1/n basis (i.e. the first error is free, the second will
#'   discount the match score by 50%, etc.). Matches with a string distance above
#'   `max_dist` will be dropped completely, so that vaguely similar names do not
#'   get a residual score.
#' @param edit_weights Used by stringdistmatrix for tweaking edit/deletion/insertion
#'   weights.
#' @param consensus_threshold Consensus values have to reach this threshold to be
#'   accepted.
#' @param nthread Number of threads stringdistmatrix should use.
#' @param dump_path Path for validation output.
#' @examples
#' match_address_pile(
#'   targ_df = sites_df,
#'   candidates_df = streets_df,
#'   structured_cols = c("street_name", "street_type", "street_direction"), # Exists in both
#'   unstructured_cols = c("prj_raw_area"), # Unstructured area string (e.g. "Island Bay, Wellington")
#'   guess_cols = c("suburb_name", "town_city_name", "ta_name", "regc_name"),
#'   col_weights = c(
#'     street_name = 1.0,
#'     street_type = 0.2,
#'     street_direction = 0.5,
#'     suburb_name = 1.0,
#'     town_city_name = 0.8,
#'     ta_name = 0.6,
#'     regc_name = 0.5))
#' 
match_address_piles <- function(targ_df, candidates_df,
                                structured_cols, unstructured_cols, guess_cols,
                                col_weights, min_raw_score,
                                max_dist = 1, edit_weights = c(1, 1, 1, 1), consensus_threshold = 0.95,
                                nthread = 10, dump_path = NULL) {
  message(Sys.time(), ": Matching address piles...")
  # Check that a dataframe has the expected columns
  tryCatch(
    targ_df %>% select(all_of(c(structured_cols, unstructured_cols))) -> m,
    error = function(e) {
      message("\033[1;33mtarg_df doesn't have the expect columns!\033[0m")
      stop(e)
    })
  tryCatch(
    candidates_df %>% select(all_of(c(structured_cols, guess_cols))) -> m,
    error = function(e) {
      message("\033[1;33mcandidates_df doesn't have the expect columns!\033[0m")
      stop(e)
    })
  tryCatch({
    expected <- c(structured_cols, guess_cols)
    missing <- expected[!expected %in% names(col_weights)]
    if (length(missing) > 0) {
      stop("\033[1;33mcol_weights not defined for: ", missing, "\033[0m")
    }
  })

  # Scrub and dedupe candidates
  candidates_df <-
    candidates_df %>%
    mutate(
      across(
        .cols = all_of(c(structured_cols, guess_cols)),
        .names = "{.col}_scrubbed",
        .fns = scrub_place),
      valid_count = apply(across(ends_with("_scrubbed")), 1, function(r) {
        r[!is.na(r)] %>% unique() %>% length()
      })) %>%
    # Look for streets with the same street_/suburb_id - these are conflicting names
    # Pick the name with most capitals
    group_by(street_id, suburb_id) %>%
    mutate(street_name_caps = str_count(street_name, "[A-Z]")) %>%
    filter(street_name_caps == max(street_name_caps)) %>%
    ungroup()

  # Scrub structured columns and make placeholder for address_pile
  scrubbed_df <-
    targ_df %>%
    mutate(
      across(
        .cols = all_of(structured_cols),
        .names = "{.col}_scrubbed",
        .fns = scrub_place),
      address_pile = replicate(n(), list()))

  # Combine, scrub and dedupe unstructured_cols into address_pile
  if (!is.null(unstructured_cols)) {
    scrubbed_df <-
      scrubbed_df %>%
      # Split unstructure columns into list columns
      mutate(
        across(
          .cols = all_of(unstructured_cols),
          .names = "{.col}_split",
          .fns = ~ str_split(., ", "))
      ) %>%
      # Combine list columns into a single list column and clean
      mutate(
        address_pile =
          select(., ends_with("_split")) %>%
            pmap(c) %>% # Combine into a single column of lists
            map(scrub_place) %>% # Sanitise each item
            map(~ .[!is.na(.)] ) %>% # Filter out NAs
            map(unique)) %>%
      select(-ends_with("_split")) # Tidy
  }

  # Generate match lookup tables for structured columns
  # Scores for individual columns are kept as separate frames
  structured_matches <- structured_cols
  names(structured_matches) <- structured_matches

  structured_matches <- 
    structured_matches %>%
    map(function(col_name) {
      message(Sys.time(), str_glue(": Generating match lookups for structured column '{col_name}'..."))
      scrubbed_name <- paste0(col_name, "_scrubbed")
      unique_vals <- scrubbed_df[[scrubbed_name]] %>% .[!is.na(.)] %>% unique()
      if (length(unique_vals) == 0) {
        warning(str_glue("\033[1;33mtarg_df${col_name} does not have any valid values!\033[0m"))
      }

      raw_scores <-
        make_match_lookup(
          candidates_df[[scrubbed_name]],
          unique_vals,
          edit_weights, nthread, max_dist)

      return(raw_scores * col_weights[[col_name]])
    })

  # Generate match lookup tables for unstructured columns
  # Only use the highest score across the columns
  # i.e. If "auckland" matches to "auckland" (suburb, worth 1.0) and
  # "auckland" (towncity, worth 0.8), then the match is 1.0
  if (!is.null(unstructured_cols)) {
    unstructured_matches <-
      guess_cols %>%
      map(function(col_name) {
        message(Sys.time(), str_glue(": Generating match lookups for unstructured column '{col_name}'..."))
        scrubbed_name <- paste0(col_name, "_scrubbed")
        unique_vals <- scrubbed_df$address_pile %>% unlist() %>% unique()

        raw_scores <-
          make_match_lookup(
            candidates_df[[scrubbed_name]],
            unique_vals,
            edit_weights, nthread, max_dist)

        return(raw_scores * col_weights[[col_name]])
      }) %>%
      do.call(pmax, .) %>%
      as_tibble()
  }

  message(Sys.time(), ": Using lookup table to process address piles...")
  res <-
    scrubbed_df %>%
    apply(1, function(r) {
      tryCatch({
        # Fetch and combine scores from lookup table
        structured_score <-
          structured_cols %>%
          map(function(col_name) {
            val <- r[[paste0(col_name, "_scrubbed")]]
            if (is.na(val)) val <- ".NA"
            return(structured_matches[[col_name]][[as.character(val)]])
          }) %>%
          Reduce(`+`, .)

        unstructured_score <-
          r$address_pile %>% unlist() %>%
          map(~unstructured_matches[[.]]) %>%
          Reduce(`+`, .)
        if (is.null(unstructured_score)) unstructured_score <- 0

        # Find viable matches
        winners <-
          candidates_df %>%
          mutate(score = structured_score + unstructured_score - (valid_count * 0.01)) %>% # Use valid_count as tiebreaker
          filter(!is.na(score) & score > min_raw_score)
        # Find best matches
        if (nrow(winners) > 1) {
          winners <- winners %>% filter(score == max(score))
        }

        # Single winner, good to go
        if (nrow(winners) == 1) {
          return(winners %>% mutate(match_type = "match"))
        }

        # No winners, give up
        placeholder <- winners %>% filter(FALSE) %>% add_row()
        if (nrow(winners) == 0) (
          return(placeholder %>% mutate(match_type = "unmatched"))
        )

        # No single winner, look for a compromise candidate
        # CAUTION: This is not an actual row, but the parts of rows that agree
        # Key usecase is when the same road cross many suburbs, where we are
        # sure of the road, but don't have a suburb match; in this case, we want
        # to keep the road, just drop the suburb. Do not do this when there is a
        # large number of winners - it's very slow, and the answer is likely to
        # be useless.
        if (nrow(winners) <= 20) {
          compromise <- winners %>% get_consensus(consensus_threshold)
          if (nrow(compromise) == 1) {
            return(compromise %>% mutate(match_type = "compromise"))
          }
        }

        # No winners found and no viable compromise, give up
        return(placeholder %>% mutate(match_type = "ambiguous"))
      }, error = function(e) { print(r); stop(e) })
    }) %>%
    do.call(rbind, .) %>%
    select(-ends_with("_scrubbed"), -valid_count)

  if (!is.null(dump_path)) {
    unmatched_df <-
      scrubbed_df[is.na(res$street_id),] %>%
      select(all_of(c(structured_cols, unstructured_cols))) %>%
      unique() %>%
      write_csv(str_glue("{dump_path}/address_piles_unmatched.csv"))
    message(Sys.time(), str_glue(": {nrow(unmatched_df)} unmatched address piles."))
  }

  message(Sys.time(), ": Address piles parsed.")
  return(res)
}