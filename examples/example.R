library(tidyverse)
library(hud.address)

# Real addresses from Pacifecon
raw_addresses <-
  read.csv("examples/raw_addresses.csv") %>%
  as_tibble()

# Extract structured address data
extracted_addresses <-
  raw_addresses %>%
  rename(raw_street_addresses = raw_street) %>%
  left_join(
    .$raw_street_addresses %>%
      hud.address::extract_street_addresses(),
    by = "raw_street_addresses",
    relationship = "many-to-many")

# Get a local copy of the NZAD addresses
nzad_df <-
  hud.address::fetch_nzad_addresses() %>%
  as_tibble()

# Match extracted addresses to NZAD addresses
matched_addresses <-
  hud.address::match_nzad_addresses(
    targ_df = extracted_addresses,
    nzad_df = nzad_df,
    structured_cols = c("street_name", "street_type", "street_direction"), # Exists in both
    unstructured_cols = c("raw_area"), # Unstructured area string (e.g. "Island Bay, Wellington")
    guess_cols = c("suburb_name", "town_city_name", "ta_name", "regc_name")) # Raw area can contain suburb/town/TA names
