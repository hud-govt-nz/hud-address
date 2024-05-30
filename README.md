# hud-address
Tools for parsing and matching addresses.

This is a hard job! All the heavy lifting is done behind the scenes by `extract_addresses.py`, sitting behind R wrappers. 


## Installation
You'll need `devtools::install_github` to install the package:
```R
devtools::install_github("hud-govt-nz/hud-address")
```


## Usage
```R
library(tidyverse)
library(hud.address)

# Real addresses from Pacifecon
raw_addresses <-
  read.csv("examples/raw_addresses.csv") %>%
  as_tibble()

# Extract structured address data
extracted_addresses <-
  raw_addresses %>%
  left_join(
    .$raw_street %>%
      hud.address::extract_street_addresses() %>%
      rename(raw_street = raw_street_addresses),
    by = "raw_street",
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
```


## Maintaining this package
If you make changes to this package, you'll need to rerun document from the root directory to update all the R generated files.
```R
library(roxygen2)
roxygenise()
```

I had real problems installing `roxygen2`, because there's a problem with the upstream library `cli`. It's been fixed, but it's not in the CRAN version as of 29-08-2022. You might need the Github version:
```R
library(devtools)
install_github("r-lib/cli")
install_github("r-lib/roxygen2")
library(cli)
library(roxygen2)
roxygenise()
```
