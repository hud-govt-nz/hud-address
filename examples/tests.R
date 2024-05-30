# Half-arsed test suite - should really properly automate this
# Just run each line and seee if the results make sense
library(hud.address)

extract_street_addresses(c("1 Bob St"))
extract_street_addresses(c("101-105 Bob St"))
extract_street_addresses(c("1,3,5 Bob St"))
extract_street_addresses(c("1,3-5 Bob St"))
extract_street_addresses(c("1,3 & 5 Bob St"))
extract_street_addresses(c("1,3 and 5 Bob St"))
extract_street_addresses(c("31 Rue D' Amarres"))
extract_street_addresses(c("119 Main Rd St Arnaud"))
extract_street_addresses(c("1 No 1 Rd"))
extract_street_addresses(c("100A-B Normandale Rd"))
extract_street_addresses(c("100A-100C Normandale Rd"))
extract_street_addresses(c("100A - 100C Normandale Rd"))
extract_street_addresses(c("130 FDU1 Vivian St")) # DO NOT ACCEPT - there is a number in the road name
extract_street_addresses(c("130A Thames Coast Sh25 Rd")) # DO NOT ACCEPT - there is a number in the road name
extract_street_addresses(c("1005 AB State Highway 25")) # BAD RESULT
extract_street_addresses(c("358 and 360 B M Gubb Rd"))
extract_street_addresses(c("358A B M Gubb Rd"))
extract_street_addresses(c("358AA B M Gubb Rd"))
extract_street_addresses(c("358AAA B M Gubb Rd")) # DO NOT ACCEPT - three alpha is too many alpha
extract_street_addresses(c("354-358 B M Gubb Rd"))
extract_street_addresses(c("358A-358D B M Gubb Rd"))
