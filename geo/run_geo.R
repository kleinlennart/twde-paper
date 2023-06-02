### Full Geo Pipeline run script ###

## 1. Preprocessing
source("6-Analyses/Geotagging/1-geo_cleaning.R")
# -> get tokens

## 2. Tag countries
## - flag emoji
## - regex keywords
# TODO: -> from matched state to Germany?

source("6-Analyses/Geotagging/2-geo_country.R")
# add vars:
# - geo_country_match
# - geo_country_flag
# TODO: -> combine to one with case_when

## 3. Tag States
## - keywords
source("6-Analyses/Geotagging/3-geo_states.R")
# add vars
# - geo_state_match


# 4. Tag Cities + postal codes!
source("6-Analyses/Geotagging/4-geo_cities.R")

# -> infer states?



source("6-Analyses/Geotagging/5-geo_misc.R")
## 5. Misc
## - postal codes
## - coordinates
## - full addresses?

# 6. Combine all information to vars
source("6-Analyses/Geotagging/6-geo_combine.R")

# -> dichotomize vars for state
# 1. combine to one
# 2. ether unnest or just tag with str_detect (match_helper)
# or with case_when for each state if str_detec?
