###################################################################################################
# Generate maps for:
# Birge, H.E., Bielski, C.H., Gellman, J., Plantinga, A.J., Allen, C.R., Twidwell, D., 2019. 
# "Payments for carbon can lead to unwanted costs from afforestation in the U.S. Great Plains."
###################################################################################################

# install.packages(c("sf", "lwgeom", "maps", "mapdata", "spData", "tigris", "tidycensus", "leaflet", "tmap", "tmaptools"))

# libraries
library(tidyverse)
library(readxl)
library(sf)

# "not in" function
`%ni%` <- Negate(`%in%`)

###################################################################################################
#
# Import and clean the data
#
###################################################################################################

# read in CPI data, series CUUR0000SA0 (used for CPI Inflation calculator)
CPI <- read_xlsx("raw_data/CPI_CUUR0000SA0.xlsx",range="A12:P23")
# source: https://data.bls.gov/timeseries/CUUR0000SA0

# inflation from 1997-2007
inflation_97_07 <- (CPI$Annual[length(CPI$Annual)] - CPI$Annual[1])/CPI$Annual[1]

# read in forest conversion data (Nielsen et al. 2013)
download.file(url="http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx",
              destfile = "raw_data/county-level-data_nielsen2013.xlsx")
forest_conversion <- read_xlsx("raw_data/county-level-data_nielsen2013.xlsx",range="A11:O3080")
colnames(forest_conversion) <- c("FIPS","county",
               "land_price_wo_harvest_crop", "land_price_wo_harvest_pasture", "land_price_wo_harvest_range",
               "land_price_w_harvest_crop", "land_price_w_harvest_pasture", "land_price_w_harvest_range",
               "tree_establishment_CRP","tree_establishment_CRP_predicted",
               "C_uptake_wo_harvest", "C_uptake_w_harvest",
               "land_eligible_conversion_crop","land_eligible_conversion_pasture","land_eligible_conversion_range")
# those column names correspond to the Nielsen et al. column names.
# source: http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx

# adjust from 1997 dollars to 2007 dollars
forest_conversion_inflated <- forest_conversion  # a temporary df for inflation calculations
for (i in 3:10) {
  forest_conversion_inflated[,i] <- forest_conversion[,i]*(1+inflation_97_07)  # inflation calculation
  }
colnames(forest_conversion_inflated)[3:10] <-
  paste0(colnames(forest_conversion)[3:10],"_inflated")
forest_conversion <- left_join(
  forest_conversion,
  forest_conversion_inflated
  ) %>%
  select(FIPS:land_eligible_conversion_range,
         land_price_wo_harvest_crop_inflated:tree_establishment_CRP_predicted_inflated)  # joining the inflated values
rm(forest_conversion_inflated)  # removing the temporary inflated df

# calculate opportunity cost
forest_conversion <- forest_conversion %>%
  mutate(
    opp_cost_wo_harvest_crop = sum(land_price_wo_harvest_crop_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_wo_harvest_pasture = sum(land_price_wo_harvest_pasture_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_wo_harvest_range = sum(land_price_wo_harvest_range_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_crop = sum(land_price_w_harvest_crop_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_pasture = sum(land_price_w_harvest_pasture_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_range = sum(land_price_w_harvest_range_inflated, tree_establishment_CRP_predicted_inflated, na.rm=TRUE)
    )

# calculate MC = opp_cost/MT of C
forest_conversion <- forest_conversion %>%
  mutate(
    mc_wo_harvest_crop = ifelse(is.finite(opp_cost_wo_harvest_crop/C_uptake_wo_harvest),opp_cost_wo_harvest_crop/C_uptake_wo_harvest,NA),
    mc_wo_harvest_pasture = ifelse(is.finite(opp_cost_wo_harvest_pasture/C_uptake_wo_harvest),opp_cost_wo_harvest_pasture/C_uptake_wo_harvest,NA),
    mc_wo_harvest_range = ifelse(is.finite(opp_cost_wo_harvest_range/C_uptake_wo_harvest),opp_cost_wo_harvest_range/C_uptake_wo_harvest,NA),
    mc_w_harvest_crop = ifelse(is.finite(opp_cost_w_harvest_crop/C_uptake_w_harvest),opp_cost_w_harvest_crop/C_uptake_w_harvest,NA),
    mc_w_harvest_pasture = ifelse(is.finite(opp_cost_w_harvest_pasture/C_uptake_w_harvest),opp_cost_w_harvest_pasture/C_uptake_w_harvest,NA),
    mc_w_harvest_range = ifelse(is.finite(opp_cost_w_harvest_range/C_uptake_w_harvest),opp_cost_w_harvest_range/C_uptake_w_harvest,NA)
    )

# afforested acres under a $12/t carbon price
forest_conversion <- forest_conversion %>%
  mutate(
    crop_acres_12 = ifelse(is.na(mc_wo_harvest_crop),NA,
                            ifelse(mc_wo_harvest_crop > 12, 0,
                                   land_eligible_conversion_crop)),
    pasture_acres_12 = ifelse(is.na(mc_wo_harvest_pasture),NA,
                            ifelse(mc_wo_harvest_pasture > 12, 0,
                                   land_eligible_conversion_pasture)),
    range_acres_12 = ifelse(is.na(mc_wo_harvest_range),NA,
                            ifelse(mc_wo_harvest_range > 12, 0,
                                   land_eligible_conversion_range)),
    total_acres_12 = sum(crop_acres_12,pasture_acres_12,range_acres_12,na.rm=TRUE)
  )

# afforested acres under a $20/t carbon price
forest_conversion <- forest_conversion %>%
  mutate(
    crop_acres_20 = ifelse(is.na(mc_wo_harvest_crop),NA,
                           ifelse(mc_wo_harvest_crop > 20, 0,
                                  land_eligible_conversion_crop)),
    pasture_acres_20 = ifelse(is.na(mc_wo_harvest_pasture),NA,
                              ifelse(mc_wo_harvest_pasture > 20, 0,
                                     land_eligible_conversion_pasture)),
    range_acres_20 = ifelse(is.na(mc_wo_harvest_range),NA,
                            ifelse(mc_wo_harvest_range > 20, 0,
                                   land_eligible_conversion_range)),
    total_acres_20 = sum(crop_acres_20,pasture_acres_20,range_acres_20,na.rm=TRUE)
  )

# afforested acres under a $50/t carbon price
forest_conversion <- forest_conversion %>%
  mutate(
    crop_acres_50 = ifelse(is.na(mc_wo_harvest_crop),NA,
                           ifelse(mc_wo_harvest_crop > 50, 0,
                                  land_eligible_conversion_crop)),
    pasture_acres_50 = ifelse(is.na(mc_wo_harvest_pasture),NA,
                              ifelse(mc_wo_harvest_pasture > 50, 0,
                                     land_eligible_conversion_pasture)),
    range_acres_50 = ifelse(is.na(mc_wo_harvest_range),NA,
                            ifelse(mc_wo_harvest_range > 50, 0,
                                   land_eligible_conversion_range)),
    total_acres_50 = sum(crop_acres_50,pasture_acres_50,range_acres_50,na.rm=TRUE)
  )

###################################################################################################
#
# Map
#
###################################################################################################

download.file(url="https://www2.census.gov/geo/tiger/TIGER2018/COUNTY/tl_2018_us_county.zip",
              destfile="raw_data/tl_2018_us_county.zip")
unzip("raw_data/tl_2018_us_county.zip",exdir="raw_data/tl_2018_us_county")

# creating a list of counties in Alaska and Hawaii to filter out of shape file
AKHI_fips = c('02013',	'02016',	'02020',	'02050',	'02060',	'02068',	
              '02070',	'02090',	'02100',	'02105',	'02110',	'02122',	
              '02130',	'02150',	'02164',	'02170',	'02180',	'02185',	
              '02188',	'02195',	'02198',	'02220',	'02230',	'02240',	
              '02261',	'02270',	'02275',	'02282',	'02290',	'15001',	
              '15003',	'15005',	'15007',	'15009')

counties <- st_read("raw_data/tl_2018_us_county/tl_2018_us_county.shp", quiet=TRUE) %>%
  arrange(GEOID) %>%
  filter(GEOID %ni% AKHI_fips)
# source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2018.html

counties %>%
  filter(GEOID %ni% forest_conversion$FIPS)
