###################################################################################################
# Generate maps for:
# Birge, H.E., Bielski, C.H., Gellman, J., Plantinga, A.J., Allen, C.R., Twidwell, D., 2019. 
# "Payments for carbon can lead to unwanted costs from afforestation in the U.S. Great Plains."
###################################################################################################

# libraries
library(tidyverse)
library(readxl)

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

# discount rate
discount <- 0.05

# social cost of carbon
carbon_cost <- 12

# read in forest conversion data (Nielsen et al. 2013)
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
forest_conversion_inflated <- forest_conversion  # a temporary df
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
    opp_cost_wo_harvest_crop = land_price_wo_harvest_crop_inflated + tree_establishment_CRP_predicted_inflated,
    opp_cost_wo_harvest_pasture = land_price_wo_harvest_pasture_inflated + tree_establishment_CRP_predicted_inflated,
    opp_cost_wo_harvest_range = land_price_wo_harvest_range_inflated + tree_establishment_CRP_predicted_inflated,
    opp_cost_w_harvest_crop = land_price_w_harvest_crop_inflated + tree_establishment_CRP_predicted_inflated,
    opp_cost_w_harvest_pasture = land_price_w_harvest_pasture_inflated + tree_establishment_CRP_predicted_inflated,
    opp_cost_w_harvest_range = land_price_w_harvest_range_inflated + tree_establishment_CRP_predicted_inflated
    )