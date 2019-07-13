###################################################################################################
# Generate maps for:
# Birge, H.E., Bielski, C.H., Gellman, J., Plantinga, A.J., Allen, C.R., Twidwell, D., 2019. 
# "Payments for carbon can lead to unwanted costs from afforestation in the U.S. Great Plains."
###################################################################################################

# install.packages(c("readxl","sf","tidyverse"))
rm(list=ls(all=TRUE))

# libraries
library(maps)
library(readxl)
library(sf)
library(tidyverse)

# "not in" function
`%ni%` <- Negate(`%in%`)

###################################################################################################
#
# Import and clean data
#
###################################################################################################

# read in CPI data
download.file(url="https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
              destfile="raw_data/CPI.txt")
CPI <- read_delim(file="raw_data/CPI.txt",
                  delim="\t")
colnames(CPI) <- c("series_id","year","period","value","footnote_codes")
CPI <- CPI %>%
  mutate(series_id = str_replace_all(series_id," ","")) %>%
  dplyr::filter(series_id=="CUUR0000SA0" &  # the desired series
                  year >= 1997 &
                  year <= 2007 &
                  period == "M13") %>%  # M13: the annual average inflation
  mutate(value=as.numeric(value))
# source: https://www.bls.gov/cpi/data.htm
# series CUUR0000SA0, "All items in U.S. city average, all urban consumers, not seasonally adjusted"
  
# read in forest conversion data (Nielsen et al. 2013)
download.file(url="http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx",
              destfile = "raw_data/county-level-data_nielsen2013.xlsx",
              mode="wb")

forest_conversion <- read_xlsx("raw_data/county-level-data_nielsen2013.xlsx",range="A11:O3080")
colnames(forest_conversion) <- c("FIPS","county",
               "land_price_wo_harvest_crop", "land_price_wo_harvest_pasture", "land_price_wo_harvest_range",
               "land_price_w_harvest_crop", "land_price_w_harvest_pasture", "land_price_w_harvest_range",
               "tree_establishment_CRP","tree_establishment_CRP_predicted",
               "C_uptake_wo_harvest", "C_uptake_w_harvest",
               "land_eligible_conversion_crop","land_eligible_conversion_pasture","land_eligible_conversion_range")
# those column names correspond to the Nielsen et al. column names
# source: http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx

# adjust from 1997 dollars to 2007 dollars
inflation_97_07 <- (CPI$value[11] - CPI$value[1])/CPI$value[1]  # inflation 1997-2007
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
         land_price_wo_harvest_crop_inflated:tree_establishment_CRP_predicted_inflated)  # join inflated values
rm(forest_conversion_inflated)  # removing the temporary inflated df

###################################################################################################
#
# Analysis
#
###################################################################################################

# calculate opportunity cost
forest_conversion <- forest_conversion %>%
  mutate(
    opp_cost_wo_harvest_crop = sum(land_price_wo_harvest_crop_inflated, 
                                   tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_wo_harvest_pasture = sum(land_price_wo_harvest_pasture_inflated, 
                                      tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_wo_harvest_range = sum(land_price_wo_harvest_range_inflated, 
                                    tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_crop = sum(land_price_w_harvest_crop_inflated, 
                                  tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_pasture = sum(land_price_w_harvest_pasture_inflated, 
                                     tree_establishment_CRP_predicted_inflated, na.rm=TRUE),
    opp_cost_w_harvest_range = sum(land_price_w_harvest_range_inflated, 
                                   tree_establishment_CRP_predicted_inflated, na.rm=TRUE)
    )

# calculate MC = opp_cost/MT of C
forest_conversion <- forest_conversion %>%
  mutate(
    mc_wo_harvest_crop = ifelse(is.finite(opp_cost_wo_harvest_crop/C_uptake_wo_harvest),
                                opp_cost_wo_harvest_crop/C_uptake_wo_harvest,NA),
    mc_wo_harvest_pasture = ifelse(is.finite(opp_cost_wo_harvest_pasture/C_uptake_wo_harvest),
                                   opp_cost_wo_harvest_pasture/C_uptake_wo_harvest,NA),
    mc_wo_harvest_range = ifelse(is.finite(opp_cost_wo_harvest_range/C_uptake_wo_harvest),
                                 opp_cost_wo_harvest_range/C_uptake_wo_harvest,NA),
    mc_w_harvest_crop = ifelse(is.finite(opp_cost_w_harvest_crop/C_uptake_w_harvest),
                               opp_cost_w_harvest_crop/C_uptake_w_harvest,NA),
    mc_w_harvest_pasture = ifelse(is.finite(opp_cost_w_harvest_pasture/C_uptake_w_harvest),
                                  opp_cost_w_harvest_pasture/C_uptake_w_harvest,NA),
    mc_w_harvest_range = ifelse(is.finite(opp_cost_w_harvest_range/C_uptake_w_harvest),
                                opp_cost_w_harvest_range/C_uptake_w_harvest,NA)
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

# download county data
download.file(url="https://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip",
              destfile="raw_data/cb_2014_us_county_20m.zip")
unzip("raw_data/cb_2014_us_county_20m.zip",exdir="raw_data/cb_2014_us_county_20m")
# source: https://www.census.gov/geographies/mapping-files.html

# read in county shapefile
county_shp <- st_read("raw_data/cb_2014_us_county_20m/cb_2014_us_county_20m.shp",quiet=T)
county_shp <- st_transform(county_shp,crs=2163)

# filter for counties in the Nielsen et al. dataset
county_shp <- county_shp %>%
  mutate(county_fips=as.numeric(paste0(STATEFP,COUNTYFP))) %>%
  dplyr::filter(county_fips %in% forest_conversion$FIPS)

# generate map
county_map <- ggplot(forest_conversion) +
  geom_sf(data=county_shp,fill="white", color="#7f7f7f") +
  theme_bw() +
  theme(plot.background = element_rect(fill = "transparent",color = NA),
        panel.border = element_blank(),
        panel.background =element_rect(fill = "transparent",color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right")
county_map





