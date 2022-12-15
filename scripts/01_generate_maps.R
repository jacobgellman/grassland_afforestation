# jacob gellman
# generate maps

###################################################################################################
#
# preamble
# 
###################################################################################################

# load or install necessary libraries. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(beepr, # beep noise
               measurements, # convert units easily
               progress,  # progress bar
               readxl,  # read excel files
               scales, # for comma function
               sf,  # for vector data
               tictoc, # system timer
               tidyverse # tidyverse
)

# clear environment and set wd.
rm(list=ls(all=TRUE));gc()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));setwd("..")
options(scipen=999)  # turn off sci notation
options(dplyr.summarise.inform = FALSE)  # turn off dplyr group by comments
sf::sf_use_s2(FALSE)  # make sf work using GEOS instead of s2
`%ni%` <- Negate(`%in%`)  # "not in" function

###################################################################################################
#
# Import and clean data
#
###################################################################################################

# read in CPI data
# download.file(url="https://download.bls.gov/pub/time.series/cu/cu.data.0.Current",
#               destfile="raw_data/cpi/cpi.txt")
cpi <- read_delim(file="raw_data/cpi/cpi.txt",delim="\t")
colnames(cpi) <- c("series_id","year","period","value","footnote_codes")
cpi <- 
  cpi %>%
  mutate(series_id = str_replace_all(series_id," ","")) %>%
  dplyr::filter(series_id=="CUUR0000SA0" &  # the desired series
                  year >= 1997 &
                  year <= 2007 &
                  period == "M13") %>%  # M13: the annual average inflation
  mutate(value=as.numeric(value))
# source: https://www.bls.gov/cpi/data.htm
# series CUUR0000SA0, "All items in U.S. city average, all urban consumers, not seasonally adjusted"
  
# read in forest conversion data (Nielsen et al. 2013)
# download.file(url="http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx",
#               destfile = "raw_data/nielsen_et_al_2013/county-level-data_nielsen2013.xlsx",
#               mode="wb")
forest_conversion <- read_xlsx("raw_data/nielsen_et_al_2013/county-level-data_nielsen2013.xlsx",range="A11:O3080")
colnames(forest_conversion) <- c("FIPS","county",
               "land_price_wo_harvest_crop", "land_price_wo_harvest_pasture", "land_price_wo_harvest_range",
               "land_price_w_harvest_crop", "land_price_w_harvest_pasture", "land_price_w_harvest_range",
               "tree_establishment_CRP","tree_establishment_CRP_predicted",
               "C_uptake_wo_harvest", "C_uptake_w_harvest",
               "land_eligible_conversion_crop","land_eligible_conversion_pasture","land_eligible_conversion_range")
# those column names correspond to the Nielsen et al. column names
# source: http://www.fs.fed.us/pnw/pubs/pnw_gtr888/county-level-data_nielsen2013.xlsx

# adjust from 1997 dollars to 2007 dollars
inflation_97_07 <- (cpi$value[cpi$year==2007] - cpi$value[cpi$year==1997])/cpi$value[cpi$year==1997]  # inflation 1997-2007
forest_conversion_inflated <- forest_conversion  # a temporary df for inflation calculations
for (i in 3:10) {
  forest_conversion_inflated[,i] <- forest_conversion[,i]*(1+inflation_97_07)  # inflation calculation
  }
colnames(forest_conversion_inflated)[3:10] <- paste0(colnames(forest_conversion)[3:10],"_inflated")
forest_conversion <- 
  bind_cols(
    forest_conversion,
    forest_conversion_inflated[,3:10]
    ) %>%
  select(FIPS:land_eligible_conversion_range,
         land_price_wo_harvest_crop_inflated:tree_establishment_CRP_predicted_inflated)  # join inflated values
rm(forest_conversion_inflated)  # removing the temporary inflated df

###################################################################################################
#
# analysis
#
###################################################################################################

# calculate opportunity cost
forest_conversion <- 
  forest_conversion %>%
  mutate(
    opp_cost_wo_harvest_crop = rowSums(cbind(land_price_wo_harvest_crop_inflated, 
                                   tree_establishment_CRP_predicted_inflated), na.rm=TRUE),
    opp_cost_wo_harvest_pasture = rowSums(cbind(land_price_wo_harvest_pasture_inflated, 
                                      tree_establishment_CRP_predicted_inflated), na.rm=TRUE),
    opp_cost_wo_harvest_range = rowSums(cbind(land_price_wo_harvest_range_inflated, 
                                    tree_establishment_CRP_predicted_inflated), na.rm=TRUE),
    opp_cost_w_harvest_crop = rowSums(cbind(land_price_w_harvest_crop_inflated, 
                                  tree_establishment_CRP_predicted_inflated), na.rm=TRUE),
    opp_cost_w_harvest_pasture = rowSums(cbind(land_price_w_harvest_pasture_inflated, 
                                     tree_establishment_CRP_predicted_inflated), na.rm=TRUE),
    opp_cost_w_harvest_range = rowSums(cbind(land_price_w_harvest_range_inflated, 
                                   tree_establishment_CRP_predicted_inflated), na.rm=TRUE)
    )

# calculate MC = opp_cost/MT of C
forest_conversion <- 
  forest_conversion %>%
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
forest_conversion <- 
  forest_conversion %>%
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
    total_acres_12 = rowSums(cbind(crop_acres_12,pasture_acres_12,range_acres_12),na.rm=TRUE)
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
    total_acres_20 = rowSums(cbind(crop_acres_20,pasture_acres_20,range_acres_20),na.rm=TRUE)
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
    total_acres_50 = rowSums(cbind(crop_acres_50,pasture_acres_50,range_acres_50),na.rm=TRUE)
  )

###################################################################################################
#
# summary stats
#
###################################################################################################

# how many range acres could be afforested?
comma(sum(forest_conversion$range_acres_50,na.rm=T))  # 61,205,300
comma(sum(forest_conversion$crop_acres_50,na.rm=T))  # 15,362,490
comma(sum(forest_conversion$pasture_acres_50,na.rm=T))  # 12,773,180

# a subset of plains states
forest_conversion_plains <- 
  forest_conversion %>%
  mutate(state_code=NA)
for (i in 1:length(forest_conversion_plains$county)) {
  forest_conversion_plains$state_code[i] <- 
    str_sub(forest_conversion_plains$county[i],-2,-1)
}
forest_conversion_plains <- 
  forest_conversion_plains %>%
  filter(state_code %in% c("MT","WY","ND","SD",
                           "NE","KS","OK","TX"))
sum(forest_conversion_plains$range_acres_50,na.rm=T)

###################################################################################################
#
# Map
#
###################################################################################################

###################################################################################################
# map prep
###################################################################################################

# download county shapefile data
download.file(url="https://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_20m.zip",
              destfile="raw_data/cb_2014_us_county_20m.zip")
unzip("raw_data/cb_2014_us_county_20m.zip",exdir="raw_data/cb_2014_us_county_20m")
# source: https://www.census.gov/geographies/mapping-files.html

# read in county shapefile
county_shp <- read_sf("raw_data/cb_2014_us_county_20m/cb_2014_us_county_20m.shp")
county_shp <- county_shp %>% st_transform(crs="ESRI:102003")  # albers equal area

# filter for counties in the Nielsen et al. dataset
county_shp <- 
  county_shp %>%
  mutate(FIPS=as.numeric(paste0(STATEFP,COUNTYFP))) %>%
  dplyr::filter(FIPS %in% forest_conversion$FIPS) 

# join the forest_conversion df to the county sf object
county_shp <- 
  left_join(
    county_shp,
    forest_conversion,
    by="FIPS"
  )

# generate manual breaks to use for colored fill in map
county_shp$brks_range_12 <- cut(county_shp$range_acres_12,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200",
                                                "201 - 400", "401 - 600", "601 - 1600"))
county_shp$brks_total_12 <- cut(county_shp$total_acres_12,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200", 
                                                "201 - 400", "401 - 600", "601 - 1600"))
county_shp$brks_range_20 <- cut(county_shp$range_acres_20,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200",
                                                "201 - 400", "401 - 600", "601 - 1600"))
county_shp$brks_total_20 <- cut(county_shp$total_acres_20,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200", 
                                                "201 - 400", "401 - 600", "601 - 1600"))
county_shp$brks_range_50 <- cut(county_shp$range_acres_50,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200",
                                                "201 - 400", "401 - 600", "601 - 1600"))
county_shp$brks_total_50 <- cut(county_shp$total_acres_50,
                                       breaks=c(-1, 50000, 100000, 200000, 400000, 600000, 1600000),
                                       labels=c("0 - 50", "51 - 100", "101 - 200", 
                                                "201 - 400", "401 - 600", "601 - 1600"))
###################################################################################################
# map A: carbon priced at $12 per ton
###################################################################################################

ggplot() +
  geom_sf(data=county_shp,aes(fill=brks_range_12), color="#7f7f7f",size=0.1) +
  theme_bw() +
  scale_fill_manual(na.value="white", 
                    labels = c("0 - 50", "51 - 100", "101 - 200",
                               "201 - 400",
                               "No data"),
                    values = c("#fff7bc","#f5bc7c","#eca05f","#e1803e"),
                    name = "Rangeland acres (1,000s)\nnewly forested")+
  theme(plot.background = element_rect(color="white",fill="white"),
        panel.border = element_blank(),
        panel.background =element_rect(fill = "transparent",color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right")
ggsave("figures/maps/county_map_12.pdf",width=6.5,height=4)
ggsave("figures/maps/county_map_12.png",width=6.5,height=4)

###################################################################################################
# map B: carbon priced at $20 per ton
###################################################################################################

ggplot() +
  geom_sf(data=county_shp,aes(fill=brks_range_20), color="#7f7f7f",size=0.1) +
  theme_bw() +
  scale_fill_manual(na.value="white", 
                    labels = c("0 - 50", "51 - 100", "101 - 200",
                               "201 - 400",
                               "No data"),
                    values = c("#fff7bc","#f5bc7c","#eca05f","#e1803e"),
                    name = "Rangeland acres (1,000s)\nnewly forested")+
  theme(plot.background = element_rect(color="white",fill="white"),
        panel.border = element_blank(),
        panel.background =element_rect(fill = "transparent",color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right")
ggsave("figures/maps/county_map_20.pdf",width=6.5,height=4)
ggsave("figures/maps/county_map_20.png",width=6.5,height=4)

###################################################################################################
# map C: carbon priced at $50 per ton
###################################################################################################

ggplot() +
  geom_sf(data=county_shp,aes(fill=brks_range_50), color="#7f7f7f",size=0.1) +
  theme_bw() +
  scale_fill_manual(na.value="white", 
                    labels = c("0 - 50", "51 - 100", "101 - 200",
                               "201 - 400", "401 - 600", "601 - 1600",
                               "No data"),
                    values = c("#fff7bc","#f5bc7c","#eca05f","#e1803e","#d4611d","#cc4c02"),
                    name = "Rangeland acres (1,000s)\nnewly forested")+
  theme(plot.background = element_rect(color="white",fill="white"),
        panel.border = element_blank(),
        panel.background =element_rect(fill = "transparent",color = NA),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position="right")
ggsave("figures/maps/county_map_50.pdf",width=6.5,height=4)
ggsave("figures/maps/county_map_50.png",width=6.5,height=4)
