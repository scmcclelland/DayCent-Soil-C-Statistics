# filename:     geo-analysis.R    
# created:      20 April 2026
# last updated: 20 April 2026
# author:       Docker Clark

# description: 
#-------------------------------------------------------------------------------
# libraries 
#-------------------------------------------------------------------------------

library(data.table)
library(sf)
library(terra)

#-------------------------------------------------------------------------------
# create filepaths
#-------------------------------------------------------------------------------
#base data path
b_path <- "/gpfs/projects/McClellandGroup/projects/woodwell/DayCent-Soil-C-Statistics/data"

args    <- commandArgs(trailingOnly = TRUE) 
args[1] <- "analysis-input"
args[2] <- "analysis-output"
args[3] <- 'ccg-ntill'
args[4] <- '10-yr'
args[5] <- "delta-cumulative-SOC"

#assuming shp_p means shapefile path
shp_p <- paste(b_path, args[1], "shp", sep = "/")

#add a scenario
load(paste0(b_path, "/", args[1], "/",      #base file path
            args[4], "/", args[5], "-",     #time scale & SOC delta
            args[3],".RData"))              #scenario code and extension
#-------------------------------------------------------------------------------
# Code snippets from SC McClelland
#-------------------------------------------------------------------------------
#----------------------------------------------------------------
# ADD regions
#----------------------------------------------------------------
# read in shape file #~/analysis-input/shp
r_shp   <- st_read(paste(shp_p, 'WB_countries_Admin0_10m.shp', sep = '/'))
# read in crop mask
r       <- rast(paste(b_path, args[1], 'msw-cropland-rf-ir-area.tif', sep = '/'))
# keep first layer
r       <- r[[1]]

# create function
# inputs are the shape file and the raster (above)
create_WB_cty = function(shp_f, rst) {
  # ORIGINAL WB NAME and OBJECTID
  shp_dt          = setDT(as.data.table(st_drop_geometry(shp_f)))
  # MATCH resolution of simulation data, dimensions the same
  target.r    = rst
  # CONFIRM SHP in same crs as target
  country.sf  = st_transform(shp_f, crs(target.r))
  country_r   = terra::rasterize(
    x         = vect(country.sf),
    y         = target.r,
    field     = "OBJECTID",
    touches   = TRUE          # optional: include cells touched by polygons
  )
  # CREATE data.frame, merge
  new_shp_dt  = as.data.frame(country_r, cells = TRUE, xy = TRUE)
  new_shp_dt  = setDT(new_shp_dt) # data.table object
  # GET WB names to match to ID
  shp_names   = data.table(WB_NAME   = shp_dt$WB_NAME,
                           ID        = shp_dt$OBJECTID)
  # JOIN with WB names
  new_shp_dt  = new_shp_dt[shp_names, on = .(OBJECTID = ID)]  
  return(new_shp_dt)
}
###### Working through this to understand it #########
############# stopped here 4/20/206 ##################
# create country data table with function
WB_dt = create_WB_cty(r_shp, r)
# join country data table to simulation data
dt_scenario = dt_scenario[WB_dt[,c('cell', 'WB_NAME')], on = .(gridid = cell)]
# remove NAs
dt_scenario = dt_scenario[!is.na(crop)]
setorder(dt_scenario, gridid)
gc() #garbage collection

#----------------------------------------------------------------
# ADD IPCC REGION NAMES
#----------------------------------------------------------------
# we may want to break out DEV (developed countries)
# the DEV grouping doesn't make sense environmentally
# for example, we might want 'North America' 'Europe' and 'Oceania'
# it might make sense to merge some of those into existing groups
# 
# IPCC Region Names (AR6 & Roe et al. 2021)
# Africa and Middle East
AME   = c('Congo, Democratic Republic of', 'Nigeria', 'Tanzania', 'South Africa', 'Congo, Rep. of', 'Zambia',
          'Angola', 'Cameroon', 'Ethiopia', 'Mozambique', 'Iran, Islamic Republic of', 'Uganda',
          'Central African Republic', 'Gabon', 'Sudan', "Côte d'Ivoire", 'Kenya', 'Egypt, Arab Republic of',
          'Ghana', 'Zimbabwe', 'Mali', 'Namibia', 'South Sudan', 'Chad', 'Morocco', 'Botswana', 'Burkina Faso',
          'Niger', 'Guinea', 'Algeria', 'Liberia', 'Malawi', 'Senegal', 'Somalia', 'Saudi Arabia', 'Benin', 
          'Sierra Leone', 'Iraq', 'Rwanda', 'Eritrea', 'eSwatini', 'Benin', 'Burundi', 'Djibouti', 'Equatorial Guinea',
          'Madagascar', 'Mauritania', 'Tunisia', 'Syrian Arab Republic', 'Lebanon', 'Jordan', 'Libya', 'Israel', 
          'West Bank and Gaza', 'Kuwait', 'Oman', 'Qatar', 'United Arab Emirates', 'Yemen, Republic of', 'Cabo Verde',
          'Guinea-Bissau', 'Togo', 'Comoros', 'Mauritius', 'Lesotho')
ADP   = c('China', 'Indonesia', 'India', 'Myanmar', 'Vietnam', 'Malaysia', 'Thailand', 'Pakistan', 'Papua New Guinea',
          'Philippines', 'Bangladesh', 'Cambodia', "Lao People's Democratic Republic", 'Mongolia', 'Korea, Republic of',
          'Afghanistan', 'Nepa', 'Sri Lanka', "Korea, Democratic People's Republic of", 'Solomon Islands', 'Bhutan',
          'Timor-Leste', 'Fiji', 'Nepal', 'Hong Kong (SAR, China)', 'Brunei Darussalam', 'Samoa', 'Vanuatu', 'Tonga')
DEV   = c('United States of America', 'Canada', 'Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Czech Republic', 'Denmark',
          'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Latvia', 'Lithuania', 'Luxembourg',
          'Netherlands', 'Poland', 'Portugal', 'Romania', 'Slovak Republic', 'Slovenia','Spain', 'Sweden', 'United Kingdom', 'Australia', 'Ukraine',
          'Japan', 'Turkey', 'New Zealand', 'Norway', 'Iceland', 'Greenland (Den.)', 'Faroe Islands (Den.)', 'Switzerland', 'Saint-Pierre-et-Miquelon (Fr.)',
          'Cyprus', 'Puerto Rico (US)', 'American Samoa (US)', 'Saint Helena, Ascension and Tristan da Cunha (UK)', 'New Caledonia (Fr.)',
          'French Southern and Antarctic Lands (Fr.)', 'Falkland Islands (UK)/Islas Malvinas', 'South Georgia and South Sandwich Islands (UK)')
EEWCA = c('Russian Federation', 'Kazakhstan', 'Belarus', 'Uzbekistan', 'Turkmenistan', 'Kyrgyz Republic', 'Azerbaijan',
          'Moldova', 'Tajikistan', 'Armenia', 'Serbia', 'Bosnia and Herzegovina', 'Georgia', 'Montenegro', 'Kosovo', 'Albania',
          'North Macedonia')
LAC   = c('Brazil', 'Colombia', 'Mexico', 'Argentina', 'Bolivia', 'Peru', 'Venezuela', 'Paraguay', 'Ecuador', 'Chile', 'Guyana', 'Suriname',
          'Cuba', 'Uruguay', 'Honduras', 'Nicaragua', 'Guatemala', 'Guyana', 'Costa Rica', 'Panama', 'Dominican Republic', 'El Salvador', 'Belize',
          'Bahamas, The', 'Haiti', 'Turks and Caicos Islands (UK)', 'Jamaica', 'Venezuela, Republica Bolivariana de', 'Trinidad and Tobago')
#creating IPCC names
dt_scenario[WB_NAME %in% AME, IPCC_NAME   := 'AME']
dt_scenario[WB_NAME %in% ADP, IPCC_NAME   := 'ADP']
dt_scenario[WB_NAME %in% DEV, IPCC_NAME   := 'DEV']
dt_scenario[WB_NAME %in% EEWCA, IPCC_NAME := 'EEWCA']
dt_scenario[WB_NAME %in% LAC, IPCC_NAME   := 'LAC']

# check if missing but the groups above should capture everything