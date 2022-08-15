#################################################################
### Scripts to attach climatologies variables to in situ data ###
### Originally developed & conceptualized by T.A.Oliver       ###
### Revised & Edited & Maintained by K.R.Tanaka               ###
#################################################################

rm(list = ls())

library(spatial)
library(raster)
library(lubridate)
library(raster)
library(visdat)

dir = getwd()

# import Tom's functions
source("scripts/HelperCode/ExpandingExtract.R")

# import survey data, SM = master REA survey file, subset if necessary 
load('data/SURVEY MASTER.RData'); SM = SURVEY_MASTER
table(SM$REGION)
# SM = subset(SM, REGION == "MARIAN")

SM$LONGITUDE_LOV = ifelse(SM$LONGITUDE_LOV < 0, SM$LONGITUDE_LOV + 360, SM$LONGITUDE_LOV)

# remove NAs in lat & lon columns, then turn it into spatial object
SM = SM[complete.cases(SM[,c("LONGITUDE_LOV", "LATITUDE_LOV")]), ]
SM_sp = SM; SM_sp = as.data.frame(SM_sp)
coordinates(SM_sp) = ~LONGITUDE_LOV + LATITUDE_LOV

# get list of rasters (i.e., climatologies)
rasterlist = list.files(c("~/Desktop/EDS/DataDownload/Chlorophyll_A_Monthly_MODIS_2017//Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/Chlorophyll_A_Monthly_MODIS_2018//Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/Chlorophyll_A_Monthly_MODIS_2019//Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/PAR_Monthly_2017/Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/PAR_Monthly_2018/Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/PAR_Monthly_2019/Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/SST_Monthly_2017/Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/SST_Monthly_2018/Domain_Level_Data/mean/",
                          "~/Desktop/EDS/DataDownload/SST_Monthly_2019/Domain_Level_Data/mean/"),
                        recursive = T, 
                        pattern = "_AllIslands.nc", 
                        full.names = T)

# see all rasterarized climatological variables
strsplit(rasterlist, "/")

###################
### normal loop ###
###################
start = Sys.time()
for(raster_i in 1:length(rasterlist)){
  
  # raster_i = 1
  
  rasname_full = rasterlist[raster_i] 
  rasname_sp = strsplit(rasname_full, "/")[[1]]
  rasname = rasname_sp[length(rasname_sp)]
  rasname = gsub(rasname, pattern = "-", replacement = ".")
  rasname = gsub(rasname, pattern = "_AllIslands.nc", replacement = "")
  
  this_r = raster(rasterlist[raster_i])
  
  if (this_r@extent@xmin < 0) this_r = shift(rotate(shift(this_r, 180)), 180)
  
  crs(SM_sp) = crs(this_r) 
  
  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": ", rasname))
  
  this_Ex = ExpandingExtract(this_r, SM_sp, Dists = seq(0, 50, 10))
  
  eval(parse(text = paste0("SM_sp$", rasname, " = this_Ex$values")))
  
  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": Extraction Complete."))
  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": Write Out Complete."))
  
}
stop = Sys.time()
start - stop
beepr::beep(2)

df = as.data.frame(SM_sp)
df = df[,c(2:4, 16:17, 48:length(names(df)))]

vis_miss(df)

write_csv(df,  paste0("outputs/NCRMP_Climatologies_", Sys.Date(), ".csv"))

save(df, file = paste0("outputs/NCRMP_Climatologies_", Sys.Date(), ".RData"))

library(dplyr)
library(ggplot2)

df %>%
  subset(REGION == "MHI") %>%
  subset(ISLAND == "Molokai") %>%
  group_by(LONGITUDE_LOV, LATITUDE_LOV, ISLAND) %>% 
  summarise(ev = mean(Chlorophyll_A_Monthly_MODIS_2017_mean_2002.07.16_2017.12.16, na.rm = T)) %>%
  ggplot(aes(LONGITUDE_LOV, LATITUDE_LOV, fill = ev, color = ev)) +
  geom_point(shape = 21, size = 3, alpha = 0.8) +
  scale_fill_viridis_c(trans = "log") + 
  scale_color_viridis_c(trans = "log")

