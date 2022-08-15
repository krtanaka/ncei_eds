###########################################################################
### Mask Ocean Color Datasets using STRM Bathymetry and Topography data ###
###########################################################################

rm(list = ls())

library(raster)
library(dplyr)
library(maps)
library(readr)
library(ncdf4)
library(filesstrings)
library(ggplot2)

#################################################################
### load STRM Bathymetry and Topography data                  ###
### Shuttle Radar Topography Mission (SRTM)                   ###
### Global Bathymetry and Topography at 15 Arc Sec: SRTM15+   ###
### https://doi.org/10.1029/2019EA000658                      ###
#################################################################
# dir = "G:/Environmental Data Summary/DataDownload/" # backup
dir = "M:/Environmental Data Summary/DataDownload/" # main
dir = "/Users/kisei.tanaka/Desktop/EDS/DataDownload/" # main

STRM15_360 = raster(paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc"))
STRM15_180 = raster(paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc"))

oc = read_csv("data/EDS_parameters.csv")

oc = oc %>% subset(DOWNLOAD == "YES")

oc

var_list = oc$PARAMETER.NAME
var_list = var_list[-which(var_list %in% c("WaveHeight_Monthly"))]

data_list = oc$DATASET.ID

date_start = gsub("/", "_", oc$START_DATE)
date_end = gsub("/", "_", oc$STOP_DATE)

col_name_list = paste(data_list, date_start, date_end, sep = "_")

start_time <- Sys.time()

print(paste0("Running through ",length(var_list)," parameters: "))

print(var_list)

for (i in 1:length(var_list)) {
  
  # i = 3
  
  var_name = var_list[i]; var_name
  col_name = col_name_list[i]; col_name
  
  path = paste0(dir, var_name, "/Island_Level_Data/")
  island_nc_files = list.files(path = path, pattern = ".nc"); island_nc_files
  if (dir.exists(paste0(path, "unmasked/"))) island_nc_files = island_nc_files[grepl('_30meter_5pct_mask_w_STRM15', island_nc_files)]; island_nc_files

  var_island = NULL
  
  for (island_i in 1:length(island_nc_files)) {
    
    # island_i = 10
    
    # Get filenames
    island_nc_file_name = island_nc_files[island_i]
    
    var_df = stack(paste0(path, island_nc_file_name))
    var_df = mean(var_df, na.rm = T)
    
    if (var_df@extent@xmin < 0) {
      
      strm = STRM15_180
      
    } else {
      
      strm = STRM15_360
      
    }
    
    cropped_strm = crop(strm, extent(var_df));# beepr::beep(2)
    cropped_strm[cropped_strm > 0] <- NA 
    cropped_strm[cropped_strm < -50] <- NA 
    cropped_strm = resample(cropped_strm, var_df, method = "bilinear") 
    
    var_df = stack(var_df, cropped_strm)
    var_df = rasterToPoints(var_df) %>% as.data.frame()
    var_df$z = ifelse(is.na(var_df$layer.2) == T, NA, var_df$layer.1)
    var_df = var_df[,c("x", "y", "z")]
    if (dim(na.omit(var_df))[1] != 0) var_df = var_df %>% na.omit() 
    var_df$Island = strsplit(island_nc_file_name, split = paste0("_", var_name))[[1]][1]
    colnames(var_df)[1:3] = c("Lon", "Lat", col_name)
    
    var_island = rbind(var_island, var_df) 
    
    print(island_nc_file_name)
    
  }# island loop
  
  png(paste0("outputs/", col_name, ".png"), height = 7, width = 10, res = 500, units = "in")
  (var_island %>% 
      ggplot(aes_string("Lon", "Lat", fill = names(var_island)[3], color = names(var_island)[3])) +
      geom_point(shape = 21, alpha = 0.5) + 
      scale_fill_viridis_c() + 
      scale_color_viridis_c() + 
      coord_fixed() + 
      theme(legend.position = "bottom"))
  dev.off()
  
  write_csv(var_island, file = paste0("outputs/", col_name, ".csv"))
  
} #param loop
