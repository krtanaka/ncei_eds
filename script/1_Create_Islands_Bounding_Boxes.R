################################################################
### Scripts to create bounding boxes around surveyed islands ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Edited & Maintained by K.R.Tanaka & T.A.Oliver ###
################################################################

rm(list = ls())

detach("package:plyr", unload = TRUE)

library(dplyr)
library(readr)
library(ggplot2)

load('data/SURVEY MASTER.RData'); df = SURVEY_MASTER

df$lon = df$LONGITUDE_LOV
df$lat = df$LATITUDE_LOV

drop_LatLonNAs = unique(c(which(is.na(df$lon)), which(is.na(df$lat))))
if(length(drop_LatLonNAs) > 0) df = df[-drop_LatLonNAs,] 
dim(df)

df = df %>% subset(REGION != "CT")

# df$lon = ifelse(df$lon < 0, df$lon + 180, df$lon)

###############
### mapping ###
###############

unique(df$REGION)
region = unique(df$REGION)[1]

# I tried different bounding methods... range(pretty()) seems reasonable. 

bound = 0.1

df %>%
  subset(REGION == region) %>%
  group_by(ISLAND) %>% 
  summarise(RIGHT_XMAX = min(lon)-bound,
            LEFT_XMIN = max(lon)+bound,
            TOP_YMAX = min(lat)-bound,
            BOTTOM_YMIN = max(lat)+bound) %>%
  ggplot() +
  annotation_map(map_data("world")) +
  geom_point(data = df %>% subset(REGION == region), aes(lon, lat, color = ISLAND), alpha = 0.5) +
  geom_rect(mapping = aes(
    xmin = LEFT_XMIN,
    xmax = RIGHT_XMAX,
    ymin = BOTTOM_YMIN,
    ymax = TOP_YMAX,
    fill = ISLAND,
    color = ISLAND), alpha = 0.2) + 
  facet_wrap(.~ISLAND, scales = "free") +
  theme(legend.position = "none") 


############################
### export as a csv file ###
############################

boxes = df %>%
  group_by(ISLAND) %>% 
  summarise(RIGHT_XMAX = min(lon)-bound,
            LEFT_XMIN = max(lon)+bound,
            TOP_YMAX = min(lat)-bound,
            BOTTOM_YMIN = max(lat)+bound)

df_frame = df[,c("REGION", "ISLAND")]

df = merge(boxes, df_frame)
df = df[!duplicated(df), ]

df = df[,c("REGION", "ISLAND", "TOP_YMAX", "BOTTOM_YMIN", "LEFT_XMIN", "RIGHT_XMAX")]
df$INCLUDED.ISLANDS = ""
colnames(df)[2] = "ISLAND.CODE"

df$ISLAND.CODE = gsub(" ", "_", df$ISLAND.CODE)

write_csv(df, 'data/Island_Extents.csv')
