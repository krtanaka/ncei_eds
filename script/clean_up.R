library(dplyr)

load("outputs/Survey_Master_Timeseries_2021-02-27.Rdata"); df = SM; rm(SM)

glimpse(df)

df[df == -9991] <- NA

df = df[,c(4, 3, 2, 12, 16, 17, 52:380)]

colnames(df)[1:6] = c("Site", "Island", "Region", "Date", "Latitude", "Longitude")

names(df) <- gsub("Weekly", "weekly", names(df)); names(df)
names(df) <- gsub("Daily", "daily", names(df)); names(df)
names(df) <- gsub("_8Day_", "_8days_", names(df)); names(df)

names(df) <- gsub("_kdPAR_VIIRS_", "_KdPAR_", names(df)); names(df)
names(df) <- gsub("_Chlorophyll_A_ESAOCCCI_", "_Chla_", names(df)); names(df)
names(df) <- gsub("_PAR_MODIS_", "_PAR_", names(df)); names(df)
names(df) <- gsub("_Kd490_ESAOCCCI_", "_Kd490_", names(df)); names(df)
names(df) <- gsub("_SST_CRW_", "_SST_", names(df)); names(df)
names(df) <- gsub("DHW.", "", names(df)); names(df)
names(df) <- gsub("_Degree_Heating_Weeks_", "_DHW_", names(df)); names(df)

names(df) <- gsub("Major_", "major_", names(df)); names(df)
names(df) <- gsub("Np10y_", "", names(df)); names(df)
names(df) <- gsub("MeanMax_", "mean_maximum_", names(df)); names(df)
names(df) <- gsub("CI95Max_", "ci95_maximum_", names(df)); names(df)
names(df) <- gsub("MeanDur_", "mean_durnal_", names(df)); names(df)
names(df) <- gsub("MaxMax_", "maximum_maximum_", names(df)); names(df)

names(df) <- gsub("_MO03", "_03mo", names(df)); names(df)
names(df) <- gsub("_YR10YR01", "_10yr_01yr", names(df)); names(df)
names(df) <- gsub("_YR01", "_01yr", names(df)); names(df)
names(df) <- gsub("_YR03", "_03yr", names(df)); names(df)
names(df) <- gsub("_YR05", "_05yr", names(df)); names(df)
names(df) <- gsub("_YR10", "_10yr", names(df)); names(df)
names(df) <- gsub("_ALLB4", "_all_before", names(df)); names(df)

df = df %>% mutate(across(where(is.numeric), ~ round(., 3)))

readr::write_csv(df, "outputs/ncrmp_eds_final_2000-2020.csv")
