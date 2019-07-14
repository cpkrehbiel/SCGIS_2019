################################################################################
#          Working with AppEEARS Point Outputs in R: SCGIS Use Case            #
# -----------------------------------------------------------------------------#
# The goal of this script is to demonstrate how to use R to import AppEEARS    #
# Point Sampler output CSV files and filter by quality values. The script      #
# demonstrates how to write a short function to calculate Normalized           #
# Difference Vegetation Index (NDVI) values from Surface Reflectance (SR)      #
# data. Lastly, the script will show some basic visualization techniques for   #
# plotting the results, and how to export the plots and quality-filtered data. #
################################################################################
#                          1.  Setup Environment                               #
################################################################################
in_dir <- 'D:/SCGIS_Workshop/'              # IMPORTANT: Update to local dir
setwd(in_dir)                               # Set the working dir to the in_dir
out_dir <- paste(in_dir, 'output/', sep='') # Name output directory
suppressWarnings(dir.create(out_dir))       # Create output directory

################################################################################
#                 2. Import AppEEARS Output Point CSV Files                    #
################################################################################
# Search for (list.files()) and read AppEEARS output csv files (read.csv())
LST <- read.csv(list.files(pattern='.*MOD11A1-006-results.csv'),stringsAsFactors=FALSE)
SR <- read.csv(list.files(pattern='.*MCD43A4-006-results.csv'),stringsAsFactors=FALSE)
NDSI <- read.csv(list.files(pattern='.*MOD10A1.006-results.csv'),stringsAsFactors=FALSE)

# Remove unnecessary columns
names(LST)  # Print column names to determine which columns to delete
LST <- within(LST, rm("Latitude", "Longitude", "MODIS_Tile","MOD11A1_006_Sample_X_1km", "MOD11A1_006_Line_Y_1km" ))
SR <- within(SR, rm("Latitude", "Longitude", "MODIS_Tile","MCD43A4_006_Sample_X_500m", "MCD43A4_006_Line_Y_500m" ))
NDSI <- within(NDSI, rm("Latitude", "Longitude", "MODIS_Tile","MOD10A1_006_Sample_X_500m", "MOD10A1_006_Line_Y_500m"))

################################################################################
#                         3. Filter by Quality                                 #
################################################################################
# Start with Land Surface Temperatures (LST), exclude cloudy/poor quality obs
print(paste("Original Data includes ", length(LST$MOD11A1_006_LST_Day_1km), " Observations."))
LST <- LST[LST$MOD11A1_006_QC_Day_MODLAND_Description != 'LST not produced due to cloud effects',]
LST <- LST[LST$MOD11A1_006_QC_Day_Data_Quality_flag_Description != 'TBD',]
print(paste("Quality filtered Data has ", length(LST$MOD11A1_006_LST_Day_1km), " Observations remaining."))

# Make dataframe (df) with TrailCam ID, Date, Quality Filtered LST, & Catch Per Unit Effort (CPUE)
LST <- data.frame(LST$ID, LST$Date, LST$MOD11A1_006_LST_Day_1km, LST$Category)
colnames(LST) <- c("TrailCam", "Date", "LST", "CPUE")  # Set column names

# Next exclude poor quality Normalized Difference Snow Index (NDSI) observations
NDSI <- NDSI[NDSI$MOD10A1_006_NDSI_Snow_Cover_Basic_QA_Quality_Mask_Description != 'Unusable input or no data',]
NDSI <- NDSI[NDSI$MOD10A1_006_NDSI_Snow_Cover_Algorithm_Flags_QA_Inland_Water_Description != 'Yes',]
NDSI <- NDSI[NDSI$MOD10A1_006_NDSI_Snow_Cover_Algorithm_Flags_QA_Low_Visible_Screen_Description != 'Yes',]
NDSI <- NDSI[NDSI$MOD10A1_006_NDSI_Snow_Cover_Algorithm_Flags_QA_Low_NDSI_Screen_Description != 'Yes',]
NDSI <- NDSI[NDSI$MOD10A1_006_NDSI_Snow_Cover >= 0 & NDSI$MOD10A1_006_NDSI_Snow_Cover <= 100,]

# Make new df only including the TrailCam ID, Date, Quality Filtered NDSI, & CPUE
NDSI <- data.frame(NDSI$ID, NDSI$Date, NDSI$MOD10A1_006_NDSI_Snow_Cover, NDSI$Category)
colnames(NDSI) <- c("TrailCam", "Date", "NDSI", "CPUE") # Set column names

# Lastly, only include good quality Surface Reflectance Observations for both bands
srq = 'Processed, good quality (full BRDF inversions)'
SR <- SR[SR$MCD43A4_006_BRDF_Albedo_Band_Mandatory_Quality_Band1_MODLAND_Description == srq,]
SR <- SR[SR$MCD43A4_006_BRDF_Albedo_Band_Mandatory_Quality_Band2_MODLAND_Description == srq,]

# Make new df only including the TrailCam ID, Date, Quality Filtered SR Bands 1-2, & CPUE
SR <- data.frame(SR$ID, SR$Date, SR$MCD43A4_006_Nadir_Reflectance_Band1, SR$MCD43A4_006_Nadir_Reflectance_Band2, SR$Category)

################################################################################
#   4. Calculate Normalized Difference Vegetation Index (NDVI) from SR data    #
################################################################################
# Write a function to calculate NDVI from Red & NIR Reflectance
calc_NDVI <- function(x, y){(x - y) / (x + y)} # NDVI = (NIR - Red)/(NIR + Red)

# Call the NDVI function on the SR data
cNDVI = calc_NDVI(SR$SR.MCD43A4_006_Nadir_Reflectance_Band2, SR$SR.MCD43A4_006_Nadir_Reflectance_Band1)

# Write the output to a new df, including TrailCam ID, Date, & CPUE
NDVI <- data.frame(SR$SR.ID, SR$SR.Date, cNDVI, SR$SR.Category)
colnames(NDVI) <- c("TrailCam", "Date", "NDVI", "CPUE") # Set column names
rm(SR)                                                  # Remove SR

################################################################################
#                5. Plot Time Series Graphs for the daily data                 #
################################################################################
# Convert Dates from factor to Date
LST$Date <- as.Date(LST$Date)
NDSI$Date <- as.Date(NDSI$Date)
NDVI$Date <- as.Date(NDVI$Date)

# Boxplot time series using all of the trail cams
boxplot(LST$LST ~ LST$Date, xlab="Date" , col="#1F77B4", ylab="LST (K)" )
boxplot(NDSI$NDSI ~ NDSI$Date, xlab="Date" , col="#1F77B4", ylab="NDSI" )
boxplot(NDVI$NDVI ~ NDVI$Date, xlab="Date" , col="#1F77B4", ylab="NDVI" )

# Subset to a single trail cam
LSTtc <- LST[LST$TrailCam == "ELKBR069",]
NDVItc <- NDVI[NDVI$TrailCam == "ELKBR069",]
NDSItc <- NDSI[NDSI$TrailCam == "ELKBR069",]

# Plot the data for a single trail cam
plot(LSTtc$LST~LSTtc$Date, main="2015 Daily LST for Trail Cam: ELKBR069",
     ylab="LST (K)", xlab="Date", type="b",col="black", pch=21, bg="#1F77B4", cex=1.2)
plot(NDSItc$NDSI~NDSItc$Date, main="2015 Daily NDSI for Trail Cam: ELKBR069",
     ylab = "NDSI", xlab="Date", type="b",col="black", pch=21, bg="#1F77B4", cex=1.2)
plot(NDVItc$NDVI~NDVItc$Date, main="2015 Daily NDVI for Trail Cam: ELKBR069",
     ylab="NDVI", xlab="Date",type="b",col="black", pch=21, bg="#1F77B4", cex=1.2)

################################################################################
#                       6. Calculate Annual Indices                            #
################################################################################
# First, create master df with  Deer Catch Per Unit Effort 2015 Data
CPUE <- aggregate(CPUE ~ TrailCam, data=LST, FUN=function(x) CPUE=unique(x))

# Calculate last day with snow present by trail cam
NDSI <- NDSI[as.Date(NDSI$Date) < "2015-07-01",] # Remove second half of year
NDSI <- NDSI[NDSI$NDSI > 0,]                     # Remove NDSI = 0 (no snow)
NDSI$Date <- strftime(NDSI$Date, format="%j")  # Reformat date to Julian DOY
NDSI_stats <- aggregate(Date ~ TrailCam, data=NDSI, FUN=function(x) Date=max(x))

# Calculate max NDVI and mean annual LST by trail cam
NDVI_stats <- aggregate(NDVI ~ TrailCam, data=NDVI, FUN=function(x) c(Max=max(x)))
LST_stats <- aggregate(LST ~ TrailCam, data=LST, FUN=function(x) c(Mean=mean(x)))

CPUE$NDSI_LastSnow <- NDSI_stats$Date # Add NDSI stats to the CPUE df
CPUE$NDVI_Max <- NDVI_stats$NDVI   # Add NDVI stats to the CPUE df
CPUE$LST_Mean <- LST_stats$LST     # Add LST stats to the CPUE df

################################################################################
#            7. Plot NDVI, Snowcover, and LST vs. Deer CPUE data               #
################################################################################
par(mfrow=c(2,2))    # set the plotting area into a 2 x 2 array
plot(CPUE$NDSI_LastSnow ~ CPUE$CPUE, main="2015 Last Snow vs. Deer CPUE", ylab="Last Occurrence of Snow (DOY)",
     xlab="Count Per Unit Effort (Deer/day)", col="black", pch=21, bg="#1F77B4", cex=1.5)
plot(CPUE$NDVI_Max ~ CPUE$CPUE, main="2015 Max NDVI vs. Deer CPUE", ylab="Annual Maximum NDVI",
     xlab="Count Per Unit Effort (Deer/day)", col="black", pch=21, bg="#1F77B4", cex=1.5)
plot(CPUE$LST_Mean ~ CPUE$CPUE, main="2015 Mean LST vs. Deer CPUE", ylab="Annual Mean LST (K)",
     xlab="Count Per Unit Effort (Deer/day)", col="black", pch=21, bg="#1F77B4", cex=1.5)

################################################################################
#                   8. Export Results to Image and CSV Files                   #
################################################################################
# Export environmental descriptor vs. deer CPUE graphs to a png image file
invisible(dev.copy(png, paste0(out_dir, '2015_CPUEvsEnvironmentalDescriptors.png'),width=600, height=600))
invisible(dev.off())

# Export Results to CSV
write.csv(CPUE, file=paste0(out_dir, "SCGIS_Example_Results.csv"), row.names=FALSE)
rm(CPUE, LST, LST_stats, NDSI, NDSI_stats, NDVI, NDVI_stats, LSTtc, NDSItc, NDVItc, cNDVI, srq)

################################################################################
#                        9. Additional Information                             #
################################################################################
# Need to work with AppEEARS Area Outputs in R? Be sure to check out:
# https://lpdaac.usgs.gov/resources/e-learning/working-appeears-netcdf-4-output-data-r/
#
# Material written by Cole Krehbiel1
# Contact: LPDAAC@usgs.gov
# Voice: +1-605-594-6116
# Organization: Land Processes Distributed Active Archive Center (LP DAAC)
# Website: https://lpdaac.usgs.gov/
# Date last modified: 07-12-2019

# 1 Innovate! Inc., contractor to the USGS, EROS, Sioux Falls, SD, USA.
# Work performed under USGS contract G15PD00467 for LP DAAC2.
# 2 LP DAAC Work performed under NASA contract NNG14HH33I.
################################################################################