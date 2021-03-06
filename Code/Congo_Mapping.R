
getwd()

### load packages
pacman::p_load(ggplot2, ggspatial,lubridate, dplyr, here, data.table, sp, devtools, rgdal, raster, maps, mapdata, maptools, geosphere, spatstat, tidyr, spdep, sf, lwgeom)
options(digits = 3)

plotcoo <- read.csv("./data/congo.plot.locations.2017.csv", header = T, stringsAsFactors = F)

plotcoo$plot <- as.character(plotcoo$plot)
sapply(plotcoo, class)

latlongProj = "+proj=longlat +ellps=WGS84 +no_defs"
aeaProj = "+proj=tmerc +lat_0=0 +lon_0=12 +k=0.9996 +x_0=500000 +y_0=500000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

Congo_spatial = SpatialPoints(coords = as.matrix(plotcoo[,c("lon", "lat")]), proj4string = CRS(latlongProj))

#set x-y limits for all plots
xRange = range(plotcoo$lon)
yRange = range(plotcoo$lat)


###GOOGLE MAP TERRAIN---no need to run unless you need imagery
#graphics off

remove.packages("ggmap")
remove.packages("ggplot2")

devtools::install_github("hadley/ggplot2@v2.2.0", force= TRUE)
devtools::install_github("dkahle/ggmap", force= TRUE)

library(ggmap)
library(ggplot2)

plotcoo <- data.frame(plotcoo)

api <- "AIzaSyABrFTwhDeG-_hVA-JJT5tSe8llJC0T_XA"
register_google(key = api)

TerrainMap_congo <- ggmap(get_map(location = c(16.25, 2.25), zoom = 10, maptype = "terrain"))
TerrainMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))

GoogleSatMap_congo <- ggmap(get_map(location = c(16.25,2.25), zoom = 10, maptype = "satellite"))
GoogleSatMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))


#remove.packages("ggplot2")
#install.packages("ggplot2")
#library(ggplot2)

###-------------------------------------------------------------###

### HFI ###

library(raster)

#load HFI raster
HFImap = raster("~/Documents/Congo/congo-mapping/HFI_Africa.tif") 

#If connecting via the remote drive
HFImap = raster("/Volumes/Research/Poulsen/Remote_Sensing/Continent_Africa_Data/Human Footprint Index/hfp_africa/HFI_Africa.tif") 
plot(HFImap)

#Crop HFI layer to extent (plus a little extra) of Congo plots
xRange = range(plotcoo$lon) + c(-1, 1) * diff(range(plotcoo$lon))
yRange = range(plotcoo$lat) + c(-1, 1) * diff(range(plotcoo$lat))
HFIcrop = crop(HFImap, extent(15.8,16.8,1.85,2.52))

library(spdep)

#check coordinate systems
st_crs(HFIcrop)
st_crs(Congo_spatial) 
# both in lat lon so good to go

#Extract HFI values at each plot location 
plotcoo$HFI <- raster::extract(HFIcrop, plotcoo[, c("lon", "lat")])


###-------------------------------------------------------------###

### GLOBAL COVER ###
glc <- raster("~/Documents/Congo/congo-mapping/globe_cover//GLOBCOVER_L4_200901_200912_V2.3.tif")

#If connecting via the remote drive
glc <- raster("/Volumes/Research/Poulsen/Remote_Sensing/Global_Data/GlobCover/GLOBCOVER_L4_200901_200912_V2.3.tif")

#check projection
st_crs(glc)

#Crop Global Cover layer to extent (plus a little extra) of Congo plots
glc.crop = crop(glc, extent(15.8,16.8,1.85,2.52))

plot(glc.crop)
points(x = plotcoo$lon, y = plotcoo$lat, pch = ".", xlab = "Longitude",
       ylab = "Latitude", main = "Sangha Plots on Global Cover")

#Extract Global Cover values at each plot location 
plotcoo$glob.cov <- raster::extract(glc, plotcoo[, c("lon", "lat")]) #buffer wasn't needed, same values w/in 50m


###-------------------------------------------------------------###

### CLOUD COVER ###

path <- list.files(path = "/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Gabon/EnvironmentalLayers/Cloud Cover", pattern = ".tif", recursive= T, full.names = T)
ras <- lapply(path, raster)

STACK1 <- stack(ras)
writeRaster(x = STACK1, filename = "CloudCover_Stacked_Monthly.tif", driver = "GeoTiff")


mean_stack <- calc(STACK1, fun = mean, na.rm = T)

mean_stack <- stackApply(STACK1, indices =  rep(1,nlayers(STACK1)), fun = "mean", na.rm = T)


mean_crop = crop(mean, extent(7, 15, -4, 3))
#plot(mean_crop)
#points(x = NRIdat$Long, y = NRIdat$Lat, pch = ".", cex=3, xlab = "Longitude", col="black",
      # ylab = "Latitude", main = "")

#Extract CC values at each plot location
#NRIdat$Cloud_Cover = extract(mean_crop, NRIdat[, c("Long", "Lat")])
#head(NRIdat)

###-------------------------------------------------------------###

### PRECIPITATION ###

plotcoo.plot_removed <- plotcoo[,c(2:3)]

# Years you want the data for (goes back to 2000)
years<- c(2013)

# Path to the Precipitation files on John's Drive

path <-"/Volumes/Research/Poulsen/Remote_Sensing/Continent_Africa_Data/CHIRPS_precip_monthly"
setwd(path)

for (year in years){
  precip_filenames <- list.files(path=paste0(path,"/",year), full.names=T, recursive=T, pattern = ".tif") #List precipitation files from remote drive
  precip_rasters <- lapply(precip_filenames,raster) #Read files in the designated path as rasters
  precip_stack <- stack(precip_filenames) #Stack the rasters (i.e. combine to form one raster)

  precip_sum <- sum(precip_stack, na.rm=TRUE) #Get the sum of the stacked rasters (i.e. the average yearly precipitation)
  
  plotcoo$precip<- raster::extract(precip_sum, plotcoo.plot_removed, stringsAsFactors = F) #Extract the mean annual precipitation at the Congo plot points and attach to the dataframe
  colnames(plotcoo)[which(names(plotcoo) == "precip")] <- paste0("precip_sum",year) #Rename the precipitation column to include the year
  
  writeRaster(precip_sum, paste0(path,"/Total_Annual_Precipitation_",year,".tif"), format="GTiff", overwrite=T) #Save the new single annual total raster as a .tif in the Precipitation folder
}

###-------------------------------------------------------------###

### DISTANCE TO ROAD ###

roads <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Roads/Réseau_routier/Réseau_routier.shp"))
st_crs(roads)
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

sfFromLongLat <- function(data, long, lat, crs = 4326) {
  sf <- st_as_sf(data, coords = c(long, lat), crs=crs)
  return(sf)
}

pts <- sfFromLongLat(plotcoo, "lon", "lat")
roads.sf <- st_as_sf(roads)

findMinDist <- function(point, edges) {
  dist <- min(st_distance(point, edges))
  return(dist)
}
  
for (i in 1:nrow(pts)) {
  row <- pts[i,]
  dist <- findMinDist(row, roads.sf)
  pts$dist.road.m[i] <- round(as.numeric(dist, 2))
}

#saveRDS(eas.92, eas.92.name)

###-------------------------------------------------------------###

### DISTANCE TO VILLAGE ### 

villages <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Villages/Localities/Localities.shp"))

st_crs(villages)
villages <- spTransform(villages, CRS("+proj=longlat +datum=WGS84 +no_defs"))

vil.sf <- st_as_sf(villages)

for (i in 1:nrow(pts)) {
  row <- pts[i,]
  dist <- findMinDist(row, vil.sf)
  pts$dist.vil.m[i] <- round(as.numeric(dist, 2))
}

###-------------------------------------------------------------###

### DISTANCE TO WATER COURSE ###
rivers <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Water_course/Water Course/Cours_d'eau.shp"))

rivers.sf <- st_as_sf(rivers)


for (i in 1:nrow(pts)) {
  row <- pts[i,]
  dist <- findMinDist(row, rivers.sf)
  pts$dist.riv.m[i] <- round(as.numeric(dist, 2))
}


###-------------------------------------------------------------###

### DISTANCE TO PROTECTED AREAS ###
PAs <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Protected Areas/Aires_protégées/Aires_protegees.shp"))

PAs.sf <- st_as_sf(PAs)

for (i in 1:nrow(pts)) {
  row <- pts[i,]
  dist <- findMinDist(row, PAs.sf)
  pts$dist.PAs.m[i] <- round(as.numeric(dist, 2))
}

###-------------------------------------------------------------###

### COMBINE ALL COVARIATES ###

plots_with_covariates <- merge(plotcoo, pts, by= "plot")
plots_with_covariates <- plots_with_covariates[,c("plot","lat","lon","HFI.x","glob.cov.x","precip_sum2013.x","dist.road.m","dist.vil.m","dist.riv.m","dist.PAs.m")]  
colnames(plots_with_covariates)[1:10] <- c("Plot","Latitude", "Longitude", "HFI", "GlobCover", "Precip_sum_2013", "Dist_Road_m", "Dist_Village_m","Dist_Rivers_m","Dist_PA_m")

AGB_by_Plot <- read.csv("../congo-carbon/data/plot-agb-summary-200415.csv")
AGB_by_Plot <- AGB_by_Plot[,-c(1)]

plots_with_covariates <- merge(plots_with_covariates, AGB_by_Plot, by="Plot")
write.csv(plots_with_covariates,"../congo-carbon/data/CongoCarbon_Plot_Covariates.csv")

###------------------------------------------------------------###

###------------------------- MAPPING -------------------------###

attach(plots_with_covariates)

TerrainMap_congo <- ggmap(get_map(location = c(16.25, 2.25), zoom = 10, maptype = "terrain"))
TerrainMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))

GoogleSatMap_congo <- ggmap(get_map(location = c(16.25,2.25), zoom = 10, maptype = "satellite"))
GoogleSatMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))

### Study Site Location Map ###
ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-30, 70), ylim = c(-45, 50), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  #scale_fill_manual(world$name_long=="Republic of the Congo", values = c("blue"))+
  theme_bw()



### HFI ###

HFIcrop.df <- as.data.frame(HFIcrop, xy = TRUE) 

#par(mfrow=c(2,2))
  
pdf(here("maps", "CongoCarbon_HFI_Map.pdf"), width = 11, height = 8.5)

ggplot() +
  geom_raster(HFIcrop.df, mapping= aes(x=x, y=y, fill= HFI_Africa)) +
  geom_point(data = plots_with_covariates, 
             mapping = aes(x = Longitude, y = Latitude, size= sum_AGB09),
             color="darkblue", alpha=0.4) +
  annotation_north_arrow(location = "br", which_north = "true") +
  scale_fill_gradient(limits= c(0, 30), low = "gray97", high = "red", na.value="white") +
  scale_size_continuous(name="Sum AGB 2009 (MgE)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB09))),max(pretty(range(plots_with_covariates$sum_AGB09))))) +
  labs(fill="HFI", x= expression("Longitude " ( degree*"E")), y=expression("Latitude " ( degree*"N"))) +
  theme_classic() +
  theme(legend.key = element_rect(fill = "white", colour = "white"))
  
dev.off()


### GlobCover ###

glc.crop.df <- as.data.frame(glc.crop, xy = TRUE) 

pdf(here("maps", "CongoCarbon_GlobCover_Map.pdf"), width = 11, height = 8.5)

ggplot(data = plots_with_covariates, 
       mapping = aes(x = Longitude, y = Latitude)) +
  geom_raster(glc.crop.df, mapping= aes(x=x, y=y, fill= GLOBCOVER_L4_200901_200912_V2.3)) +
  annotation_north_arrow(location = "br", which_north = "true") +
  geom_point(aes(size= sum_AGB09),color="darkblue", alpha=0.4) +
  scale_fill_gradient(low = "gray97", high = "green", na.value=NA) +
  scale_size_continuous(name="Sum AGB 2009 (MgE)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB09))),max(pretty(range(plots_with_covariates$sum_AGB09))))) +
  labs(fill="GlobCover", x= expression("Longitude " ( degree*"E")), y=expression("Latitude " ( degree*"N"))) +
  theme_classic() +
  theme(legend.key = element_rect(fill = NA, colour = NA))+
  coord_cartesian()

dev.off()


### Precipitation ###

##NEEDS WORK

pdf(here("maps", "CongoCarbon_Precipitation_Map.pdf"), width = 11, height = 8.5)

ggplot(data = plots_with_covariates, 
       mapping = aes(x = Longitude, y = Latitude, size= sum_AGB09)) +
  ggspatial::layer_spatial(precip_sum) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "br", which_north = "true") +
  geom_point(color="darkblue", alpha=0.7) +
  scale_fill_gradient(low = "gray97", high = "darkgrey", na.value=NA) +
  scale_size_continuous(name="Sum AGB 2009 (MgE)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB09))),max(pretty(range(plots_with_covariates$sum_AGB09))))) +
  labs(fill="Precipitation", x= expression("Longitude " ( degree*"E")), y=expression("Latitude " ( degree*"N"))) +
  theme_classic() +
  theme(legend.key = element_rect(fill = NA, colour = NA))

dev.off()


### Distance to road ###

pdf(here("maps", "CongoCarbon_Distance_Roads_Map.pdf"), width = 11, height = 8.5)

TerrainMap_congo +
  geom_path(aes(x=long, y=lat, group=group), data= fortify(roads), color= "gray22") +
  coord_map() +
  #annotation_scale(location = "tl") +
  #annotation_north_arrow(location = "br", which_north = "true") +
  geom_point(data = plots_with_covariates, mapping = aes(x = Longitude, y = Latitude, size= sum_AGB09, color= Dist_Road_m/1000), alpha=0.7) +
  scale_color_gradient(name="Distance to Road (Km)", low = "royalblue1", high = "midnightblue", na.value=NA) +
  scale_size_continuous(name="Sum AGB 2009 (MgE)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB09))),max(pretty(range(plots_with_covariates$sum_AGB09))))) +
  labs(x= expression("Longitude " ( degree*"E")), y=expression("Latitude " ( degree*"N"))) +
  theme_classic() +
  theme(legend.key = element_rect(fill = NA, colour = NA))

dev.off()

### Distance to Village ###

pdf(here("maps", "CongoCarbon_Distance_Village_Map.pdf"), width = 11, height = 8.5)

ggplot() +
  geom_sf(data= vil.sf, color= "darkgray") +
  coord_sf(xlim = c(xRange[1], xRange[2]), ylim = c(yRange[1], yRange[2]), expand = FALSE) +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "br", which_north = "true") +
  geom_point(data = plots_with_covariates, mapping = aes(x = Longitude, y = Latitude, size= sum_AGB09),color="darkblue", alpha=0.7) +
  #scale_fill_gradient(low = "gray97", high = "darkgrey", na.value=NA) +
  scale_size_continuous(name="Sum AGB 2009 (MgE)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB09))),max(pretty(range(plots_with_covariates$sum_AGB09))))) +
  #labs(fill="GlobCover") +
  theme_classic() +
  theme(legend.key = element_rect(fill = NA, colour = NA))

dev.off()



  