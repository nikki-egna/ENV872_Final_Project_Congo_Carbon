######## GLM ANALYSIS #######

#Main effects sum '05
AGB.main.05 <- lm(data = plots_with_covariates, sum_AGB05 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.05)

step(AGB.main.05)

#Best model
AGB.main.05 <- lm(formula = sum_AGB05 ~ Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m, 
                  data = plots_with_covariates)
summary(AGB.main.05)

#Main effects sum '09
AGB.main.09 <- lm(data = plots_with_covariates, sum_AGB09 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.09)

step(AGB.main.09)

#Best model
AGB.main.09 <- lm(formula = sum_AGB09 ~ Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m, 
                  data = plots_with_covariates)
summary(AGB.main.09)

#Main effects sum '13
AGB.main.13 <- lm(data = plots_with_covariates, sum_AGB13 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.13)

step(AGB.main.13)

#Best model
AGB.main.13 <- lm(formula = sum_AGB13 ~ Precip_sum_2013 + Dist_Road_m + Dist_Village_m + 
                    Dist_PA_m + Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.13)

#Main effects change '05-'09
AGB.main.0509 <- lm(data = plots_with_covariates, change0509 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0509)

step(AGB.main.0509)

#Best model
AGB.main.0509 <- lm(formula = sum_AGB13 ~ Precip_sum_2013 + Dist_Road_m + Dist_Village_m + 
                      Dist_PA_m + Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.0509)

#Main effects change '09-'13
AGB.main.0913 <- lm(data = plots_with_covariates, change0913 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0913)

step(AGB.main.0913)

#Best model
AGB.main.0913 <- lm(formula = change0913 ~ GlobCover + Precip_sum_2013, data = plots_with_covariates)
summary(AGB.main.0913)

#Main effects change '05-'13
AGB.main.0513 <- lm(data = plots_with_covariates, change0513 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0513)

step(AGB.main.0513)

AGB.main.0513 <- lm(formula = change0513 ~ GlobCover + Precip_sum_2013 + Dist_Road_m + 
                      Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.0513)



######### MAPPING ##########

getwd()

### load packages
pacman::p_load(ggplot2, ggmap, ggspatial,lubridate, dplyr, here, data.table, sp, devtools, rgdal, raster, maps, mapdata, maptools, geosphere, spatstat, tidyr, spdep, sf, lwgeom)
options(digits = 3)

plotcoo <- read.csv("./Data/Raw/congo.plot.locations.2017.csv", header = T, stringsAsFactors = F)

plotcoo$plot <- as.character(plotcoo$plot)
sapply(plotcoo, class)

latlongProj = "+proj=longlat +ellps=WGS84 +no_defs"
aeaProj = "+proj=tmerc +lat_0=0 +lon_0=12 +k=0.9996 +x_0=500000 +y_0=500000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

Congo_spatial = SpatialPoints(coords = as.matrix(plotcoo[,c("lon", "lat")]), proj4string = CRS(latlongProj))

#set x-y limits for all plots
xRange = range(plotcoo$lon)
yRange = range(plotcoo$lat)


#GOOGLE MAPS

plotcoo <- data.frame(plotcoo)

api <- "AIzaSyABrFTwhDeG-_hVA-JJT5tSe8llJC0T_XA"
register_google(key = api)

TerrainMap_congo <- ggmap(get_map(location = c(16.25, 2.25), zoom = 10, maptype = "terrain"))
TerrainMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))

GoogleSatMap_congo <- ggmap(get_map(location = c(16.25,2.25), zoom = 10, maptype = "satellite"))
GoogleSatMap_congo + geom_point(data = plotcoo, aes(x = lon, y = lat))


###-------------------------------------------------------------###
congo <- subset(world, world$name_long == "Republic of the Congo")
### Study Site Location Map ###
africa_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data= congo, color="black", fill= "darkgreen") +
  coord_sf(xlim = c(-30, 70), ylim = c(-45, 50), expand = FALSE)+
  #geom_rect(xmin= 10, xmax= 20.3, ymin= -6.5, ymax= 5, color="red", fill=NA, size=.7)+
  geom_rect(xmin= xRange[1], xmax= xRange[2], ymin= yRange[1], ymax= yRange[2], color="red", fill=NA, size=.8)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotate(geom = "label", x = 15, y = 9, label = "Republic of the Congo", 
           color = "black", size = 3, fill= "white") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x="Longitude", y="Latitude") +
  theme_bw()

africa_map

congo_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data= congo, color="black", fill= "darkgreen") +
  coord_sf(xlim = c(9.8, 20.5), ylim = c(-7.5, 5.8), expand = FALSE)+
  geom_rect(xmin= xRange[1], xmax= xRange[2], ymin= yRange[1], ymax= yRange[2], color="red", fill=NA, size=.8)+
  annotation_scale(location = "br", width_hint = 0.5) +
  annotate(geom = "label", x = 16.2, y = 3.1, label = "Study Region", 
           color = "black", size = 3, fill= "white") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x="Longitude", y="Latitude") +
  theme_bw()

congo_map

arrow <- data.frame(x1 = 3, x2 = 3, y1 = 9.5, y2 = 14.5)
plot_grid(africa_map, congo_map, nrow = 1, align="h", axis="b", rel_widths = c(1.25,1))

### HFI ###

#load HFI raster
HFImap = raster("~/Documents/Congo/congo-mapping/HFI_Africa.tif") 

#If connecting via the remote drive
HFImap = raster("/Volumes/Research/Poulsen/Remote_Sensing/Continent_Africa_Data/Human Footprint Index/hfp_africa/HFI_Africa.tif") 
plot(HFImap)

#Crop HFI layer to extent of Congo plots
HFIcrop = crop(HFImap, extent(15.8,16.8,1.85,2.52))

#check coordinate systems
st_crs(HFIcrop)
st_crs(Congo_spatial) 
# both in lat lon so good to go

### GLOBAL COVER ###
glc <- raster("~/Documents/Congo/congo-mapping/globe_cover//GLOBCOVER_L4_200901_200912_V2.3.tif")

#If connecting via the remote drive
glc <- raster("/Volumes/Research/Poulsen/Remote_Sensing/Global_Data/GlobCover/GLOBCOVER_L4_200901_200912_V2.3.tif")

#check projection
st_crs(glc)

#Crop Global Cover layer to extent (plus a little extra) of Congo plots
glc.crop = crop(glc, extent(15.8,16.8,1.85,2.52))

### DISTANCE TO ROADS ###

roads <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Roads/Réseau_routier/Réseau_routier.shp"))
st_crs(roads)
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

### DISTANCE TO VILLAGE ### 

villages <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Villages/Localities/Localities.shp"))

st_crs(villages)
villages <- spTransform(villages, CRS("+proj=longlat +datum=WGS84 +no_defs"))

vil.sf <- st_as_sf(villages)

### DISTANCE TO WATER COURSE ###
rivers <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Water_course/Water Course/Cours_d'eau.shp"))

rivers.sf <- st_as_sf(rivers)

### DISTANCE TO PROTECTED AREAS ###
PAs <- readOGR(dsn = path.expand("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Protected Areas/Aires_protégées/Aires_protegees.shp"))

PAs.sf <- st_as_sf(PAs)


