#Set working directory
setwd("~/Desktop/Classes/Spring 2020/Environmental Data Analytics/ENV872_Final_Project_Congo_Carbon")

#Load packages
pacman::p_load(ggplot2, ggspatial, cowplot, lubridate, dplyr, here, data.table, sp, devtools, rgdal, raster, maps, mapdata, maptools, reshape2, geosphere, spatstat, tidyr, spdep, sf, lwgeom)

#Read in data
plots_with_covariates <- read.csv("./Data/Processed/CongoCarbon_AGB_and_Covariates_by_Plot.csv")
CongoCarbon_Raw_Data <- read.csv("./Data/Raw/CongoCarbon_Raw_Data.csv")

#Test for normality of AGB in 2013
summary(CongoCarbon_Raw_Data$AGB13.MgE)
class(CongoCarbon_Raw_Data$AGB13.MgE)

Raw.Data.subsample <- sample_n(CongoCarbon_Raw_Data, 5000)
shapiro.test((Raw.Data.subsample$AGB13.MgE))
#Reject the null, data not normal

#Graphic representation of abnormality
ggplot(CongoCarbon_Raw_Data, aes(x = AGB13.MgE)) +
  geom_histogram() 
qqnorm(CongoCarbon_Raw_Data$AGB13.MgE); qqline(CongoCarbon_Raw_Data$AGB13.MgE)



######## GLM ANALYSIS #######

#Main effects sum '13
AGB.main.13 <- lm(data = plots_with_covariates, sum_AGB13 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.13)

step(AGB.main.13)

#Best model
AGB.main.13 <- lm(formula = sum_AGB13 ~ Precip_sum_2013 + Dist_Road_m + Dist_Village_m + 
                    Dist_PA_m + Dist_Saw_Mills_m, data = plots_with_covariates)
summary(AGB.main.13)


#Main effects change '09-'13
AGB.main.0913 <- lm(data = plots_with_covariates, change0913 ~ HFI + GlobCover + Soil + Precip_sum_2013 + Dist_Road_m + Dist_Village_m + Dist_PA_m + Dist_Saw_Mills_m)
summary(AGB.main.0913)

step(AGB.main.0913)

#Best model
AGB.main.0913 <- lm(formula = change0913 ~ GlobCover + Precip_sum_2013, data = plots_with_covariates)
summary(AGB.main.0913)


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

TerrainMap_congo <- get_map(location = c(16.3, 2.2), zoom = 10, maptype = "terrain")
GoogleSatMap_congo <- get_map(location = c(16.25,2.25), zoom = 10, maptype = "satellite")


###-------------------------------------------------------------###

### Study Site Location Map ###
congo <- subset(world, world$name_long == "Republic of the Congo")

africa_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data= congo, color="black", fill= "darkgreen") +
  coord_sf(xlim = c(-21, 55), ylim = c(-45, 45), expand = FALSE)+
  #geom_rect(xmin= 10, xmax= 20.3, ymin= -6.5, ymax= 5, color="red", fill=NA, size=.7)+
  geom_rect(xmin= xRange[1], xmax= xRange[2], ymin= yRange[1], ymax= yRange[2], color="red", fill=NA, size=.8)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotate(geom = "label", x = 15, y = 8, label = "Republic of the Congo", 
           color = "black", size = 5, fill= "white") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x="", y="") +
  theme_bw()

africa_map

congo_map <- ggplot(data = world) +
  geom_sf() +
  geom_sf(data= congo, color="black", fill= "darkgreen") +
  coord_sf(xlim = c(9.8, 20.5), ylim = c(-7.5, 5.8), expand = FALSE)+
  geom_rect(xmin= xRange[1], xmax= xRange[2], ymin= yRange[1], ymax= yRange[2], color="red", fill=NA, size=.8)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotate(geom = "label", x = 16.2, y = 3.1, label = "Study Region", 
           color = "black", size = 5, fill= "white") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.35, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(x="", y="") +
  theme_bw()

congo_map

#arrow <- data.frame(x1 = 18.5, x2 = 23, y1 = 9.5, y2 = 14.5)

combined_africa_congo_map <- plot_grid(africa_map, congo_map, nrow = 1, align="h", axis="b", rel_widths = c(1.06,1))


pdf(here("Output", "Africa_Congo_Site_Map.pdf"), width = 11, height = 8.5)

ggdraw(combined_africa_congo_map)+
 geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrow, 
             arrow = arrow(), lineend = "round")
dev.off()


### ROADS ###

roads <- st_read("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Roads/Réseau_routier/Réseau_routier.shp")
st_crs(roads)
roads <- spTransform(roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))

roads.sf <- st_as_sf(roads)

### VILLAGE ### 

villages <- st_read("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Villages/Localities/Localities.shp")
st_crs(villages)

vil.sf <- st_as_sf(villages)

### PROTECTED AREAS ###

PAs <- st_read("/Volumes/Research/Poulsen/Remote_Sensing/Country_Data/RS_Congo/Protected Areas/Aires_protégées/Aires_protegees.shp")
PAs.sf <- st_as_sf(PAs)

pdf(here("Output", "Roads_Villages_PA_Plot.pdf"), width = 11, height = 8.5)

ggmap(TerrainMap_congo) +
  geom_sf(data= roads, color= "gray22", inherit.aes = FALSE) +
  geom_sf(data= villages, size=4, shape=24, color= "black", fill= "orange",inherit.aes = FALSE) +
  geom_sf(data= PAs, color="darkgreen", fill="darkgreen", alpha=0.3, inherit.aes = FALSE) +
  coord_sf() +
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tl", which_north = "true",pad_y = unit(0.25, "in")) +
  geom_point(data = plots_with_covariates, mapping = aes(x = Longitude, y = Latitude, size= sum_AGB13), color="darkblue",alpha=0.7) +
  #scale_color_gradient(name="Distance to Road (Km)", low = "royalblue1", high = "midnightblue", na.value=NA) +
  scale_size_continuous(name="Sum AGB 2013 (Mg)", limits=c(min(pretty(range(plots_with_covariates$sum_AGB13))),max(pretty(range(plots_with_covariates$sum_AGB13))))) +
  labs(x= "", y= "") +
  theme_classic() +
  theme(legend.key = element_rect(fill = NA, colour = NA))

dev.off()
