# ENV872_Final_Project_Congo_Carbon

## Summary

This repository uses data from the Poulsen Tropical Ecology Lab, and contains information on tree diameter and species from plots in the Congo. This data is being collected in order to model the amount of above ground biomass (AGB) within each plot across the study sites. Utilizing remote sensing techniques, the Poulsen Lab performed analysis to determine several covariate values at each plot, including distance to nearest road, distance to nearest village, distance to nearest river, distance to nearest protected area, Human Footprint Index, and Globcover value. This project will look at which covariates have a significant impact on AGB at the study sites.

## Investigators

Nikki Egna -- Master's Student -- Duke University -- nicole.egna@duke.edu -- Primary investigator

Anna Nordseth -- PhD Candidate -- Duke University -- anna.nordseth@duke.edu -- Secondary investigator

John Poulsen -- Director -- Poulsen Tropical Ecology Lab -- Duke University -- john.poulsen@duke.edu -- Supervisor


## Keywords

Above ground biomass, remote sensing, congo, tropical ecology, tree growth

## Database Information

The CongoCarbon_Raw_Data.csv was collected by the Poulsen Tropical Ecology Lab, and their on-ground team based in the Congo. This data is not public facing, and accesswas granted through Dr. John Poulsen. The CongoCarbon_AGB_by_Plot.csv was created by John Poulsen, Anna Nordseth, and Nikki Egna based on the raw data. The covariate data (CongoCarbon_Plot_Covariates.csv) was created by Nikki Egna in March of 2020, and the data it utilizes was obtained from the following sources: Human Footprint Index was derived from Wildlife Conservation Society (WCS), Center for International Earth Science Information Network (CIESIN), and Columbia University and was accessed in December of 2019. The GlobCover raster was downloaded from the ESA Globcover 2005 Project, led by MEDIAS-France/POSTEL, in December of 2019. Precipitation data was obtained from the Climate Hazards group Infrared Precipitation with Stations (CHIRPS) in December of 2019. The roads, river, and villages data comes from the Ministère de l’Économie Forestière CNIAF_MEFDD (https://cog.forest-atlas.org), accessed in April, 2020.


## Folder structure, file formats, and naming conventions 

The repository contains folders called 'Code', 'Data', and 'Output'. The 'Code' folder contains all of the R scripts and Markdowns necessary for this research. The 'Data' folder contains the CSV's utilized for this project. This folder also has two subfolders, 'Processed' (for CSVs that have been altered by a script) and 'Raw' (for the CSV's that have not been altered). The raster and vector data for remote sensing analysis is within the 'Raw' folder as well. The 'Output' folder contains any maps or figures created in this research.

This repository contains CSV (Comma Separated Values) files for the raw and processed data, as well as raster for the remote sensing layers and vector data for the various covariates.

All CSV files are named beginning 'CongoCarbon_' and then includes a description of what the file is. For example, the CSV containing each plot and their associated covariate values is named 'CongoCarbon_Covariates_by_Plot.csv'. 

The raster layers are TIFF files that are named describing what the raster is. For example, Human Footprint Index is named HFI.tiff. The vector data are shapefiles that are contained within folders describing the shapefiles. For example, in the 'Raw' folder there is a rubfolder called 'Roads' which contains all necessary files for mapping the roads shapefile called 'CongoRoads.shp'.

## Metadata

### CongoCarbon_Raw_data.csv

Column Name | Description
----------- | -----------
Tree ID	    | Identification number assigned to the unique tree (Numeric)
Tag No      | Secondary tree identification number (Numeric)
Plot	      | Study plot number (Numeric)
Subplot	    | Subplot number within each plot (Numeric)
Family      | Tree family name (Character)
Species	    | Tree species name (Character)
Tree Notes  |	Notes from the field researcher about the tree(Character)
WD	        | Wood density (Numeric)
DBH0_05	    | First diameter measurement in 2005 (Numeric; Centimeters)
DBH1_05	    | Second diameter measurement in 2005 (Numeric; Centimeters)
DBH2_05	    | Third diameter measurement in 2005 (Numeric; Centimeters)
DBH3_05	    | Fourth diameter measurement in 2005 (Numeric; Centimeters)
DBH4_05	    | Fifth diameter measurement in 2005 (Numeric; Centimeters)
DBH0_09	    | First diameter measurement in 2009 (Numeric; Centimeters)
DBH1_09	    | Second diameter measurement in 2009 (Numeric; Centimeters)
DBH2_09	    | Third diameter measurement in 2009 (Numeric; Centimeters)
DBH3_09	    | Fourth diameter measurement in 2009 (Numeric; Centimeters)
DBH4_09   	| Fifth diameter measurement in 2009 (Numeric; Centimeters)
DBH0_13	    | First diameter measurement in 2013 (Numeric; Centimeters)
DBH1_13	    | Second diameter measurement in 2013 (Numeric; Centimeters)
DBH2_13	    | Third diameter measurement in 2013 (Numeric; Centimeters)
DBH3_13	    | Fourth diameter measurement in 2013 (Numeric; Centimeters)
DBH4_13	    | Fifth diameter measurement in 2013 (Numeric; Centimeters)
Census Notes| Notes from the field researcher about general survey topics (Character)

### CongoCarbon_Plot_Covariates

Column Name         | Description
------------------- | -----------
Plot	              | Tree plot ID number (Factor)
Latitude	          | Latitude of the plot (Numeric)
Longitude	          | Longitude of the plot (Numeric)
HFI	                | Human Footprint Index value at the plot location (Factor)
GlobCover           |	GlobCover vegetation index value at the plot location (Factor)
Precip_sum_2013	    | Annual precipitation sum for 2013 at plot location (Numeric; millimeters)
Dist_Road_m	        | Distance from plot to the nearest road (Numeric; Meters)
Dist_Village_m	    | Distance from plot to the nearest village (Numeric; Meters)
sum_AGB05	          | Sum of above ground biomass within the plot for 2005 (Numeric; g/m^2)
sum_AGB09           | Sum of above ground biomass within the plot for 2009 (Numeric; g/m^2)

### CongoCarbon_AGB_by_Plot.csv

Column Name         | Description
------------------- | -----------
Plot	              | Study plot number (Numeric)
sum_AGB05	          | Sum of above ground biomass within the plot for 2005 (Numeric; g/m^2)
sum_AGB09           | Sum of above ground biomass within the plot for 2009 (Numeric; g/m^2)
sum_AGB13           | Sum of above ground biomass within the plot for 2013 (Numeric; g/m^2)


## Scripts and code

Congo_Plots_Clean 191220.R takes uses information in the raw data file, such as diameter, tree species, and wood density, to estimates above ground biomass (AGB) for each tree. It corrects any mistake values and checks to make sure that the resulting AGB is all logical. It then sums the AGB by plot, creating the CongoCarbon_AGB_by_Plot.csv, which lists the plot number and the sum AGB for each year (2005, 2009, and 2013). The Congo_Mapping.R script extracts covariate values for each plot based upon different raster and vector data. For example, it reads a shapefile of the roads in Congo, and runs a function to determine the distance from each plot to the nearest road. It creates the CongoCarbon_Plot_Covariates.csv.

## Quality assurance/quality control

Resulting AGB caluculations were reviewed by Nikki Egna, Anna Nordseth, and John Poulsen to determine potential errors and erraneous values. For example, values of shrinking AGB from 2005 to 2013 would not be possible because trees that are not cut down cannot shrink, they can only grow larger. Thus "shrinking trees" were analyzed further in order to determine the error at hand. For these trees, the original paper field data logs were referenced to check if the errors were simply transcription mistakes. If this was not the case, the field coordinator was contacted to gain further clarity.

