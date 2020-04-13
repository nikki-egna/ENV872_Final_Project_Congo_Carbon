# ENV872_Final_Project_Congo_Carbon

## Summary

This repository uses data from the Poulsen Tropical Ecology Lab, and contains information on tree diameter and species from plots in the Congo. This data is being collected in order to model the amount of above ground biomass (AGB) within each plot across the study sites. Utilizing remote sensing techniques, the Poulsen Lab performed analysis to determine several covariate values at each plot, including distance to nearest road, distance to nearest village, distance to nearest river, distance to nearest protected area, Human Footprint Index, and Globcover value. This project will look at which covariates have a significant impact on AGB at the study sites.

## Investigators

Nikki Egna
Master's Student
Duke University
nicole.egna@duke.edu
Primary investigator

Anna Nordseth
PhD Candidate
Duke University
anna.nordseth@duke.edu
Secondary investigator

John Poulsen
Director
Poulsen Tropical Ecology Lab
Duke University
john.poulsen@duke.edu


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

<For each data file in the repository, describe the data contained in each column. Include the column name, a description of the information, the class of data, and any units associated with the data. Create a list or table for each data file.> 

## Scripts and code

<list any software scripts/code contained in the repository and a description of their purpose.>

## Quality assurance/quality control

<describe any relevant QA/QC procedures taken with your data. Some ideas can be found here:>
<https://www.dataone.org/best-practices/develop-quality-assurance-and-quality-control-plan>
<https://www.dataone.org/best-practices/ensure-basic-quality-control>
<https://www.dataone.org/best-practices/communicate-data-quality>
<https://www.dataone.org/best-practices/identify-outliers>
<https://www.dataone.org/best-practices/identify-values-are-estimated>

