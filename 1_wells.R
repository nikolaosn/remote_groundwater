
library(sf)
library(tmap)
tmap_mode("view")

library(sf)
library(ncdf4)

library(stars)
library(terra)





main_folder<-"initial_data/case_A"
#main_folder<-"initial_data/case_B"


site_inf<-read.csv(paste0(main_folder,"/well_data/SITE_INFO.csv"))
site_inf <- st_as_sf(site_inf, coords = c("DecLongVa", "DecLatVa"), crs = 4326)
waterlevel<-read.csv(paste0(main_folder,"/well_data/WATERLEVEL.csv"))
waterlevel<-waterlevel[ , colSums(is.na(waterlevel))==0]
waterlevel[,3]<-as.Date(waterlevel[,3]) 
split_waterlevel <- split(waterlevel, waterlevel$SiteNo)


well_ID=1
site_no<-as.character(site_inf[well_ID,2][[1]])
selected_element <- split_waterlevel[names(split_waterlevel) == site_no][[1]]
selected_element[,5]<-selected_element[,5]*0.3048 #transform from ft to meters
selected_element[,8]<-selected_element[,8]*0.3048 #transform from ft to meters

