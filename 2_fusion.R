library(imager)
library(matrixStats)


days_after<-0 #is there a recharge delay? set to zero (best version)
keep_a_part<-1 #if set to 1, only a portion of the image is used to feed the model

main_dimentions<-512 #image dimensions
new_height <- main_dimentions
new_width <- main_dimentions

where_are_the_nc_locations<-paste0(main_folder, "/safe_data/nc_data")
nc_names<-list.files(where_are_the_nc_locations)

main_list<-list()
metadata<-data.frame(matrix(ncol=5,nrow=1))
colnames(metadata)<-c("ID", "D_range", "Clouds_pec", "Date", "GT")
i=0; k=0
while(i<length(nc_names)){

  i=i+1
  nc_data_i <- nc_open(paste0(where_are_the_nc_locations,"/",nc_names[i]) )
  names_of_files<-nc_data_i$var$SCL$dim[[3]]$vals
  j=0
  while(j<length(names_of_files)){
    k=k+1
    print(k)
    j=j+1
    choose_file=names_of_files[[j]]
    target_date=as.Date("1990-01-01")+choose_file+days_after
    
    selected_element_new<-selected_element
    selected_element_new$Date <- as.Date(selected_element_new$Time)
    selected_element_new$Difference <- abs(selected_element_new$Date - target_date)
    closest_row <- selected_element_new[which.min(selected_element_new$Difference), ]
    d_dif<-as.numeric(closest_row$Difference)
    value_data<-as.numeric(closest_row[1,5])
    
    
    nc_SCL <- ncvar_get(nc_data_i, "SCL", start=c(1,1,j), count=c(-1,-1,1))
    nc_B03 <- ncvar_get(nc_data_i, "B03", start=c(1,1,j), count=c(-1,-1,1))
    nc_B08 <- ncvar_get(nc_data_i, "B08", start=c(1,1,j), count=c(-1,-1,1))
    nc_B11 <- ncvar_get(nc_data_i, "B11", start=c(1,1,j), count=c(-1,-1,1))
    
    if(keep_a_part==1){
      
      #for case A , croping the image
      nc_SCL<-nc_SCL[1:main_dimentions,1:main_dimentions]
      nc_B03<-nc_B03[1:main_dimentions,1:main_dimentions]
      nc_B08<-nc_B08[1:main_dimentions,1:main_dimentions]
      nc_B11<-nc_B11[1:main_dimentions,1:main_dimentions]

      #for case B , croping the image
      # nc_SCL<-nc_SCL[1:main_dimentions,200:(main_dimentions+200-1)]
      # nc_B03<-nc_B03[1:main_dimentions,200:(main_dimentions+200-1)]
      # nc_B08<-nc_B08[1:main_dimentions,200:(main_dimentions+200-1)]
      # nc_B11<-nc_B11[1:main_dimentions,200:(main_dimentions+200-1)]
    }

    #NDWI
    rgb_array<-(nc_B03-nc_B08)/(nc_B03+nc_B08)
    
    #annotate cloud percentages:
    cloud_classes <- c(8, 9, 10)
    total_pixels <- length(nc_SCL)
    cloudy_pixels <- sum(nc_SCL %in% cloud_classes)
    percentage_cloudy <- (cloudy_pixels / total_pixels) *100
    
    main_list[[k]]<-rgb_array
    metadata[[k,1]]<-k
    metadata[[k,2]]<-d_dif
    metadata[[k,3]]<-percentage_cloudy
    metadata[[k,4]]<-target_date
    metadata[[k,5]]<-value_data
  }
}



#subsetting using metadata:
cloud_perc_max<-25 #max clouds percentile
accepted_d_range<-0 #beta version. set to 0
Subseting<-TRUE

metadata_subset<-metadata
main_list_subset<-main_list

if(Subseting==TRUE){
metadata_subset<-metadata_subset[metadata_subset[,3]<=cloud_perc_max,]
metadata_subset<-metadata_subset[metadata_subset[,2]<=accepted_d_range,]
main_list_subset <- main_list_subset[metadata_subset$ID]
}

#remove NA images
i=0
save_na_image_id<-c()
while(i<length(main_list_subset)){
  i=i+1
  if(is.na(sum(main_list_subset[[i]]))==TRUE){
    save_na_image_id<-c(save_na_image_id, i)
  }
}

if (length(save_na_image_id)>0){
  main_list_subset<-main_list_subset[-save_na_image_id]
  metadata_subset<-metadata_subset[-save_na_image_id,]
}

#for validation perposes: Convert the array to an image and plot it
#plot(as.cimg(main_list_subset[[1]]))
nrow(metadata_subset)
TRUE%in%is.na(main_list_subset) #FALSE means no NA values

# we need only the following elements: main_list_subset and metadata_subset

