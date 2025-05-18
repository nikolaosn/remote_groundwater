library(reticulate)
library(tensorflow)
library(keras3)
library(withr)
library(ggplot2)
library(abind)

#main_list_subset, metadata_subset, minivgg_model and index are the only ones needed


main_dimentions<-512
images <- array(unlist(main_list_subset), dim = c(main_dimentions,main_dimentions, 1, length(main_list_subset)))
dim(images)
images <- aperm(images, c(4, 1, 2, 3))
dim(images)

train_images_raw  <- images[index, ,,] #plot(as.cimg(train_images_raw[1,,]))
test_images_raw   <- images[-index, ,,]

##  Flatten the array to calculate the percentiles
flattened_array <- as.vector(train_images_raw)

##  Calculate the 5th and 95th percentiles
lower_bound <- quantile(flattened_array, 0.000001)
upper_bound <- quantile(flattened_array, 0.999999)

normalize_images <- function(img, low, high) {
  norm <- (img - low) / (high - low)
  norm[norm < 0] <- 0
  norm[norm > 1] <- 1
  return(norm)
}

train_images <- normalize_images(train_images_raw, lower_bound, upper_bound)
test_images  <- normalize_images(test_images_raw,  lower_bound, upper_bound)


# Splitting the data into the same train and test sets
train_images <- array_reshape(train_images, c(nrow(train_images), main_dimentions, main_dimentions, 1))
labels<-metadata_subset[,5]
train_labels <- labels[index]
test_images <- array_reshape(test_images, c(nrow(test_images), main_dimentions, main_dimentions, 1))
test_labels <- labels[-index]


all_dates<-metadata_subset[,4]
train_dates<-all_dates[index]
test_dates<-all_dates[-index]

all_clouds<-metadata_subset[,3]
train_clouds<-all_clouds[index]
test_clouds<-all_clouds[-index]

all_Drange<-metadata_subset[,2]
train_Drange<-all_Drange[index]
test_Drange<-all_Drange[-index]

all_ID<-metadata_subset[,1]
train_ID<-all_ID[index]
test_ID<-all_ID[-index]



predictions <- minivgg_model %>% predict(test_images)

restored_predictions <- predictions 
restored_test_labels <- test_labels

#results dataframe
results_dataf<-data.frame(matrix(ncol=6,nrow=length(labels)))

#first: actual values (train), then predicted values (test)
results_dataf[1:length(train_dates),1]<-train_labels
results_dataf[(length(train_dates)+1):length(labels),1]<-test_labels
#first: actual values (train), then predicted values (test)
results_dataf[1:length(train_dates),2]<-NA
results_dataf[(length(train_dates)+1):length(labels),2]<-predictions
#dates
results_dataf[1:length(train_dates),3]<-train_dates
results_dataf[(length(train_dates)+1):length(labels),3]<-test_dates
results_dataf[,3] <- as.Date(results_dataf[, 3], origin = "1970-01-01")
#days_dif
results_dataf[1:length(train_dates),4]<-train_clouds
results_dataf[(length(train_dates)+1):length(labels),4]<-test_clouds
#clouds
results_dataf[1:length(train_dates),5]<-train_Drange
results_dataf[(length(train_dates)+1):length(labels),5]<-test_Drange
#ID
results_dataf[1:length(train_dates),6]<-train_ID
results_dataf[(length(train_dates)+1):length(labels),6]<-test_ID
#D_GT
results_dataf$D_GT<-abs(results_dataf[,1]-results_dataf[,2])
colnames(results_dataf)<-c("GT","predicted", "Date","Clouds_pec","D_range","ID", "D_GT")


cloud_percentile_to_subset<-10
results_dataf_subs<-results_dataf
results_dataf_subs<-results_dataf_subs[is.na(results_dataf_subs[,7])==FALSE,]
results_dataf_subs<-results_dataf_subs[results_dataf_subs[,4]<cloud_percentile_to_subset,]


gi_mi_ID_to_plot<-10
row_ID_to_plot <- which(metadata_subset$ID == gi_mi_ID_to_plot)
#plot(as.cimg(images[row_ID_to_plot, ,,]))


# Calculating Mean Squared Error using restored labels
MSE <- mean((restored_test_labels - restored_predictions)^2)
RMSE <- sqrt(mean((restored_test_labels - restored_predictions)^2))
RMSE_average<-sqrt(mean((restored_test_labels - mean(restored_test_labels))^2))
RMSE_subset <- sqrt(mean((results_dataf_subs$D_GT)^2))

plot(
  restored_test_labels, 
  restored_predictions, 
  xlab = "Observed (m)", 
  ylab = "Predicted (m)", 
  col = 'blue', 
  main = 'Real vs predicted', 
  pch = 18, 
  cex = 1.9,   
  cex.lab = 1.5,  
  cex.main = 1.5, 
  cex.axis = 1.9  
)
abline(0, 1, lwd = 2)



ggplot(results_dataf, aes(x = Date)) +
  geom_point(aes(y = GT, color = "Observed"), size = 2) +
  geom_point(aes(y = predicted, color = "Predicted"), shape = 17, size = 2) +
  labs(x = "Date", y = "Groundwater level (m)", color = "Curve") +
  scale_color_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  theme_minimal() +
  theme(
    text = element_text(size = 18)
  )


print("Root Mean Squared Error if average value:")
RMSE_average

print("Standard deviation")
sd(restored_test_labels)


print("Root Mean Squared Error:")
print(RMSE)

print("Pearson cor")
cor(restored_test_labels, restored_predictions)

print("Spearman cor")
cor(restored_test_labels, restored_predictions, method="spearman")




