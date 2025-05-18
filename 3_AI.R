library(reticulate)
library(tensorflow)
library(keras3)
library(withr)
library(ggplot2)
library(abind)

# we need only the following elements: main_list_subset and metadata_subset

main_dimentions<-512
images <- array(unlist(main_list_subset), dim = c(main_dimentions,main_dimentions, 1, length(main_list_subset)))
dim(images)
images <- aperm(images, c(4, 1, 2, 3))
dim(images)

#first: split
index <- sample(1:dim(images)[1], round(0.8 * dim(images)[1]))
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

# test the image representation
# plot(as.cimg(train_images[2, ,,]))

TRUE%in%is.na(images) #NA check

labels<-metadata_subset[,5]
# Splitting the data into train and test sets 80/20
train_images <- array_reshape(train_images, c(nrow(train_images), main_dimentions, main_dimentions, 1))
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

minivgg_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3)) %>%
  layer_activation_relu() %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(filters = 64, kernel_size = c(3, 3)) %>%
  layer_activation_relu() %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(filters = 128, kernel_size = c(3, 3)) %>%
  layer_activation_relu() %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_conv_2d(filters = 256, kernel_size = c(3, 3)) %>%
  layer_activation_relu() %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  layer_flatten() %>%
  
  layer_dense(units = 256) %>%
  layer_activation_relu() %>%
  
  layer_dense(units = 128) %>%
  layer_activation_relu() %>%
  
  layer_dense(units = 64) %>%
  layer_activation_relu() %>%
  
  layer_dense(units = 8) %>%
  layer_activation_relu() %>%
  
  layer_dense(units = 1, activation = 'linear')



minivgg_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(),
  metrics = c('mse')
)


history <- minivgg_model %>% fit(
  x = train_images,
  y = train_labels,
  epochs = 750,
  batch_size = 64,
  validation_split = 0
)


#minivgg_model and index are the only ones needed


