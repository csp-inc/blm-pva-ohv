## ---------------------------
##
## Script name: Functions.R
##
## This script creates functions to apply to the OHV layers for use in demographic models for tortoise survival.
##
## Author: Madeline Standen
##
## Date Created: 02/__/2024
## Date last updated: 03/14/2025
##
## Email contact: madi[at]csp-inc.org
##
## ---------------------------
##
## Notes: 


## Summary stats functions -----

# This function summarizes the OHV density classes in the decadal layers
# Apply to a rasterstack
class_summary <- function(x){
  
  out_list <- list()
  decades <- c("1970s","1980s","2010s","2020s")
  for(i in 1:nlyr(x)){
    prop <- as.data.frame(table(values(x[[i]])))
    prop$sum <- sum(prop$Freq)
    prop$area <- prop$Freq*(150*150)
    prop$dec_area <- sum(prop$area)
    prop$Decade <- decades[i]
    out_list[[i]] <- prop
  }
  prop_years <- bind_rows(out_list)
  
  prop_years$Proportion <- prop_years$Freq/prop_years$sum
  
  dt_range <- st_read("./shapefiles/DTrange/dtrange_web.shp")
  prop_years$area_prop <- prop_years$area/as.numeric(st_area(dt_range))
  prop_years$dec_area_prop <- prop_years$area/prop_years$dec_area
  names(prop_years) <- c("Class","Freq","sum","area","dec_area","Decade","Proportion","Range_area_prop","dec_area_prop")
  prop_years$Class <- as.character(prop_years$Class)
  
  return(prop_years)
}

# This function combines values for OHV density classes medium and high
# Apply to a dataframe created by class_summary()
join2_4 <- function(x){

  decades <- c("1970s","1980s","2010s","2020s")
  joined_list <- list()
  for (i in 1:4){
    year <- filter(x, Decade == decades[i])
    high <- filter(year, Class == "2" | Class == "4")
    high_prop <- colSums(high[,c("Freq","Proportion","area","Range_area_prop","dec_area_prop")])
    high_prop_df <- t(as.data.frame(c("2",as.numeric(unname(high_prop[1])),high[1,3],as.numeric(unname(high_prop[3])),high[1,5],decades[i],as.numeric(unname(high_prop[2])),as.numeric(unname(high_prop[4])),as.numeric(unname(high_prop[5])))))
    high_prop_df <- as.data.frame(high_prop_df)
    colnames(high_prop_df) <- colnames(high)
    rownames(high_prop_df) <- 3
    high_prop_df$Proportion <- as.numeric(high_prop_df$Proportion)
    high_prop_df$Range_area_prop <- as.numeric(high_prop_df$Range_area_prop)
    joined_list[[i]] <- rbind(year[1:2,],high_prop_df)
  }
  prop_years_high <- bind_rows(joined_list)
  prop_years_high$Class <- as.character(prop_years_high$Class)
  
  return(prop_years_high)
}

# This function combines values for OHV density classes low, medium, and high
# Apply to a dataframe created by class_summary()
join1_4 <- function(x){

decades <- c("1970s","1980s","2010s","2020s")
joined_list <- list()
for (i in 1:4){
  year <- filter(x, Decade == decades[i])
  year <- year %>%
    mutate_at(c("Freq","Proportion","area","Range_area_prop","dec_area_prop"), as.numeric)
  high <- filter(year, Class == "1" | Class == "2" | Class == "4")
  high_prop <- colSums(high[,c("Freq","Proportion","area","Range_area_prop","dec_area_prop")])
  high_prop_df <- t(as.data.frame(c("2",as.numeric(unname(high_prop[1])),high[1,3],as.numeric(unname(high_prop[3])),high[1,5],decades[i],as.numeric(unname(high_prop[2])),as.numeric(unname(high_prop[4])),as.numeric(unname(high_prop[5])))))
  high_prop_df <- as.data.frame(high_prop_df)
  colnames(high_prop_df) <- colnames(high)
  rownames(high_prop_df) <- 1
  high_prop_df$Proportion <- as.numeric(high_prop_df$Proportion)
  high_prop_df$Range_area_prop <- as.numeric(high_prop_df$Range_area_prop)
  joined_list[[i]] <- rbind(year[1,],high_prop_df)
}
prop_years_binary <- bind_rows(joined_list)

return(prop_years_binary)
}

# This function randomly samples 1000 chips 1000 times and summarizes OHV density information
random_sampling <- function(x, small_ext = FALSE){
  if(small_ext){
    x <- x[complete.cases(x), ]
  }
  values_df_70 <- x[,c(2,6)]
  values_df_70 <- values_df_70[complete.cases(values_df_70$V1970), ]
  
  values_df_80 <- x[,c(3,6)]
  values_df_80 <- values_df_80[complete.cases(values_df_80$V1980), ]
 
  values_df_10 <- x[,c(4,6)]
  values_df_10 <- values_df_10[complete.cases(values_df_10$V2010), ]
  
  values_df_20 <- x[,c(5,6)]
  values_df_20 <- values_df_20[complete.cases(values_df_20$V2020), ]
  
  list <- list()
  list[[1]] <- values_df_70
  list[[2]] <- values_df_80
  list[[3]] <- values_df_10
  list[[4]] <- values_df_20
  
  output_mean <- list()
  output_sd <- list()

  set.seed(41)
  for (i in 1:4){
    df <- list[[i]]
    prop_df <- data.frame(matrix(nrow = 4, ncol = 1000))
    for (j in 1:1000){
      # Generate 1000 random row indices
      random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
      # Subset the dataset with the randomly selected indices
      subset_df <- df[random_indices, ]
      values <- as.data.frame(table(subset_df[,1]))
      values <- values[,2]/sum(values[,2])
      prop_df[,j] <- values
    }
    output_mean[[i]] <- rowMeans(prop_df)
    prop_mat <- as.matrix(prop_df)
    output_sd[[i]] <- apply(prop_mat, 1, sd)
  }
  
  final_prop <- as.data.frame(cbind(output_mean[[1]],
                                    output_mean[[2]],
                                    output_mean[[3]],
                                    output_mean[[4]]))
  
  final_sd <- as.data.frame(cbind(output_sd[[1]],
                                  output_sd[[2]],
                                  output_sd[[3]],
                                  output_sd[[4]]))
  
  
  final_prop$OHV_val <- c("0","1","2","4")
  
  final_prop <- final_prop[,c(5,1:4)]
  
  names(final_prop) <- c("OHV_val","V1970","V1980","V2010","V2020")
  
  final_sd$OHV_val <- c("0","1","2","4")
  
  final_sd <- final_sd[,c(5,1:4)]
  
  names(final_sd) <- c("OHV_val","sd1970","sd1980","sd2010","sd2020")
  
  mean_long <- final_prop %>% 
    pivot_longer(
      cols = c("V1970","V1980","V2010","V2020"), 
      names_to = "year",
      values_to = "mean"
    )
  
  sd_long <- final_sd %>% 
    pivot_longer(
      cols = c("sd1970","sd1980","sd2010","sd2020"), 
      names_to = "year",
      values_to = "sd"
    )
  
  final_random <- cbind(mean_long,sd_long[,3])
  return(final_random)
}


# This function randomly samples 1000 chips 1000 times and summarizes OHV density information, combining classes medium and high
random_sampling3 <- function(x, small_ext = FALSE){

        if(small_ext){
          x <- x[complete.cases(x), ]
        }
  
      values_df_70 <- x[,c(2,6)]
      values_df_70 <- values_df_70[complete.cases(values_df_70$V1970), ]
      
      values_df_80 <- x[,c(3,6)]
      values_df_80 <- values_df_80[complete.cases(values_df_80$V1980), ]
      
      values_df_10 <- x[,c(4,6)]
      values_df_10 <- values_df_10[complete.cases(values_df_10$V2010), ]
      
      values_df_20 <- x[,c(5,6)]
      values_df_20 <- values_df_20[complete.cases(values_df_20$V2020), ]
      
      list <- list()
      list[[1]] <- values_df_70
      list[[2]] <- values_df_80
      list[[3]] <- values_df_10
      list[[4]] <- values_df_20


      output_mean_merge <- list()
      output_sd_merge <- list()
      
      set.seed(41)
      for (i in 1:4){
        df <- list[[i]]
        prop_df <- data.frame(matrix(nrow = 3, ncol = 1000))
        for (j in 1:1000){
          # Generate 1000 random row indices
          random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
          # Subset the dataset with the randomly selected indices
          subset_df <- df[random_indices, ]
          values <- as.data.frame(table(subset_df[,1]))
          vals_high <- values %>% filter(Var1 == "2"|Var1 == "4")
          vals_high_sum <- as.data.frame(cbind("3",sum(vals_high$Freq)))
          names(vals_high_sum) <- names(values)
          vals_low <- values %>% filter(Var1 == "0"|Var1 == "1")
          values <- rbind(vals_low,vals_high_sum)
          values$Freq <- as.integer(values$Freq)
          values <- values[,2]/sum(values[,2])
          prop_df[,j] <- values
        }
        output_mean_merge[[i]] <- rowMeans(prop_df)
        prop_mat <- as.matrix(prop_df)
        output_sd_merge[[i]] <- apply(prop_mat, 1, sd)
      }
      
      output_mean_merge[[1]]
      output_mean_merge[[2]]
      output_mean_merge[[3]]
      output_mean_merge[[4]]
      
      final_prop_merge <- as.data.frame(cbind(output_mean_merge[[1]],
                                              output_mean_merge[[2]],
                                              output_mean_merge[[3]],
                                              output_mean_merge[[4]]))
      
      final_sd_merge <- as.data.frame(cbind(output_sd_merge[[1]],
                                            output_sd_merge[[2]],
                                            output_sd_merge[[3]],
                                            output_sd_merge[[4]]))
      
      
      final_prop_merge$OHV_val <- c("0","1","3")
      
      final_prop_merge <- final_prop_merge[,c(5,1:4)]
      
      names(final_prop_merge) <- c("OHV_val","V1970","V1980","V2010","V2020")
      
      
      final_sd_merge$OHV_val <- c("0","1","3")
      
      final_sd_merge <- final_sd_merge[,c(5,1:4)]
      
      names(final_sd_merge) <- c("OHV_val","sd1970","sd1980","sd2010","sd2020")
      
      mean_long_merge <- final_prop_merge %>% 
        pivot_longer(
          cols = c("V1970","V1980","V2010","V2020"), 
          names_to = "year",
          values_to = "mean"
        )
      
      sd_long_merge <- final_sd_merge %>% 
        pivot_longer(
          cols = c("sd1970","sd1980","sd2010","sd2020"), 
          names_to = "year",
          values_to = "sd"
        )
      
      final_random_merge <- cbind(mean_long_merge,sd_long_merge[,3])
      

      return(final_random_merge)

}


# This function randomly samples 1000 chips 1000 times and summarizes OHV density information
# It then determines the length in the estimate 
random_sampling_length <- function(x, small_ext = FALSE){
  # x <- values_sample
  if(small_ext){
    x <- x[complete.cases(x), ]
  }
  
  ncells <- nrow(x)
  
  values_df_70 <- x[,c(2,6)]
  values_df_70 <- values_df_70[complete.cases(values_df_70$V1970), ]
  
  values_df_80 <- x[,c(3,6)]
  values_df_80 <- values_df_80[complete.cases(values_df_80$V1980), ]
  
  values_df_10 <- x[,c(4,6)]
  values_df_10 <- values_df_10[complete.cases(values_df_10$V2010), ]
  
  values_df_20 <- x[,c(5,6)]
  values_df_20 <- values_df_20[complete.cases(values_df_20$V2020), ]
  
  list <- list()
  list[[1]] <- values_df_70
  list[[2]] <- values_df_80
  list[[3]] <- values_df_10
  list[[4]] <- values_df_20
  
  # Make lists for each decade
  output_mean <- vector(length = 4,mode = "list")
  output_sd <- vector(length = 4,mode = "list")
  # Make dataframe for each decade with a column for each scaling factor
  for(i in 1:4){
    output_mean[[i]] <- data.frame(matrix(ncol = 3, nrow = 1))
    output_sd[[i]] <- data.frame(matrix(ncol = 3, nrow = 1))
  }

  min_sc <- c(0,1,151,451)
  mean_sc <- c(0,75,300,11475)
  max_sc <- c(0,150,450,22500)
  
  scaling_df <- as.data.frame(cbind(min_sc,mean_sc,max_sc))
  
  set.seed(41)
  for (i in 1:4){ # for each decade
    df <- list[[i]]
    length_df <- data.frame(matrix(nrow = 4, ncol = 1000))
    # repeat sampling of 1000 cells 1000 times
    for (j in 1:1000){
      # Generate 1000 random row indices
      random_indices <- sample(1:nrow(df), 1000, replace = FALSE)
      # Subset the dataset with the randomly selected indices
      subset_df <- df[random_indices, ]
      values <- as.data.frame(table(subset_df[,1]))
      length_df[,j] <- values[,2] # add each sample values as a column
    }
    # multiply by the scaling factor and find the mean and standard deviation of samples
    for (k in 1:3){ # for each scaling factor
      length_df_scaled <- length_df
      # for each OHV class
      for(l in 1:4){ 
        length_df_scaled[l,] <- length_df[l,]*scaling_df[l,k]
      }
      # add all the lengths contributed by each class together
      lengths <- colSums(length_df_scaled)
      lengths <- as.numeric(lengths)
      # multiply 
      lengths_mdt <- (lengths*ncells)/1000
      output_mean[[i]][1,k] <- mean(lengths_mdt)
      output_sd[[i]][1,k] <- sd(lengths_mdt)
    }
  }
  
  lengths <- bind_rows(output_mean)
  lengths_sd <- bind_rows(output_sd)
  
  
  colnames(lengths) <- c("min","mean","max")
  rownames(lengths) <- c("V1970","V1980","V2010","V2020")
  colnames(lengths_sd) <- c("min","mean","max")
  rownames(lengths_sd) <- c("V1970","V1980","V2010","V2020")

  lengths_long <- lengths %>% 
    pivot_longer(
      cols = c("min","mean","max"), 
      names_to = "scale",
      values_to = "mean_length"
    )
  
  lengths_sd_long <- lengths_sd %>% 
    pivot_longer(
      cols = c("min","mean","max"), 
      names_to = "scale",
      values_to = "sd_length"
    )
  

  final_random <- cbind(lengths_long,lengths_sd_long[,2])
  final_random$decade <- c("V1970","V1970","V1970","V1980","V1980","V1980","V2010","V2010","V2010","V2020","V2020","V2020")
  
  return(final_random)
}




### Cleaning functions -----

# This function identifies cells with a value of low, med, or high and are surrounded
# by none and changes these values to none
gol_fun <- function(x) {
  
  #Get the center cell
  center <- x[5]
  
  # if the center is 0 or NA, dont run
  if (center==0 | is.na(center)) { 
    return(center)
  } else {
    
    # Find the sum of all the cells, minus value of center cell
    ncells <- sum(x, na.rm=TRUE)
    ncells  <- ncells - x[5]
    
    # If sum of all cells around center is 0, make center cell 1
    if (ncells==0) { 
      return(1)
    } # If sum of cells around center is greater than 1, make 0
    else { 
      return(0)
    }
  }
}

# This function applies the function gol_fun within a moving window
saltNpepper <- function(x) {
  foc_mat<-matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
  f <- focal(x, w=foc_mat, fun=gol_fun)
}


# This function applies the saltNpepper mask to clean the OHV layers
# Apply to a rasterstack
salt_clean <- function(x,writeR = FALSE){
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    salt_mask <- saltNpepper(raster)
    pepper_mask<- classify(salt_mask, cbind(0,NA), right=FALSE)
    raster_masked <- mask(raster,pepper_mask, inverse = TRUE, updatevalue = 0)
    names(raster_masked) <- paste0(names(raster_masked),"_masked_9")
    if(writeR){
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}


# This function masks each OHV density layer with the correct NLCD mask (water and developed low medium and high, NLCD)
# Apply to a rasterstack
nlcd_mask <- function(x, writeR = FALSE, update0 = FALSE, updateNA = TRUE, remask = TRUE){
  masks <- list.files("./other_data/masks/NLCD", recursive = TRUE, full.names = TRUE, pattern = "n21")
  
  if(remask){
  cropping_shps <- list()
  
  cropping_shps[[1]] <- st_read("./shapefiles/cropping/1970_netr_noc_all.shp")
  cropping_shps[[2]] <- st_read("./shapefiles/cropping/1980_netr_all.shp")
  cropping_shps[[3]] <- st_read("./shapefiles/cropping/2010_crop.shp")
  cropping_shps[[4]] <- st_read("./shapefiles/cropping/2020_crop.shp")
  }
  
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    mask <- rast(masks[i])
    if(updateNA){
      raster_masked <- mask(raster,mask, inverse = TRUE)
    }
    if(update0){
      raster_masked <- mask(raster,mask, inverse = TRUE, updatevalue = 0)
    }
    if(remask){
      raster_masked <- mask(raster_masked,cropping_shps[[i]])
    }
    original_name <- names(raster_masked) %>% str_replace("_masked_9", "")
    names(raster_masked) <- paste0(names(raster_masked),"_nlcdmask")
    if(writeR){
      names(raster_masked) <- paste0(original_name,"_cleaned")
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}

# This function masks each OHV density layer with the correct TIGER roads mask (S1100, S1200 and S1400)
# Apply to a rasterstack
roads_mask <- function(x, writeR = FALSE, update0 = FALSE, updateNA = TRUE, remask = TRUE){
  masks <- list.files("./other_data/masks/TIGER", recursive = TRUE, full.names = TRUE)
  masks <- c(masks[1],masks) # Need to repeat the 2000 roads mask for the 1970s
  
  if(remask){
    cropping_shps <- list()
    
    cropping_shps[[1]] <- st_read("./shapefiles/cropping/1970_netr_noc_all.shp")
    cropping_shps[[2]] <- st_read("./shapefiles/cropping/1980_netr_all.shp")
    cropping_shps[[3]] <- st_read("./shapefiles/cropping/2010_crop.shp")
    cropping_shps[[4]] <- st_read("./shapefiles/cropping/2020_crop.shp")
  }
  
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    mask <- rast(masks[i])
    if(updateNA){
      raster_masked <- mask(raster,mask, inverse = TRUE)
    }
    if(update0){
      raster_masked <- mask(raster,mask, inverse = TRUE, updatevalue = 0)
    }
    if(remask){
      raster_masked <- mask(raster_masked,cropping_shps[[i]])
    }
    original_name <- names(raster_masked) %>% str_replace("_masked_9", "") %>% str_replace("_nlcdmask", "") %>% str_replace("_cleaned", "")
    names(raster_masked) <- paste0(names(raster_masked),"_roadsmask")
    if(writeR){
      if(isTRUE(grepl("_nlcdmask|cleaned",names(raster_masked))) == TRUE){ # If it has already been masked for nlcd, then this is cleaned 3
        names(raster_masked) <- paste0(original_name,"_cleaned3")
      } else { # If its just being masked for roads, then its cleaned 2
        names(raster_masked) <- paste0(original_name,"_cleaned2")
      }
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}

# This function masks out cells for which there is not an OHV density estimate in each decade
small_ex_mask <- function(x, writeR = FALSE, update0 = FALSE, updateNA = TRUE){
  mask <- rast("./other_data/masks/small_ext.tif")
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    if(updateNA){
      raster_masked <- mask(raster,mask, inverse = TRUE)
    }
    if(update0){
      raster_masked <- mask(raster,mask, inverse = TRUE, updatevalue = 0)
    }
    names(raster_masked) <- paste0(names(raster_masked),"_small_ext")
    if(writeR){
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}



### Moving window functions -----

sum_window <- function(x, radius = 400, writeR = FALSE){
  for(i in 1:nlyr(x)){
    raster <- x[[i]]
    focal_num<-radius
    focal_size<-radius*2
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focsum_",focal_size)
    if(writeR){
      writeRaster(raster_foc, file = paste0("./output_layers/",names(raster_foc),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_foc
  }
return(x)
}


mode_window <- function(x, radius = 400, writeR = FALSE){
  for(i in 1:nlyr(x)){
    raster <- x[[i]]
    focal_num<-radius
    focal_size<-radius*2
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "modal", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focmode_",focal_size)
    if(writeR){
      writeRaster(raster_foc, file = paste0("./output_layers/",names(raster_foc),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_foc
  }
  return(x)
}

max_window <- function(x, radius = 400, writeR = FALSE){
  for(i in 1:nlyr(x)){
    raster <- x[[i]]
    focal_num<-radius
    focal_size<-radius*2
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "max", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focmax_",focal_size)
    if(writeR){
      writeRaster(raster_foc, file = paste0("./output_layers/",names(raster_foc),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_foc
  }
  return(x)
}

mean_window <- function(x, radius = 400, writeR = FALSE){
  for(i in 1:nlyr(x)){
    raster <- x[[i]]
    focal_num<-radius
    focal_size<-radius*2
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "mean", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focmean_",focal_size)
    if(writeR){
      writeRaster(raster_foc, file = paste0("./output_layers/",names(raster_foc),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_foc
  }
  return(x)
}