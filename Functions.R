## Summary stats functions


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


random_sampling <- function(x, small_ext = FALSE){
  if(small_ext){
    x <- x[complete.cases(x), ]
  }
  values_df_70 <- x[,c(2,6)]
  values_df_70 <- values_df_70[complete.cases(values_df_70$V70), ]
  
  values_df_80 <- x[,c(3,6)]
  values_df_80 <- values_df_80[complete.cases(values_df_80$V80), ]
 
  values_df_10 <- x[,c(4,6)]
  values_df_10 <- values_df_10[complete.cases(values_df_10$V10), ]
  
  values_df_20 <- x[,c(5,6)]
  values_df_20 <- values_df_20[complete.cases(values_df_20$V20), ]
  
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


random_sampling3 <- function(x, small_ext = FALSE){

        if(small_ext){
          x <- x[complete.cases(x), ]
        }
  
      values_df_70 <- x[,c(2,6)]
      values_df_70 <- values_df_70[complete.cases(values_df_70$V70), ]
      
      values_df_80 <- x[,c(3,6)]
      values_df_80 <- values_df_80[complete.cases(values_df_80$V80), ]
      
      values_df_10 <- x[,c(4,6)]
      values_df_10 <- values_df_10[complete.cases(values_df_10$V10), ]
      
      values_df_20 <- x[,c(5,6)]
      values_df_20 <- values_df_20[complete.cases(values_df_20$V20), ]
      
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





## Cleaning functions

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

# Create the function saltNpepper
saltNpepper <- function(x) {
  foc_mat<-matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)
  f <- focal(x, w=foc_mat, fun=gol_fun)
}


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



# Apply to a rasterstack

nlcd_mask <- function(x, writeR = FALSE){
  masks <- list.files("./other_data/masks/NLCD", recursive = TRUE, full.names = TRUE, pattern = "n21")
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    mask <- rast(masks[i])
    raster_masked <- mask(raster,mask, inverse = TRUE)
    names(raster_masked) <- paste0(names(raster_masked),"_nlcdmask")
    if(writeR){
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}

roads_mask <- function(x, writeR = FALSE){
  masks <- list.files("./other_data/masks/TIGER", recursive = TRUE, full.names = TRUE)
  masks <- c(masks[1],masks) # Need to repeat the 1992 roads mask for the 1970s
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    mask <- rast(masks[i])
    raster_masked <- mask(raster,mask, inverse = TRUE)
    names(raster_masked) <- paste0(names(raster_masked),"_roadsmask")
    if(writeR){
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}


small_ex_mask <- function(x, writeR = FALSE){
  mask <- rast("./other_data/masks/small_ext.tif")
  for (i in 1:nlyr(x)){
    raster <- x[[i]]
    raster_masked <- mask(raster,mask)
    names(raster_masked) <- paste0(names(raster_masked),"_small_ext")
    if(writeR){
      writeRaster(raster_masked, file = paste0("./output_layers/",names(raster_masked),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_masked
  }
  return(x)
}







# Moving window function

sum_window <- function(x, radius = 400, writeR = FALSE){
  for(i in 1:nlyr(x)){
    raster <- x[[i]]
    focal_num<-radius
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "sum", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focsum_",radius)
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
    focal_shape<-'circle'
    foc_mat<-focalMat(raster, focal_num, focal_shape, fillNA= TRUE) #matrix for use in focal function. change number based on what radius of circle should be
    foc_mat[foc_mat>0] <- 1
    raster_foc <-focal(raster, foc_mat, fun = "modal", na.policy = "all", na.rm = TRUE)
    names(raster_foc) <- paste0(names(raster),"_focmode_",radius)
    if(writeR){
      writeRaster(raster_foc, file = paste0("./output_layers/",names(raster_foc),".tif"),overwrite = TRUE)
    }
    x[[i]] <- raster_foc
  }
  return(x)
}

