# blm-pva-ohv
This repo contains code for downloading and processing the OHV route density layers and modeling results/producing figures for the OHV manuscript. Many scripts in this repo are reliant on products created in other scripts, so it is necessary to follow the suggested order of the scripts below to create these products.

1. Mosaic.R
  - This script downloads the raw OHV inference tiles from GCS and mosaics them into spatial layers. It also clipps decadal layers     using shapefiles manually created in QGIS to clip out issues where inference failed.
2. Processing.R
  - This script uses the raw OHV route density layers created in Mosaic.R and reclassifies them to create OHV layers. These are        used for the telemetry survival model, and are uploaded in GCS bucket data > 05 covariate outputs > OHV.
3. NLCD_masks.R
  - This script downloads NLCD data stored in GCS and creates a mask for the OHV layers representing water and developed areas.
4. Road_masks.R
  - This script downloads TIGER roads data stored in GCS and creates a mask for the OHV layers representing water and developed areas.
5. Stats_Func.R
  - This script uses the cleaned layers created in Processing.R and masks created in NLCD_masks.R and Road_masks.R to create           different cleaned and masked versions of the layers. It also uses functions created in Functions.R to mask these layers and run     different moving windows over these layers. These layers with the moving window function applied are used for the mark             recapture survival model and are uploaded in GCS bucket data > 06 covariate outputs > OHV_routes_roads into different sub          folders. 
6. Master_creation.R
  - This script uses original and cleaned layers created in Processing.R and Stats_Func.R to create a master csv with OHV route density categories for each cell through time. This file is used in modeling in GLM.R and for random sampling in Manuscript_figures.R.
7. All_masks.R
  - This uses layers created in Stats_Func.R to make mask layers that are used for creating QGIS maps.


After running these scripts in the suggested order, you should have outputs necessary to create summary statistics and files needed to create QGIS maps (Stats_Func.R and Change_Rasters.R), recreate figures used for the manuscript (Manuscript_figures.R), and run various analyses used in the manuscript (Routes_Analysis.R and GLM.R). 

Shapefiles used for creating figures in the OHV ms are located within the data folder in csp_tortoisehub GCS bucket.

All questions regarding this repo should be directed to Madeline Standen (madi@csp-inc.org).
All questions regarding the computer vision model used to create these OHV layers should be directed to Alex Robillard (alex@csp-inc.org).