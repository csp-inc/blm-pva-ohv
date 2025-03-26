# blm-pva-ohv
This repo contains code for downloading and processing the OHV route density layers, analyses and modeling, and producing figures for the associated manuscript (Robillard et al., 2025). Many scripts in this repo are reliant on products created in other scripts, so it is necessary to follow the suggested order of the scripts below to create these products. The code in this repository is being shared 'as is' and without guarantee of functionality or support.

1. 01_Mosaic.R
  - This script downloads the raw OHV inference tiles from GCS and merges them into spatial layers. It also clips decadal layers using shapefiles manually created in QGIS to clip out issues where inference failed.
2. 02_Processing.R
  - This script uses the raw OHV route density layers created in 01_Mosaic.R and reclassifies them to create OHV layers that were used in analyses in collaboration with the BLM and DoD.
3. 03_Road_masks.R
  - This script downloads TIGER roads data stored in GCS and creates a mask for the OHV layers representing water and developed areas.
4. 04_NLCD_masks.R
  - This script downloads NLCD data stored in GCS and creates a mask for the OHV layers representing water and developed areas.
5. 05_Stats_Func.R
  - This script uses the cleaned layers created in 02_Processing.R and masks created in 04_NLCD_masks.R and 05_Road_masks.R to create different cleaned and masked versions of the layers. It also uses functions created in Functions.R to mask these        layers and run different moving windows over these layers. These layers with the moving window function applied are used for the other modeling efforts.
6. 06_Master_creation.R
  - This script uses original and cleaned layers created in 02_Processing.R and 05_Stats_Func.R to create a master csv with OHV route density categories for each cell through time. This file is used for trend analysis in modeling in 09_GLM.R.
7. 07_All_masks.R
  - This script makes mask layers that are used for creating QGIS maps.
8. 08_Routes_analysis.R
  - This script uses the WEMO routes layer to validate the OHV 2010 output.
9. 09_GLM.R
  - This runs the GLM to test the effect of year on probability of a cell containing OHV.
10. 10_Muscript_figures.R
  - This script creates figures and stats presented in the OHV manuscript.

Any additional scripts included in this repo are for documentation of work, and are not necessary to run.
After running these scripts in the suggested order, you should have outputs necessary to create summary statistics and files needed to create QGIS maps, recreate figures used for the manuscript (10_Manuscript_figures.R), and run various analyses used in the manuscript (08_Routes_Analysis.R and 09_GLM.R). 


All questions regarding this repo should be directed to Brett Dickson (brett@csp-inc.org).
