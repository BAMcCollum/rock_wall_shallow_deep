dir_to_check <- c("models/abundance_models/",
                  "figures/assumptions_timeseries/",
                  "figures/fits_with_data_panels/",
                  "figures/prediction_arrows/",
                  "figures/raw_timeseries/",
                  "figures/raw_timeseries_panels/",
                  "figures/raw_timeseries_points/",
                  "figures/depth_dist",
                  "figures/decadal_depth_dist")
files_to_check <- list.files(dir_to_check, full.names = TRUE)

for(fn in files_to_check){
  #Check its existence
  if (file.exists(fn)) {
    #Delete file if it exists
    file.remove(fn)
  }
}
