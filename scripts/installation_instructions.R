## Step 0: install and load packages

packages <- c("tidyverse", "here", "gridExtra", "caret", "rsample", "recipes", "grid", "rstudioapi", "MASS", "actuar", "statmod", "ReIns", "pscl", "zoo", "igraph", "expm", "DMwR", "pROC", "ChainLadder", "lubridate")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

if(sum(!(packages %in% installed.packages()[, "Package"]))) {
  stop(paste('The following required packages are not installed:\n', 
             paste(packages[which(!(packages %in% installed.packages()[, "Package"]))], collapse = ', ')));
} else {
  message("Everything is set up correctly. You are ready to go.")
}

