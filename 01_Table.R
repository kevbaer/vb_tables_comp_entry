# Mailroom ----------------------------------------------------------------
library(tidyverse)
library(gt)
library(ggdist)

# Source other files ------------------------------------------------------
source("99_data.R")
source("02_make_gt.R")
source("03_plot_distro.R")

# Make table! -------------------------------------------------------------
gt_group(.list = make_gt(dat))
# since make_gt returns a list, we do it like this and group the different positions together

# To save locally, uncomment next line!
# gtsave(data = gt_group(.list = make_gt(dat)), filename = "output.html")
