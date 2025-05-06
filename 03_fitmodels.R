
# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Fitting nuisance models
# 
# ------------------------------------------------------------------------------ #

# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(haven)
library(gtsummary)
library(gt)
library(tableone)
library(readxl)
library(stringr)
library(data.table)
library(hrbrthemes)
theme_gtsummary_compact()
library(mlr3superlearner)
library(trapezoid)
library(ggplot2)
library(patchwork)
library(tictoc)
library(future)
library(furrr)
library(progressr)
library(fastDummies)
library(mlr3extralearners)
plan(multicore)
furrr_options(seed=NULL)

# Paths
# home <- Sys.getenv("HOME")
# projpath <- paste0(home,"/07 Postdoc/02 Projects/13 Sensitivity analysis/")
# datpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/"
# code <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Code/forgit/"

#projpath <- "/home/rr3551/sensanalysis/"

# Data
combined <- readRDS(paste0(projpath,"Data/combined.rds")) |>
  dummy_cols(select_columns = c(".age_factor",".race_factor",".education_factor",".firstopuse_factor"),
             remove_most_frequent_dummy = TRUE) |>
  rename(".education_factor_HSplus"=".education_factor_Greater than high school",
         ".education_factor_lessHS"=".education_factor_Less than high school",
         ".firstopuse_factor_less15"=".firstopuse_factor_<15",
         ".firstopuse_factor_2029"=".firstopuse_factor_20-29",
         ".firstopuse_factor_30plus"=".firstopuse_factor_30+")

# Call in functions for analysis
source(paste0(home,code,"00_analysisfxs.R"))

# Superlearner library
lib_bal <- c("mean","glm","xgboost","earth")
lib_unbal <- list(list("xgboost", max_delta_step = 1), 
                  list("xgboost", max_delta_step = 2), 
                  list("xgboost", max_delta_step = 5), 
                  "mean", "earth", "glm")

# Covariates -------------------------------------------------------------------

# Variables
covars <- c(".age_num",
            ".male",
            ".race_factor_Black",
            ".race_factor_Other",
            ".hispanic",
            ".education_factor_HSplus",
            ".education_factor_lessHS",
            ".unemployed",
            ".homeless",
            ".ivdrug",
            ".use_cokecrack",
            ".use_cannabis",
            ".use_stim",
            ".use_sed",
            ".firstopuse_factor_less15",
            ".firstopuse_factor_2029",
            ".firstopuse_factor_30plus")

outvar_old <- adhvar <- trtvar <- selvar <- covars
outvar_new <- c(covars,"z")


# Run models fx ----------------------------------------------------------------

tic()
datsplits <- createss(combined,123,30)
set.seed(7)
preds <- getpredictions(datsplits,
                        strat_a=TRUE,strat_z=FALSE,
                        ml=TRUE,
                        outvar_new,outvar_old,adhvar,trtvar,selvar)
nrow(preds)
toc()

# Save -------------------------------------------------------------------------
saveRDS(preds,paste0(home,datpath,"preds.rds"))
nrow(preds)
