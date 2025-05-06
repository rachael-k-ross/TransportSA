# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Cleaning of already processed XBOT data (CTN 51) 
#               from a variety of sources
# 
# ------------------------------------------------------------------------------ #

# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(haven)
library(gtsummary)
library(readxl)
library(stringr)
library(data.table)
library(hrbrthemes)
theme_gtsummary_compact()

# Paths
home <- Sys.getenv("HOME")
datpath <- "/07 Postdoc/02 Projects/00 Data/01 data/"
outpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/"
study <- "ctn0051/"

# Data

## From Ned's paper
neds <- read_xlsx(paste0(home,datpath,study,"src/CTN0051_Computed_Vars_ModPaper.xlsx"), sheet = "Data") |>
  mutate(PATID = str_pad(PATID, 13, pad = "0"))

# Harmonized baseline data from 97 (51 only)
clean_baseline_harmonized <- readRDS(paste0(home,
                                            datpath,
                                            "amy_harmonized/clean_baseline_harmonized.rds")) |>
  filter(project == 51) 

## NR-NTX injections
inj <- read_sas(paste0(home,datpath,study,"src/inj.sas7bdat")) |> arrange(PATID,INJINJDT) |> 
  filter(!is.na(INJINJDT)) #there is one row without a date

## Bup dispensing
d51 <- read_sas(paste0(home,datpath,study,"src/d51.sas7bdat")) |> arrange(PATID,D51DSEDT) |> 
  filter(!is.na(D51DSEDT)) |> #there are 2 rows without a date
  mutate(anydispensed = max(D51BUPDD), .by=PATID)

# ## End of Med data
# eom <- read_sas(paste0(home,datpath,study,"src/eom.sas7bdat")) |>
#   filter(!is.na(EODRUGDT))

## Primary outcome file 
primout <- read_sas(paste0(home,datpath,study,"src/primout_FINAL.sas7bdat")) 


# Covar, a and y data ----------------------------------------------------------

names(neds)

summary(neds$AGE)
#test <- neds |> filter(AGE>64) 2 people

covar <- neds |>
  mutate(.age_factor = factor(case_when(AGE<21 ~ 3,
                                        21<=AGE & AGE<=24 ~ 4,
                                        25<=AGE & AGE<=29 ~ 5,
                                        30<=AGE & AGE<=34 ~ 6,
                                        35<=AGE & AGE<=39 ~ 7,
                                        40<=AGE & AGE<=44 ~ 8,
                                        45<=AGE & AGE<=49 ~ 9,
                                        50<=AGE & AGE<=54 ~ 10,
                                        55<=AGE & AGE<=64 ~ 11,
                                        65<=AGE ~ 12,
                                        .default=NA)),
         .age_num = AGE,
         .male = case_when(GENDER=="Male" ~ 1,
                           GENDER=="Female" ~ 0,
                           .default=NA),
         .race_num = case_when(race_new == "White Only" ~ 2,
                               race_new == "Black Only" ~ 1,
                               race_new == "Other" ~ 3,
                               .default=NA),
         .race_factor = factor(.race_num),
         .hispanic = case_when(hispanic=="Hispanic" ~ 1,
                               hispanic=="Not Hispanic" ~ 0,
                               .default = NA),
         .education_num = case_when(c_edu=="<HS" ~ 1,
                                    c_edu=="HS/GED" ~ 2,
                                    c_edu==">HS" ~ 3,
                                    .default=NA),
         .education_factor = factor(.education_num),
         .unemployed = case_when(c_employment=="Not Employed" ~ 1,
                                 c_employment=="Other" ~ 0,
                                 .default=NA),
         .homeless = case_when(homeless=="Yes" ~ 1,
                               homeless=="No" ~ 0,
                               .default=NA),
         .ivdrug = case_when(IV_user=="Yes" ~ 1,
                             IV_user=="No" ~ 0,
                             .default=NA),
         .use_cokecrack = case_when(Coc_ASI_TLFB=="Yes" ~ 1,
                                    Coc_ASI_TLFB=="No" ~ 0,
                                    .default=NA),
         .use_cannabis = case_when(Can_ASI_TLFB=="Yes" ~ 1,
                                    Can_ASI_TLFB=="No" ~ 0,
                                    .default=NA),
         .use_stim = case_when(Amp_ASI_TLFB=="Yes" ~ 1,
                                    Amp_ASI_TLFB=="No" ~ 0,
                                    .default=NA),
         .use_sed = case_when(Sed_ASI_TLFB=="Yes" ~ 1,
                                    Sed_ASI_TLFB=="No" ~ 0,
                                    .default=NA),
         .firstopuse_num = case_when(AgeOnset_any <=14 ~ 1,
                                     15<=AgeOnset_any & AgeOnset_any<=20 ~ 2,
                                     21<=AgeOnset_any & AgeOnset_any<=29 ~ 3,
                                     30<=AgeOnset_any ~ 4,
                                     .default=NA),
         .firstopuse_factor = factor(.firstopuse_num),
         y = case_when(relapse=="Yes" ~ 1, # This matched primary paper numbers
                       relapse=="No" ~ 0,
                       .default=NA),
         a = case_when(TRT=="XR-NTX" ~ 1,
                       TRT=="BUP-NX" ~ 0,
                       .default=NA),
         s=1) |>
  select(PATID,starts_with("."),TRT,a,y,s) |>
  mutate(anyNA = +if_any(contains("."), is.na),
         sumNA = +rowSums(is.na(across(contains(".")))))

#table(covar$sumNA)
#test <- covar |> filter(anyNA !=0) #Ages >64, 2 people
         
    #  # QC
    # tapply(covar$.age_num, covar$.age_factor, summary)     
    # table(covar$.male,covar$GENDER,useNA = "ifany")
    # table(covar$.race_num,covar$race_new,useNA = "ifany")
    # table(covar$.hispanic,covar$hispanic,useNA = "ifany")
    # table(covar$.education_num,covar$c_edu,useNA = "ifany")
    # table(covar$.homeless,covar$homeless,useNA = "ifany")
    # table(covar$.ivdrug,covar$IV_user,useNA = "ifany")
    # table(covar$.use_cokecrack,covar$Coc_ASI_TLFB,useNA = "ifany")
    # table(covar$.use_cannabis,covar$Can_ASI_TLFB,useNA = "ifany")
    # table(covar$.use_stim,covar$Amp_ASI_TLFB,useNA = "ifany")
    # table(covar$.use_sed,covar$Sed_ASI_TLFB,useNA = "ifany")
    # tapply(covar$AgeOnset_any, covar$.firstopuse_num, summary)          
         
       
# Induction --------------------------------------------------------------------

inductionwindow <- 21 # could do 28

## Data checks ----

length(unique(d51$PATID)) #281/287 randomized - primary paper 270 inducted
length(unique(inj$PATID)) #204/283 randomized - primary paper 204 inducted
length(unique(covar$PATID))

length(unique(d51$PATID[d51$anydispensed==1])) # 270 had bup dispensed at any time

## XR-NTX induction ----

names(inj)

ntxinducted <- inj |>
    group_by(PATID) |>
    summarise(induction_dt = min(INJINJDT)) |>
    ungroup() |>
  left_join(clean_baseline_harmonized |> select(who,rand_dt), join_by(PATID==who)) |>
  mutate(inductionday = as.numeric(induction_dt-rand_dt),
         inducted = case_when(inductionday <= inductionwindow ~ 1,
                              .default=0)) |>
  select(PATID,inducted)

### NX-BUP ----

bupindcuted <- d51 |> 
  filter(anydispensed==1) |>
  group_by(PATID) |>
  summarise(induction_dt = min(D51DSEDT)) |>
  ungroup() |>
  left_join(clean_baseline_harmonized |> select(who,rand_dt), join_by(PATID==who)) |>
  mutate(inductionday = as.numeric(induction_dt-rand_dt),
         inducted = case_when(inductionday <= inductionwindow ~ 1,
                              .default=0)) |>
  select(PATID,inducted)

# Combine -----------------------------------------------------------

XBOTclean <- covar |>
  left_join(rbind(ntxinducted,bupindcuted), join_by("PATID")) |>
  mutate(z = case_when(inducted==1 ~ 1, .default = 0)) |>
  select(-"inducted") |>
  rename(id=PATID)

#sort(names(XBOTclean))
saveRDS(XBOTclean, paste0(home,outpath,"XBOTclean.rds")) 
rm(list=ls())
