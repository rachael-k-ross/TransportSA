# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Cleaning TEDS-A for sensitivity analysis project
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
datpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/TEDS/Data/"
outpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/"


# Load Rdata files
load(paste0(home,datpath,"tedsa_puf_2014_R.rdata"))
load(paste0(home,datpath,"tedsa_puf_2015_R.rdata"))
load(paste0(home,datpath,"tedsa_puf_2016_R.rdata"))
#names(TEDSA_PUF_2014)


# Select cohort ----------------------------------------------------------------

selectcohort <- function(data){
  data |>
    
    # Inclusion
    filter(2<AGE & AGE<12) |>
    filter(SERVICES %in% c(1,2)) |>
    filter(SUB1 %in% c(5,6,7) | SUB2 %in% c(5,6,7) | SUB3 %in% c(5,6,7)) |>
    #filter(HERFLG==1 | METHFLG==1 | OPSYNFLG==1 | DSMCRIT %in% c(5,12)) |>
    
    # Exclusion
    filter(GENDER==1 | (GENDER==2 & PREG!=1)) |>
    filter(PSOURCE != 7) |>
    filter(!(DSMCRIT %in% c(1,3,4,6,7,9,10,13,14,15,16,17,18,19)))
}

cohorts <- list(selectcohort(TEDSA_PUF_2014),
                selectcohort(TEDSA_PUF_2015),
                selectcohort(TEDSA_PUF_2016))

#map(cohorts,nrow)

# Creating variables -----------------------------------------------------------

cleandata <- function(data){
  
  # First clean singleton variables
  clean1 <- data |>
    mutate(.age_factor = factor(AGE),
           .age_num = case_when(AGE==3 ~ mean(c(18,20)),
                                  AGE==4 ~ mean(c(21,24)),
                                  AGE==5 ~ mean(c(25,29)),
                                  AGE==6 ~ mean(c(30,34)),
                                  AGE==7 ~ mean(c(35,39)),
                                  AGE==8 ~ mean(c(40,44)),
                                  AGE==9 ~ mean(c(45,49)),
                                  AGE==10 ~ mean(c(40,54)),
                                  AGE==11 ~ mean(c(55,64))),
           .male = case_when(GENDER==1 ~ 1,
                            GENDER==2 ~ 0,
                            .default = NA),
           .race_num = case_when(RACE==4 ~ 1,
                            RACE==5 ~ 2,
                            RACE %in% c(1,2,3,9,6,8,7) ~ 3,
                            .default = NA),
           .race_factor = factor(.race_num),
           .hispanic = case_when(ETHNIC %in% c(1,2,3,5) ~ 1,
                                ETHNIC==4 ~ 0,
                                .default = NA),
           # .married = case_when(MARSTAT==1 ~ 0,
           #                     MARSTAT %in% c(2,3,4) ~ 1,
           #                     .default = NA),
           .education_num = case_when(EDUC %in% c(1,2) ~ 1,
                                 EDUC %in% c(3) ~ 2,
                                 EDUC %in% c(4,5) ~ 3,
                                 .default = NA),
           .education_factor = factor(.education_num),
           .unemployed = case_when(EMPLOY %in% c(1,2) ~ 0,
                                EMPLOY %in% c(3,4) ~ 1,
                                .default = NA),
           .homeless = case_when(LIVARAG == 1 ~ 1,
                                LIVARAG %in% c(2,3) ~ 0,
                                .default = NA),
           # .firsttrt = case_when(NOPRIOR==0 ~ 1,
           #                      NOPRIOR %in% c(1,2,3,4,5) ~ 0,
           #                      .default = NA),
           .ivdrug = case_when(IDU==1 ~ 1,
                              IDU==0 ~ 0,
                              .default = NA),
           .use_cokecrack = case_when(COKEFLG == 1 ~ 1,
                                     .default = 0),
           .use_cannabis = case_when(MARFLG == 1 ~ 1,
                                    .default = 0),
           .use_stim = case_when(MTHAMFLG == 1 ~ 1,
                                AMPHFLG == 1 ~ 1,
                                STIMFLG == 1 ~ 1,
                                .default = 0),
           .use_hall = case_when(PCPFLG == 1 ~ 1,
                                HALLFLG == 1 ~ 1,
                                .default = 0),
           .use_sed = case_when(BENZFLG == 1 ~ 1,
                               TRNQFLG == 1 ~ 1,
                               BARBFLG == 1 ~ 1,
                               SEDHPFLG == 1 ~ 1,
                               .default = 0),
           year = factor(ADMYR))

  # Clean primary,secondary,tertiary substance variables
  clean2 <- data |> dplyr::select("CASEID",
                                  starts_with("SUB"),
                                  starts_with("FREQ"),
                                  starts_with("FRSTUSE"),
                                  -"FREQ_ATND_SELF_HELP") %>%
    pivot_longer(cols = -CASEID, names_to = c('.value', 'num'),
                 names_sep ="(?<=[A-Za-z])(?=[0-9])") |>
    filter(SUB %in% c(5,6,7)) |>
    mutate(dailyopuse = case_when(FREQ==3 ~ 1,
                                  FREQ %in% c(1,2) ~ 0,
                                  .default = NA),
           firstopuse = case_when(FRSTUSE %in% c(1,2) ~ 1,
                                  FRSTUSE %in% c(3,4) ~ 2,
                                  FRSTUSE %in% c(5,6) ~ 3,
                                  FRSTUSE %in% c(7) ~ 4,
                                  .default = NA)) |>
    group_by(CASEID) |>
    summarise(.dailyopuse = if(all(is.na(dailyopuse))) NA else max(dailyopuse, na.rm=TRUE),
              .firstopuse_num = if(all(is.na(firstopuse))) NA else min(firstopuse, na.rm=TRUE)) |>
    ungroup() |>
    mutate(.firstopuse_factor=factor(.firstopuse_num))
  
  merge(clean1,clean2,by=c("CASEID")) |>
    mutate(id = paste0(CASEID,year)) |>
    select(id,STFIPS,year,starts_with(".")) |>
    mutate(anyNA = +if_any(contains("."), is.na),
           sumNA = +rowSums(is.na(across(contains("."))))) |>
    mutate(s=0,a=0,z=0,y=0,TRT="") 
} 

cohorts_clean <- map(cohorts, cleandata)
# table(cohorts_clean[[1]]$sumNA)
# str(cohorts_clean[[1]])
# names(cohorts_clean[[1]])
# map(cohorts_clean, ~table(.x$anyNA))
# apply(cohorts_clean[[1]] |> select(-"CASEID"),MARGIN = 2,FUN = table,useNA="ifany")

  # QC
  # cohort14 <- cohorts_clean[[1]]
  # str(cohort14)
  # table(cohort14$age_factor,cohort14$AGE,useNA = "ifany")
  # table(cohort14$age_numave,cohort14$AGE,useNA = "ifany")
  # table(cohort14$male,cohort14$GENDER,useNA = "ifany")
  # table(cohort14$race_num,cohort14$RACE,useNA = "ifany")
  # table(cohort14$hispanic,cohort14$ETHNIC,useNA = "ifany")
  # table(cohort14$married,cohort14$MARSTAT,useNA = "ifany") # a lot of missingness
  # table(cohort14$education_num,cohort14$EDUC,useNA = "ifany")
  # table(cohort14$employed,cohort14$EMPLOY,useNA = "ifany")
  # table(cohort14$homeless,cohort14$LIVARAG,useNA = "ifany")
  # table(cohort14$firsttrt,cohort14$NOPRIOR,useNA = "ifany") # a lot of missingness
  # table(cohort14$ivdrug,cohort14$IDU,useNA = "ifany")
  # table(cohort14$use_cokecrack,cohort14$COKEFLG,useNA = "ifany")
  # table(cohort14$use_cannabis,cohort14$MARFLG,useNA = "ifany")
  # table(cohort14$use_hall,cohort14 |>
  #         mutate(any = PCPFLG + HALLFLG) |> pull(any),useNA = "ifany")
  # table(cohort14$use_stim,cohort14 |>
  #         mutate(any = MTHAMFLG + AMPHFLG + STIMFLG) |> pull(any),useNA = "ifany")
  # table(cohort14$use_sed,cohort14 |>
  #         mutate(any = BENZFLG + TRNQFLG + BARBFLG + SEDHPFLG) |> pull(any),useNA = "ifany")
  # 
  # test <- cohorts[[1]] |> dplyr::select("CASEID",
  #                                       starts_with("SUB"),
  #                                       starts_with("FREQ"),
  #                                       starts_with("FRSTUSE"),
  #                                       -"FREQ_ATND_SELF_HELP") %>%
  #   pivot_longer(cols = -CASEID, names_to = c('.value', 'num'), 
  #                names_sep ="(?<=[A-Za-z])(?=[0-9])") |>
  #   filter(SUB %in% c(5,6,7)) |>
  #   mutate(dailyopuse = case_when(FREQ==3 ~ 1,
  #                                 FREQ %in% c(1,2) ~ 0,
  #                                 .default = NA),
  #          firstopuse = case_when(FRSTUSE %in% c(1,2) ~ 1,
  #                                 FRSTUSE %in% c(3,4) ~ 2,
  #                                 FRSTUSE %in% c(5,6) ~ 3,
  #                                 FRSTUSE %in% c(7) ~ 4,
  #                                 .default = NA))
  # 
  # 
  #   table(test$dailyopuse,test$FREQ,useNA="ifany")
  #   table(test$firstopuse,test$FRSTUSE,useNA="ifany")
    

TEDSclean <- reduce(cohorts_clean, rbind) |>
  select(-".dailyopuse",-".use_hall") 

#sort(names(TEDSclean))
saveRDS(TEDSclean, paste0(home,outpath,"TEDSclean.rds"))
rm(list=ls())



        