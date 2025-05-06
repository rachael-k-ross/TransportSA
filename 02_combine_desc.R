
# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Combine data sets and create tables/descriptive stats
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

# Run cleaning code
home <- Sys.getenv("HOME")
code <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Code/forgit/"
source(paste0(home,code,"01a_cleanTEDSA.R"))
source(paste0(home,code,"01b_cleanXBOT.R"))

# Set paths
home <- Sys.getenv("HOME")
datpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/"

# Load cleaned data
teds <- readRDS(paste0(home,datpath,"TEDSclean.rds"))
xbot <- readRDS(paste0(home,datpath,"XBOTclean.rds")) |>
  mutate(STFIPS=NA,year=NA)


combined <- rbind(xbot,teds) |>
  mutate(s_factor = factor(s))
#str(combined)

# Add factor labels ------------------------------------------------------------

levels(combined$.age_factor) <- c("18-20",
                              "21-24",
                              "25-29",
                              "30-34",
                              "35-39",
                              "40-44",
                              "45-49",
                              "50-54",
                              "55-64",
                              "65+") 

levels(combined$.race_factor) <- c("Black","White","Other")

levels(combined$.education_factor) <- c("Less than high school",
                                        "High school or GED",
                                        "Greater than high school")

levels(combined$.firstopuse_factor) <- c("<15","15-20","20-29","30+")

levels(combined$s_factor) <- c("TEDS-A","X:BOT")


# Save combined with no missing data
combined_nona <- combined |>
  filter(anyNA==0)
saveRDS(combined_nona,paste0(home,datpath,"combined.rds"))


# For tables -------------------------------------------------------------------

## Labels
list_of_labels <- list(.age_factor = "Age",
                       .age_num = "Age, median (IQR)",
                       .male = "Male",
                       .race_factor = "Race",
                       .hispanic = "Hispanic/Latine",
                       .education_factor = "Education",
                       .unemployed = "Unemployed",
                       .homeless = "Homeless",
                       .ivdrug = "Intravenous use",
                       .use_cokecrack = "Cocaine/crack",
                       .use_cannabis = "Cannabis",
                       .use_stim = "Amphetamines",
                       .use_sed = "Sedatives",
                       .firstopuse_factor = "Age at first use",
                       anyNA = "Any missing",
                       s_factor = "Sample")
          
## Function for getting latex
getlatex <- function(input){
  input |>
    as_latex() |>
    as.character() |>
    cat()
}

## Subset variables
charac <- c(".age_num",
          ".age_factor",
          ".male",
          ".race_factor",
          ".hispanic",
          ".education_factor",
          ".unemployed",
          ".homeless")
opuse <- c(".ivdrug",
             ".firstopuse_factor")
othuse <- c(".use_cannabis",
            ".use_cokecrack",
            ".use_stim",
            ".use_sed")
charac_yr <- c("year",charac)


# Missingness in TEDS ----------------------------------------------------------

tbl_miss <- combined |>
  filter(s==0) |>
  select(charac,opuse,othuse,anyNA,-".age_num") |>
  mutate(across(contains("."), ~ case_when(is.na(.x) ~ 1, TRUE ~ 0))) |>
  gtsummary::tbl_summary(
    label = list_of_labels) |>
  #modify_spanning_header(all_stat_cols() ~ "**label, N(%)**") |>
  #bold_labels() |>
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}") |>
  modify_footnote(c(all_stat_cols()) ~ NA)


tbl_miss

getlatex(as_gt(tbl_miss))

# Assess years in TEDS ---------------------------------------------------------

states <- read.csv(paste0(home,datpath,"TEDSstates.csv"))

tedsyrssts <- combined_nona |>
  filter(s==0)|> 
  select(STFIPS,year) |>
  distinct() |>
  group_by(STFIPS,year) |>
  summarise(n=n()) |>
  ungroup() |>
  pivot_wider(names_from = year,values_from = n,id_cols = STFIPS) |>
  right_join(states, join_by("STFIPS"=="Num")) |>
  select(-STFIPS) |>
  relocate(State) |>
  mutate(across(where(is.numeric), ~ case_when(is.na(.x) ~ 0, .default=1))) |>
  arrange(State)


  
gt(rbind(tedsyrssts,
         tibble(State="Total",
               "2014"=sum(tedsyrssts$`2014`),
               "2015"=sum(tedsyrssts$`2015`),
               "2016"=sum(tedsyrssts$`2016`),
               ))) |> getlatex()



# Table 1 ----------------------------------------------------------------------

## Function for subsections
subtbls <- function(data,vars){
  data |>
    select(s_factor,
           all_of(vars),) |>
    gtsummary::tbl_summary(
      by = s_factor,
      label = list_of_labels) |>
    modify_spanning_header(all_stat_cols() ~ "**Sample, N(%)**") |>
    modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}") |>
    modify_footnote(c(all_stat_cols()) ~ NA) 
}


tbl_charac <- subtbls(combined_nona,charac)
tbl_opuse <- subtbls(combined_nona,opuse)
tbl_othuse <- subtbls(combined_nona,othuse)
  
tbl1 <- tbl_stack(list(tbl_charac,
                       tbl_opuse,
                       tbl_othuse),
                    group_header = c("General",
                                     "Opioid use characteristics",
                                     "Other substance use")) %>%
    as_gt() %>%
    gt::tab_style(
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_row_groups(groups = everything()))


tbl1

getlatex(tbl1)

