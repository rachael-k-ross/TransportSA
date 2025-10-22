
# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Generating results and figures (except for mc sampling analysis)
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

# Paths
home <- Sys.getenv("HOME")
datpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Data/"
outpath <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Figures/"
code <- "/07 Postdoc/02 Projects/13 Sensitivity analysis/Code/"

# Data
combined <- readRDS(paste0(home,datpath,"combined.rds"))
preds <- readRDS(paste0(home,datpath,"preds.rds")) |> 
  inner_join(combined, by=c("id"))
preds_s1 <- preds |> filter(s==1) 
preds_s0 <- preds |> filter(s==0)

# Call in functions for analysis
source(paste0(home,code,"forgit/00_analysisfxs.R"))


# For figures ------------------------------------------------------------------

# Colors
mycolors <- c("#72925f","#925f72")
mycolors <- c("darkgray","darkgray")
mycolors <- c("#525252","#525252")

# For saving figures
savefig <- function(fig,w,h){
  name <- deparse(substitute(fig))
  ggsave(paste0(home,outpath,name,".png"),fig,
         units = "in",width = w,height = h,dpi = 600)
}


# Selection diagnostics --------------------------------------------------------

round(summary(preds_s1$s1hat),5)
round(summary(preds_s0$s1hat),5)
summary(preds_s1$s1hat/mean(preds$s))
preds_s0_iow <- preds_s0 |>
  mutate(oddswt_stab = (1-s1hat)/s1hat*(mean(preds$s)/(1-mean(preds$s))),
         oddswt = (1-s1hat)/s1hat)
preds_s1_iow <- preds_s1 |>
  mutate(oddswt_stab = (1-s1hat)/s1hat*(mean(preds$s)/(1-mean(preds$s))),
         oddswt = (1-s1hat)/s1hat)
summary(preds_s0_iow$oddswt)
summary(preds_s0_iow$oddswt_stab)
summary(preds_s1_iow$oddswt)
summary(preds_s1_iow$oddswt_stab)
  
# prop.table(table(preds_s0 |> 
#         mutate(cat = case_when(s1hat<=0.01 ~ 1,
#                                .default = 0)) |> pull(cat)))

ggplot(preds_s0,aes(x=s1hat)) + 
  geom_density() +
  ylab("Density") +
  xlab("Probability of selection") +
  theme_classic() +
  scale_x_continuous(limits=c(0,.03))

preds_s0 |>
  mutate(s1hat_scaled = s1hat/mean(preds$s)) %>%
  ggplot(.,aes(x=s1hat_scaled)) + 
  geom_density() +
  ylab("Density") +
  xlab("Scaled probability of selection") +
  theme_classic() +
  scale_x_continuous(limits=c(0,5))


#savefig(trapplot,5,3.5)






# Adherence summary ----------------------------------------------------

table(combined$z[combined$s==1],combined$a[combined$s==1])

## Adherence plots in TEDS-A
foradhplot <- rbind(preds_s0 |>
                      select(z1hat_1,z1hat_0) |>
                      mutate(delta=1),
                    preds_s0 |>
                      select(z1hat_1,z1hat_0) |>
                      mutate(z1hat_1=z1hat_1*0.75,
                             z1hat_0=z1hat_0*0.75,
                             delta=0.75),
                    preds_s0 |> 
                      select(z1hat_1,z1hat_0) |>
                      mutate(z1hat_1=z1hat_1*0.5,
                             z1hat_0=z1hat_0*0.5,
                             delta=0.5)) |>
  pivot_longer(cols = c(z1hat_1,z1hat_0),names_to = "trt",
               names_prefix = "z1hat_",values_to = "adh") |>
  mutate(delta=factor(delta),
         trt=factor(trt),
         group=paste0(delta,trt))

levels(foradhplot$trt) <- c("BUP-NX","XR-NTX")

# ggplot() +
#   geom_density(data = foradhplot, 
#                aes(x=adh,
#                    group = group,
#                    colour=trt,
#                    fill=trt),alpha=.2,linewidth=0.7,
#                trim=TRUE) +
#   ylab("Density") +
#   xlab("Probability of adherence") +
#   theme_classic() +
#   scale_x_continuous(limits=c(0,1)) +
#   scale_colour_manual(values = mycolors) +
#   scale_fill_manual(values = mycolors) +
#   theme(legend.title=element_blank(),
#         legend.position = c(.2,.8),
#         legend.text = element_text(size=7),
#         legend.key.size = unit(.5, 'cm'), #change legend key size
#         legend.key.height = unit(.5, 'cm'), #change legend key height
#         legend.key.width = unit(.5, 'cm'), #change legend key width
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"),
#         legend.margin=margin(t=-0.12,l=0.07,b=0.07,r=0.15, unit='cm'))

label1 <- expression(delta==1)
label2 <- expression(delta==0.75)
label3 <- expression(delta==0.5)
toplotadh <- function(trt,color,label1x,label1y,
                      label2x,label2y,label3x,label3y,tagy){
  ggplot() +
    geom_density(data = foradhplot[foradhplot$trt==trt,], 
                 aes(x=adh,
                     group =group,
                     alpha=group,
                     linewidth=group,
                     y=after_stat(scaled)),
                 #colour=trt,
                 #fill=trt)
                 #alpha=.2,
                 colour=color,
                 fill=color,
                 trim=TRUE,
                 #linewidth=0.7
                 ) +
    ylab("Scaled density") +
    xlab("Probability of adherence") +
    theme_classic() +
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(breaks=seq(0,1,by=0.2)) +
    annotate("text", x=label1x, y=label1y, 
             label= as.character(label1),parse=TRUE,size=2.7) +
    annotate("text", x=label2x, y=label2y, 
             label= as.character(label2),parse=TRUE,size=2.7) +
    annotate("text", x=label3x, y=label3y, 
             label= as.character(label3),parse=TRUE,size=2.7) +
    annotate("text", x=.03, y=tagy, 
             label= trt,size=3) +
    scale_alpha_manual(values=c(0.1,0.4,0.8)) +
    #scale_fill_manual(values=color) +
    #scale_colour_manual(values=color) +
    scale_linewidth_manual(values=c(.4,0.6,0.8)) +
    theme(legend.position="none",
          panel.grid.major.x = element_line(color = "lightgray",
                                            linewidth = 0.05))
    #scale_y_continuous(limits=c(0,40))
}

# ntxadh <- toplotadh("XR-NTX",mycolors[2],
#                     0.79,3,
#                     0.52,8,
#                     0.23,10,
#                     12)
# bupadh <- toplotadh("BUP-NX",mycolors[1],
#                     0.96,45,
#                     0.74,65,
#                     .40,90,
#                     115)

ntxadh <- toplotadh("XR-NTX",mycolors[2],
                    0.80,.4,
                    0.51,.98,
                    0.22,.9,
                    1)
bupadh <- toplotadh("BUP-NX",mycolors[1],
                    0.97,.6,
                    0.75,.75,
                    .39,.9,
                    1)
adhplot1 <- ntxadh/bupadh #+ plot_annotation(tag_levels = "A")
adhplot1

savefig(adhplot1,5,3.5) 


## For table
rbind(rev(table(preds_s1$TRT)),
      rev(table(preds_s1$TRT[preds_s1$z==1])),
      c(mean(preds_s1$z[preds_s1$a==1]),
        mean(preds_s1$z[preds_s1$a==0])))

ntxadhsum <- summary(preds_s0$z1hat_1)
bupadhsum <- summary(preds_s0$z1hat_0)

foradhtable <- function(summary){
  rbind(round(c(summary[[4]],summary[[3]],summary[[2]],summary[[5]]),2),
  round(c(summary[[4]]*.75,summary[[3]]*.75,summary[[2]]*.75,summary[[5]]*.75),2),
  round(c(summary[[4]]*.5,summary[[3]]*.5,summary[[2]]*.5,summary[[5]]*.5),2))
}
foradhtable(ntxadhsum)
foradhtable(bupadhsum)

## Higher prob for NTX than BUP
nrow(preds_s1 %>% filter(z1hat_0<z1hat_1))/sum(preds$s) # is anyones ntx adh higher than bup adh?
nrow(preds_s0 %>% filter(z1hat_0<z1hat_1))/sum(1-preds$s)

## RUN STUFF FROM COMBINE_DESC FIRST##
# subtbls <- function(data,vars){
#   data |>
#     mutate(higher=factor(ifelse(z1hat_0<z1hat_1,1,0))) |>
#     select(higher,
#            all_of(vars),) |>
#     gtsummary::tbl_summary(
#       by = higher,
#       label = list_of_labels) |>
#     modify_spanning_header(all_stat_cols() ~ "**Sample, N(%)**") |>
#     modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}") |>
#     modify_footnote(c(all_stat_cols()) ~ NA) 
# }
# 
# 
# tbl_charac <- subtbls(preds_s0,charac)
# tbl_opuse <- subtbls(preds_s0,opuse)
# tbl_othuse <- subtbls(preds_s0,othuse)
# 
# tbl1 <- tbl_stack(list(tbl_charac,
#                        tbl_opuse,
#                        tbl_othuse),
#                   group_header = c("General",
#                                    "Opioid use characteristics",
#                                    "Other substance use")) %>%
#   as_gt() %>%
#   gt::tab_style(
#     style = gt::cell_text(style = "italic"),
#     locations = gt::cells_row_groups(groups = everything()))
# 
# 
# tbl1
# 
# getlatex(tbl1)

# Estimating risks/effects -----------------------------------------------------

## Crude trial results
table(preds_s1$y)
mean(preds_s1$y)
mean(preds_s1$y[preds_s1$a==1])
mean(preds_s1$y[preds_s1$a==0])
mean(preds_s1$y[preds_s1$a==1])-mean(preds_s1$y[preds_s1$a==0])

## Adjusted trial results
round(gettrialresults(trialestimator(preds)),2)

## Transport results assuming no trial effects
round(getresults(oldestimator(preds)),2)

## New estimand: Approach A
#getresults(newestimator(preds,1,1)) #just to compare to old estimator
round(getresults(newestimator(preds,0.75,0.75)),2)
round(getresults(newestimator(preds,0.5,0.5)),2)
#getresults(newestimator(preds,0.75,0.5))
