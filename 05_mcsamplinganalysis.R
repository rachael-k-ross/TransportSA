
# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Generating results and figures for MC sampling analysis
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

# For saving figures
savefig <- function(fig,w,h){
  name <- deparse(substitute(fig))
  ggsave(paste0(home,outpath,name,".png"),fig,
         units = "in",width = w,height = h,dpi = 600)
}


# Delta values -----------------------------------------------------------------

## XR-NTX
min1 <- 0.5
max1 <- 1
mode1_1 <- 0.6
mode1_2 <- 0.75

## BUP-NX
min0 <- .5
max0 <- 1
mode0_1 <- 0.75
mode0_2 <- 0.9


ndraws <- 10000

## Monte Carlo sampling from prob distributions
set.seed(11)
trapdraws <- tibble(
  delta0 = rtrapezoid(n = ndraws, min0, mode0_1, mode0_2, max0, n1=2,n3=2,alpha=1),
  delta1 = rtrapezoid(n = ndraws, min1, mode1_1, mode1_2, max1, n1=2,n3=2,alpha=1),
  flag1 = factor(ifelse(delta1>delta0,1,0))
)


## Trapezoidal plot
x <- seq(0,1,0.01)
density <- rbind(tibble(trt=rep("BUP-NX",length(x)),
                        delta=x,
                        density=dtrapezoid(x=x, min0, mode0_1, mode0_2, max0, n1=2,n3=2,alpha=1)),
                 tibble(trt=rep("XR-NTX",length(x)),
                        delta=x,
                        density=dtrapezoid(x=x, min1, mode1_1, mode1_2, max1, n1=2,n3=2,alpha=1)))

trapplot <- ggplot(density, aes(x=delta,y=density,group=trt,colour=trt,linetype=trt)) +
  geom_line(linewidth=0.8) + theme_classic() +
  ylab("Density") +
  xlab("Delta") +
  theme(legend.title=element_blank(),
        legend.position = c(.2,.8),
        legend.text = element_text(size=7),
        legend.key.size = unit(.5, 'cm'), #change legend key size
        legend.key.height = unit(.5, 'cm'), #change legend key height
        legend.key.width = unit(.5, 'cm'), #change legend key width
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.margin=margin(t=-0.12,l=0.07,b=0.07,r=0.15, unit='cm'),
        panel.grid.major.x = element_line(color = "lightgray",
                                          linewidth = 0.05)) +
  scale_colour_manual(values = mycolors) +
  scale_linetype_manual(values=c("solid","twodash")) +
  scale_x_continuous(limits=c(0.3,1))
trapplot

savefig(trapplot,5,3.5)



# ## Trapezoidal plot
# set.seed(11)
# fortrapplot <- tibble(
#   delta1 = rtrapezoid(n = ndraws*100, min1, mode1_1, mode1_2, max1, n1=2,n3=2,alpha=1),
#   ratio = rtrapezoid(n = ndraws*100, 1, 1.1, 1.25, 1.3, n1=2,n3=2,alpha=1),
#   delta0 = max(delta1*ratio,1)
# )
# 
# density <- trapdraws |>
#   select(-ratio) |>
#   pivot_longer(cols=c("delta0","delta1"),names_to = "trt",values_to = "delta")
# 
# 
# #trapplot <- 
# ggplot(density, aes(x=delta,group=trt,colour=trt,linetype=trt)) +
#   geom_density(linewidth=0.8,trim=TRUE) + theme_classic() +
#   ylab("Density") +
#   xlab("Delta") +
#   theme(legend.title=element_blank(),
#         legend.position = c(.2,.8),
#         legend.text = element_text(size=7),
#         legend.key.size = unit(.5, 'cm'), #change legend key size
#         legend.key.height = unit(.5, 'cm'), #change legend key height
#         legend.key.width = unit(.5, 'cm'), #change legend key width
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"),
#         legend.margin=margin(t=-0.12,l=0.07,b=0.07,r=0.15, unit='cm'),
#         panel.grid.major.x = element_line(color = "lightgray",
#                                           linewidth = 0.05)) +
#   scale_colour_manual(values = mycolors) +
#   scale_linetype_manual(values=c("solid","twodash")) +
#   scale_x_continuous(limits=c(0.3,1))
# trapplot

# Adherence summary ------------------------------------------------------------

margadh_ntx=map(trapdraws$delta1, ~ mean(preds_s0$z1hat_1*.x))
margadh_bup=map(trapdraws$delta0, ~ mean(preds_s0$z1hat_0*.x))

fromdraws <- trapdraws |>
  mutate(margadh_ntx = unlist(margadh_ntx),
         margadh_bup = unlist(margadh_bup),
         flag2 = factor(ifelse(margadh_ntx>margadh_bup,1,0)))

table(fromdraws$flag1)
table(fromdraws$flag2)
round(summary(fromdraws$margadh_ntx),3)
round(summary(fromdraws$margadh_bup),3)
round(summary(fromdraws$margadh_ntx[fromdraws$flag1==0]),3)
round(summary(fromdraws$margadh_bup[fromdraws$flag1==0]),3)

scatdeltas <- ggplot(data=fromdraws,aes(x=delta0,
                                        y=delta1,
                                        #shape = flag1,
                                        fill=flag1,
                                        colour=flag1)) +
  geom_point(size=1.2,alpha=0.7) +
  xlab(expression("BUP-NX,"~delta)) +
  ylab(expression("XR-NTX,"~delta)) +
  theme_classic() +
  scale_x_continuous(limits=c(0.4,1)) +
  scale_y_continuous(limits=c(0.4,1)) +
  geom_abline(intercept=0,slope=1,color="darkgray",linetype="longdash") +
  #scale_shape_manual(values=c(16,1)) +
  scale_fill_manual(values=c("black","#636363"))+
  scale_colour_manual(values=c("black","#636363"))+
  theme(legend.position="none",
        panel.grid.major.x = element_line(color = "lightgray",
                                          linewidth = 0.05),
        panel.grid.major.y = element_line(color = "lightgray",
                                          linewidth = 0.05))

scatadhprs <- ggplot(data=fromdraws,aes(x=margadh_bup,
                                        y=margadh_ntx,
                                        #shape = flag1,
                                        fill=flag1,
                                        colour=flag1)) +
  geom_point(size=1.2,alpha=0.7) +
  xlab("BUP-NX, adherence") +
  ylab("XR-NTX, adherence") +
  theme_classic() +
  scale_x_continuous(limits=c(0.3,1)) +
  scale_y_continuous(limits=c(0.3,1)) +
  geom_abline(intercept=0,slope=1,color="darkgray",linetype="longdash") +
  #scale_colour_manual(values = c("black","#92805f")) +
  #scale_shape_manual(values=c(16,1)) +
  scale_fill_manual(values=c("black","#636363"))+
  scale_colour_manual(values=c("black","#636363"))+
  theme(legend.position="none",
        panel.grid.major.x = element_line(color = "lightgray",
                                          linewidth = 0.05),
        panel.grid.major.y = element_line(color = "lightgray",
                                          linewidth = 0.05))


adhplot2 <- scatdeltas/scatadhprs
adhplot2
savefig(adhplot2,5,5) 



# Estimating risks/effects -----------------------------------------------------

results <- map2(trapdraws$delta0,trapdraws$delta1,getresultsmc,data=preds)

fullresults <- cbind(fromdraws,reduce(results,rbind)) |>
  mutate(rd_percent = rd*100,
         rd_lcl_percent = rd_lcl*100,
         rd_ucl_percent = rd_ucl*100)

getquantiles <- function(data){
  round(c(quantile(data,0.5),
    quantile(data,0.025),
    quantile(data,0.975)),3)
}

# Simulation results
getquantiles(fullresults$r1)
getquantiles(fullresults$r0)
getquantiles(fullresults$rd)
getquantiles(fullresults$r1[fullresults$flag1==0])
getquantiles(fullresults$r0[fullresults$flag1==0])
getquantiles(fullresults$rd[fullresults$flag1==0])
#getquantiles(fullresults$rd[fullresults$flag1==1])
#getquantiles(fullresults$rd[fullresults$flag2==0])
#getquantiles(fullresults$rd[fullresults$flag2==1])

# Incorporate random error
set.seed(11)
wre <- fullresults |>
  mutate(r1wre = r1 - rnorm(ndraws,mean = 0,sd = r1_se),
         r0wre = r0 - rnorm(ndraws,mean = 0,sd = r0_se),
         rdwre = rd - rnorm(ndraws,mean = 0,sd = rd_se))
getquantiles(wre$r1wre)
getquantiles(wre$r0wre)
getquantiles(wre$rdwre)
getquantiles(wre$r1wre[wre$flag1==0])
getquantiles(wre$r0wre[wre$flag1==0])
getquantiles(wre$rdwre[wre$flag1==0])


# Plots
mcplot <- ggplot() + 
  geom_density(data=fullresults[fullresults$flag1==0,],
               aes(x=rd_percent, y = ..count../sum(..count..)),
               linetype="longdash",
               linewidth=0.4,
               fill="black",
               alpha=0.2) +
  geom_density(data=fullresults,
               aes(x=rd_percent, y = ..count../sum(..count..)),
               #linetype="longdash",
               linewidth=0.7,
               fill="black",
               alpha=0.6) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(limits=c(25,45)) +
  ylab("Percent") +
  xlab("Risk difference (in percentage points)") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "lightgray",
                                          linewidth = 0.05))
mcplot

savefig(mcplot,5,3.5) 

fullresults |>
  
ggplot() + 
  geom_density(data=fullresults,
               aes(x=rd_percent, y = ..count../sum(..count..)),
               linetype="longdash",
               linewidth=0.4,
               fill="black",
               alpha=0.2) +
  geom_density(data=fullresults,
               aes(x=rd_lcl_percent, y = ..count../sum(..count..)),
               #linetype="longdash",
               linewidth=0.7,
               fill="black",
               alpha=0.6) +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(limits=c(0,100)) +
  ylab("Percent") +
  xlab("Risk difference (in percentage points)") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(color = "lightgray",
                                          linewidth = 0.05))


