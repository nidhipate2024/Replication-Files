# Replication of Figures 6 and B2
# Generated with the ple_dataset.dta



renv::restore()
devtools::install_github('ebenmichael/augsynth')
install.packages("MASS",repos="http://lib.stat.cmu.edu/R/CRAN")
library(tidyverse)
library(LowRankQP)
library(devtools)
library(zoo)
library(haven)
library(data.table)
library(scpi)
library(augsynth)
library(gsynth)
library(plyr)
library(purrr)
library(dplyr)
library(patchwork)
library(tidyr)
library(LowRankQP)
library(reshape2)
library(ggplot2)
library(tibble)
library(CVXR)
library(Rmpfr)

rm(list=ls(all=TRUE))
setwd('..')

data <- read_dta("ple_dataset.dta")


##############################################################################################################

#FIGURE 1 (In original article)

##############################################################################################################


# Summarize data to calculate the share of each type of populist government
df_summary <- data %>%
  group_by(year) %>%
  summarize(
    share_populist = sum(pop, na.rm = TRUE) / n() * 100,
    share_right_wing_populist = sum(rpop, na.rm = TRUE) / n() * 100,
    share_left_wing_populist = sum(lpop, na.rm = TRUE) / n() * 100
  ) %>%
  # Ensure left and right populist shares add up to the total populist share
  mutate(share_left_wing_populist = case_when(
    share_populist - share_right_wing_populist >= 0 ~ share_populist - share_right_wing_populist,
    TRUE ~ share_left_wing_populist
  ))

# Pivot data to long format for ggplot
df_long <- df_summary %>%
  pivot_longer(cols = c("share_right_wing_populist", "share_left_wing_populist"), names_to = "type", values_to = "share") %>%
  mutate(type = factor(type, levels = c("share_right_wing_populist", "share_left_wing_populist"),
                       labels = c("Right-wing populism", "Left-wing populism")))

# Plotting using ggplot2
ggplot() +
  # Add stacked areas for right-wing and left-wing populism
  geom_area(data = df_long, aes(x = year, y = share, fill = type), position = 'stack', alpha = 0.8) +
  # Add the overall populist line
  geom_line(data = df_summary, aes(x = year, y = share_populist), color = "red", size = 1) +
  scale_fill_manual(values = c("darkgray", "lightgray")) +
  labs(
    x = NULL,
    y = "Share of independent countries with populist government (%)",
    fill = NULL
  ) +
  scale_x_continuous(limits = c(1900, 2020)) + scale_y_continuous(limits = c(0, 25)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 13),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

##############################################################################################################

#FIGURE 6 and FIGURE B2

##############################################################################################################

try({
  
  cov.adj  <- NULL 
  features <- NULL  
  constant <- FALSE 
  rho      <- 'type-1'                                      
  rho.max  <- 1                                            
  u.order  <- 0                                            
  e.order  <- 0                                           
  u.lags   <- 0                                             
  e.lags   <- 0                                          
  u.sigma  <- "HC1"                                          
  e.sigma  <- "HC1"                                         
  u.missp  <- T                                           
  u.alpha  <- 0.1                                           
  e.alpha  <- 0.1                                         
  cointegrated.data <- TRUE                                
  cores    <- 1                                              
  sims     <- 200                                      
  e.method = "gaussian"                                     
  w.constr <- list(lb = 0, dir = "==", p = "L1", Q = 1)   
  
  year <- c(1973,1989,2003,1990,1968,1996,1966,1996,1994,2001,2001,1970,1975,1985,1990,1998,2003,2000,2001,2003,1999,1952,1952,1952,1946,1951,1960, 1990)
  left <- c(1   ,0   ,1   ,0   ,0   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,0   ,1   ,0   ,1   ,0   ,0   ,0   ,0   ,1   ,1   ,1   ,0   ,1   ,1   ,0    ,0)
  oid <-  c(1   ,1   ,1   ,6   ,16  ,16  ,25  ,28  ,29  ,29  ,30  ,36  ,38  ,41  ,41  ,42  ,50  ,54  ,55  ,56  ,60  ,5   ,9   ,16  ,1   ,6   ,16   ,47)
  nid <-  c(1   ,1   ,1   ,6   ,15  ,16  ,22  ,27  ,29  ,29  ,29  ,32  ,34  ,38  ,40  ,42  ,49  ,54  ,53  ,53  ,59  ,5   ,8   ,11  ,1   ,6   ,11   ,45)
  
  s <- data.frame(nid,oid,year,left)
  s$case <- paste(as.character(s$nid), as.character(s$year), sep=".")
  s$final <- paste("_", as.character(s$case))
  s$fr1 = 0
  s$fr2 = 14
  s$fr3 = 15
  s$sta = 15
  
  s[which(s$year==1952),"fr2"] = 1
  s[which(s$year==1952),"fr3"] = 9
  s[which(s$year==1946),"fr2"] = 7
  s[which(s$year==1951),"fr2"] = 2
  s[which(s$year==1951),"fr3"] = 10
  s[which(s$year==1960),"fr1"] = 1
  s[which(s$year==1990 & s$oid==47),"fr1"] = 10
  
  for (k in 1:length(s$nid)) {
    options(warn=0)
    Oldc = s$oid[k]  
    Trea = s$nid[k]
    Year = s$year[k]
    Case = s$case[k]
    Left= s$left[k]
    Set = s$final[k]
    
    data <- read_dta("ple_dataset.dta")
    #   data <- data[which(data$year >=Year- s$sta & data$year <=Year+15),]
    data <- data[which(data$year >= Year - s$sta[k] & data$year <= Year + 15), ]
    taker <- data %>% dplyr::filter(data$cid == Oldc)
    donors <- data %>% dplyr::filter(data$cid != Oldc)
    donors <- donors  %>% dplyr::mutate(simul = ifelse(atakeover == 1 & year == Year, 1,0))
    donors <- donors %>% group_by(cid) %>% dplyr::mutate(msimul = max(as.numeric(simul))) 
    donors <- donors %>%  dplyr::filter(msimul != 1)
    donors = subset(donors, select = -c(simul,msimul) )
    data <- rbind(taker, donors)
    data <- data %>% group_by(cid) %>% dplyr::filter(all(!is.na(fstgdp) | cid==Oldc ))
    data$lgfstgdp <- log(data$fstgdp)
    tysub <- data[data$year == Year, ]
    tysub <- dplyr::select(tysub, cid, country, lgfstgdp)
    names(tysub)[names(tysub) == 'lgfstgdp'] <- 'ilgfstgdp'
    data <- merge(data, tysub)
    data <- data %>% group_by(cid) %>% dplyr::mutate(d = lgfstgdp-ilgfstgdp, t = year-Year+15)
    data <- transform(data, index = as.numeric(factor(country)))
    data <-  data %>% dplyr::mutate(d = replace(d, war==1, NA))
    period.pre  <- seq(from = 0, to = 15, by = 1) 
    period.post <- (16:30)
    df  <- scdata(df = data,  features = features, constant = constant, cov.adj = cov.adj,  cointegrated.data = cointegrated.data, id.var = "index", 
                  time.var = "t", outcome.var = "d", period.pre = period.pre, period.post = period.post, unit.tr = Trea, unit.co = unique(data$index)[-Trea])  
    result <-  scpi(data = df, u.order = u.order, u.lags = u.lags, u.sigma = u.sigma, u.missp = u.missp,  e.order = e.order, e.lags = e.lags,  
                    u.alpha = u.alpha, e.alpha = e.alpha, rho = rho,  rho.max = rho.max, sims = sims, w.constr = w.constr, cores = cores, e.method = e.method)
    y.fit <- rbind(result$est.results$Y.pre.fit, result$est.results$Y.post.fit)
    yfit    <- data.frame(t = c(s$fr1[k]:s$fr2[k], s$fr3[k]:30), yfit = c(y.fit))
    y.act <- rbind(result$data$Y.pre, result$data$Y.post)
    yact    <- data.frame(t = c(period.pre, period.post), yact = c(y.act), case = Case, left = Left)
    ys   <-  merge(yact, yfit, by.x="t", by.y="t", all = TRUE)
    scl.gauss  <- result$inference.results$CI.all.gaussian[, 1, drop = FALSE]
    scr.gauss  <- result$inference.results$CI.all.gaussian[, 2, drop = FALSE]
    scl.insample  <- result$inference.results$CI.in.sample[, 1, drop = FALSE]
    scr.insample  <- result$inference.results$CI.in.sample[, 2, drop = FALSE]
    cis <- data.frame(t = c(period.post), sclinsample = c(scl.insample), scrinsample = c(scr.insample),  sclgauss = c(scl.gauss), scrgauss = c(scr.gauss))
    series  <-  merge(ys, cis, by.x="t", by.y="t", all = TRUE)
    assign(paste(Set), series) 
  } 
  
  dfs <- lapply(ls(pattern="_"), function(x) get(x))
  finaldata <- rbindlist(dfs)
  
  finaldata <- finaldata %>% group_by(case) %>% dplyr::mutate(ti = t - 15)
  
  finaldata$sclinsample[finaldata$ti == 0] <- 0
  finaldata$scrinsample[finaldata$ti == 0] <- 0
  finaldata$sclgauss[finaldata$ti == 0] <- 0
  finaldata$scrgauss[finaldata$ti == 0] <- 0
  
  finaldata$all <- 1
  finaldata$right <- NA
  finaldata$right <- ifelse(finaldata$left == 1, 0, 1)
  
  dataforplot_all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_all=mean(yfit[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, yact_all=mean(yact[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrinsample_all=mean(scrinsample[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclinsample_all=mean(sclinsample[all==1], na.rm=TRUE)), 
                                                                       ddply(finaldata, .(ti), summarise, scrgauss_all=mean(scrgauss[all==1], na.rm=TRUE)),
                                                                       ddply(finaldata, .(ti), summarise, sclgauss_all=mean(sclgauss[all==1], na.rm=TRUE)))) 
  
  dataforplot_left <- Reduce(function(x, y) merge(x, y, left=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_left=mean(yfit[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, yact_left=mean(yact[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrinsample_left=mean(scrinsample[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclinsample_left=mean(sclinsample[left==1], na.rm=TRUE)), 
                                                                         ddply(finaldata, .(ti), summarise, scrgauss_left=mean(scrgauss[left==1], na.rm=TRUE)),
                                                                         ddply(finaldata, .(ti), summarise, sclgauss_left=mean(sclgauss[left==1], na.rm=TRUE))))
  
  dataforplot_right <- Reduce(function(x, y) merge(x, y, right=TRUE), list(ddply(finaldata, .(ti), summarise, yfit_right=mean(yfit[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, yact_right=mean(yact[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrinsample_right=mean(scrinsample[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclinsample_right=mean(sclinsample[right==1], na.rm=TRUE)), 
                                                                           ddply(finaldata, .(ti), summarise, scrgauss_right=mean(scrgauss[right==1], na.rm=TRUE)),
                                                                           ddply(finaldata, .(ti), summarise, sclgauss_right=mean(sclgauss[right==1], na.rm=TRUE))))
  
  if(.Platform$OS.type == "windows") withAutoprint({
    windowsFonts(Times=windowsFont("Times New Roman"))
  })
  
  gdp_trends_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclgauss_all, ymax=scrgauss_all, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_all, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclgauss_left, ymax=scrgauss_left, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_left, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5)) 
  
  gdp_trends_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclgauss_right, ymax=scrgauss_right, x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrgauss_right, color = "90% CI (out-of-sample uncertainty)", linetype = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (out-of-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_gap_all_gauss <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=(sclgauss_all-yact_all)*(-1), ymax=(scrgauss_all-yact_all)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_all-yact_all)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_all-yact_all)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "blue", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_gap_left_gauss <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=(sclgauss_left-yact_left)*(-1), ymax=(scrgauss_left-yact_left)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_left-yact_left)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_left-yact_left)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_left-yact_left)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "darkred", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))
  
  gdp_gap_right_gauss <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=(sclgauss_right-yact_right)*(-1), ymax=(scrgauss_right-yact_right)*(-1), x=ti, fill="90% CI (out-of-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = (sclgauss_right-yact_right)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(x=ti, y = (scrgauss_right-yact_right)*(-1), color = "90% CI (out-of-sample uncertainty)", size = "90% CI (out-of-sample uncertainty)")) +
    geom_line(aes(y=(yfit_right-yact_right)*(-1), x=ti, colour = "Doppelganger gap (avg.)", fill="Doppelganger gap (avg.)", size="Doppelganger gap (avg.)"))+
    scale_colour_manual(name='', values=c("Doppelganger gap (avg.)" = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Doppelganger gap (avg.)" = "black", "90% CI (out-of-sample uncertainty)" = "grey95")) +
    scale_size_manual(name = '',  values=c("Doppelganger gap (avg.)" = 0.4, "90% CI (out-of-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.30, 0.10), breaks = c(-0.30,-0.25,-0.2,-0.15,-0.1,-0.05,0,0.05,0.1), labels = format(c("-30%","-25%","-20%","-15%","-10%","-5%","0%","+5%","+10%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+  theme_bw()+  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+ geom_hline(yintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+  theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+ guides(color=guide_legend(keyheight = 0.5, override.aes=list(fill=NA)))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 2, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) + theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))    
  
  gdp_trends_gauss = (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) & 
    plot_annotation(title = 'Panel A: Trends', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  gdp_gap_gauss = (gdp_gap_all_gauss + gdp_gap_left_gauss + gdp_gap_right_gauss)  & 
    plot_annotation(title = 'Panel B: Doppelganger gap', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times")))
  
  wrap_elements(gdp_trends_gauss) / wrap_elements(gdp_gap_gauss) 
  
  outpath <- file.path("figures", "Figure6.pdf")
  ggsave(outpath, wrap_elements(gdp_trends_gauss) / wrap_elements(gdp_gap_gauss), width = 23, height = 16, units = "cm")
  
  gdp_trends_all_insample <- ggplot(dataforplot_all) + 
    geom_ribbon(aes(ymin=sclinsample_all, ymax=scrinsample_all, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_all, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_all, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_all, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "blue", "Doppelganger avg." = "blue", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmall=1), expand=c(0.02,0.02))+ 
    labs(title = "All populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))     
  
  gdp_trends_left_insample <- ggplot(dataforplot_left) + 
    geom_ribbon(aes(ymin=sclinsample_left, ymax=scrinsample_left, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_left, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_left, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_left, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "darkred", "Doppelganger avg." = "darkred", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmleft=1), expand=c(0.02,0.02))+ 
    labs(title = "Left-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))   
  
  gdp_trends_right_insample <- ggplot(dataforplot_right) + 
    geom_ribbon(aes(ymin=sclinsample_right, ymax=scrinsample_right, x=ti, fill="90% CI (in-sample uncertainty)"), alpha = 1) +
    geom_line(aes(x=ti, y = sclinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(x=ti, y = scrinsample_right, color = "90% CI (in-sample uncertainty)", linetype = "90% CI (in-sample uncertainty)", size = "90% CI (in-sample uncertainty)")) +
    geom_line(aes(y=yfit_right, x=ti, colour = "Doppelganger avg.", fill="Doppelganger avg.", linetype="Doppelganger avg.", size="Doppelganger avg."))+
    geom_line(aes(y=yact_right, x=ti, colour = "Populist avg.", fill="Populist avg.", linetype="Populist avg.", size="Populist avg."))+
    scale_colour_manual(name='', values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_fill_manual(name = '',  values=c("Populist avg." = "black", "Doppelganger avg." = "black", "90% CI (in-sample uncertainty)" = "grey95")) +
    scale_linetype_manual(name='', values=c("Populist avg." = "solid", "Doppelganger avg." = "longdash", "90% CI (in-sample uncertainty)" = "solid")) +
    scale_size_manual(name='', values=c("Populist avg." = 0.4, "Doppelganger avg." = 0.4, "90% CI (in-sample uncertainty)" = 0.4)) +
    theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_continuous(breaks=seq(-15,15,5), expand=c(0.02,0.02))+
    scale_y_continuous(limits = c(-0.40, 0.60), breaks = c(-0.4,-0.2,0,0.2,0.4,0.60), labels = format(c("-40%","-20%","0%","+20%","+40%","+60%"),nsmright=1), expand=c(0.02,0.02))+ 
    labs(title = "Right-wing populists", x = "", y = "")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_vline(xintercept = 0, linetype="dashed",size=0.2)+
    theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 10))+ theme(aspect.ratio=3/4.25)+ 
    theme(plot.margin = unit(c(0.06,0.06,0.06,0.06), "cm"))+ theme(panel.border = element_rect(size = 0.3))+
    theme(axis.ticks = element_line(size = 0.3))+guides(color=guide_legend(ncol = 1, nrow = 3, keyheight = 0.7, override.aes=list(fill=NA)))+
    theme(legend.position = "bottom")+theme(legend.text = element_text(size = 7))+
    theme(text = element_text(family="Times")) +theme(axis.text = element_text(size = 6))+
    theme(legend.margin=margin(-18, +5, 0, -5))  
  
  gdp_trends_insample = (gdp_trends_all_insample + gdp_trends_left_insample + gdp_trends_right_insample) &
    plot_annotation(title = 'Panel A: Accounting for in-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  gdp_trends_gauss = (gdp_trends_all_gauss + gdp_trends_left_gauss + gdp_trends_right_gauss) & 
    plot_annotation(title = 'Panel B: Accounting for out-of-sample uncertainty', theme = theme(plot.title = element_text(size = 12, hjust=0.5, vjust=5, family="Times"))) 
  
  wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss) 
  
  outpath <- file.path("figures", "FigureB2.pdf")
  ggsave(outpath, wrap_elements(gdp_trends_insample) / wrap_elements(gdp_trends_gauss), width = 23, height = 16, units = "cm")
  
  rm(list=ls(all=TRUE))
  
}, silent = FALSE)

