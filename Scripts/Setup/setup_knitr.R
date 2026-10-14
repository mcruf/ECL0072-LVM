###########################
#                         #
#        R set-up         #
#                         #
###########################


set.seed(123) #set a common seed

#~~~~~~~~~~~~~~~~~~
# 1) Load packages
#~~~~~~~~~~~~~~~~~~
library(xaringan)
library(xaringanthemer)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(broom.mixed)
library(lme4)
library(knitr)
library(emmeans)
library(INLA)


#~~~~~~~~~~~~~~~~~~~~~~~~~~                  
# 2. Knitr configuration
#~~~~~~~~~~~~~~~~~~~~~~~~~~
knitr::opts_chunk$set(cache = TRUE,
                      tidy = FALSE,
                      comment = NA,
                      echo = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      cache = TRUE,
                      dev = "svg",
                      fig.retina = 3,
                      fig.align = 'center',
                      dev = "svglite",
                      #fig.path = "Figuras/",
                      cache.path = "cache/") 
                  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Define main ggplot theme
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme_bw(base_size = 12) + 
            theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 10)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 10)),
                  axis.title = element_text(face = "bold", colour = "black", size = 12),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  plot.background   = element_rect(fill = "transparent", color = NA),
                  panel.background  = element_rect(fill = "transparent", color = NA),
                  panel.grid.major.y = element_line(linetype = 'dotted'),
                  panel.grid.major.x = element_line(linetype = 'dotted'),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))


                  