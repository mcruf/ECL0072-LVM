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
library(broom.mixed)
library(lme4)
library(knitr)
library(emmeans)


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
                      fig.align = 'center',
                      dev = "svglite",
                      #fig.path = "Figuras/",
                      cache.path = "cache/") 
                  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3) Define main ggplot theme
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_theme <- theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, face = 'bold'),
                  legend.title = element_blank(),
                  axis.text.x = element_text(colour = "grey20", margin = margin(t = 25)),
                  axis.text.y = element_text(colour = "grey20", margin = margin(r = 20)),
                  axis.title = element_text(face = "bold", colour = "black", size = 16),
                  strip.background = element_rect(fill = 'white'),
                  legend.background = element_rect(fill = "transparent", colour = NA),
                  legend.box.background = element_rect(fill = "transparent", colour = NA),
                  axis.line = element_line(color = "black"),
                  plot.margin = margin(5, 10, 5, 5))


                  