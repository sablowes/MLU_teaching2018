# code to do some visual inspection and analysis of bird data from ade4 package
rm(list=ls())

# mobr
library(mobr)
library(tidyverse)

# bird data
load(url('https://github.com/sablowes/MLU_teaching2018/raw/master/data/birdData.Rdata'))

# some exploratory visualisations of the data...

# or, prepare for mobr analyses...
# fix rownames of community matrix (to avoid warning)
rownames(aviurba$fau) <- 1:nrow(aviurba$fau)
# create mob-in object
bird_mob_in <- make_mob_in(aviurba$fau, environ)

