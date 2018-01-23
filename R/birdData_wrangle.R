rm(list=ls())

library(ade4)
library(tidyverse)
data(aviurba)
str(aviurba)
# Generate the spatial matrix corresponding to data(aviurba):
x <- c(18:15,rep(14,2),13,13:8,12:8, 7:3,2:1,rep(1:2,6),rep(3:4,5),5:8)
y <- c(rep(12,5),rep(13,2),rep(12,6),rep(13,5),rep(12,7),rep(11:7,rep(2,5)),rep(6,4),rep(5:3,rep(2,3)),
       rep(2,6))
xy <- cbind(x,y)
# Visualization
#s.label(xy, label = 1:51, include.origin = FALSE)

# check that the community matrix is in the right shape for mobr: sites = rows, species = columns
head(aviurba$fau) # ready to go

# recode habitat covariates for use in mobr
habitat <- with(aviurba$mil,
                ifelse((grassland=='yes' & scrubby=='no' & deciduous=='no' & conifer=='no'), 'grassland',
                       ifelse((grassland=='no' & scrubby=='yes' & deciduous=='no' & conifer=='no'), 'scrub',
                              ifelse((grassland=='no' & scrubby=='no' & deciduous=='yes' & conifer=='no'), 'deciduous',
                                     ifelse((grassland=='no' & scrubby=='no' & deciduous=='no' & conifer=='yes'), 'conifer',
                                            ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='no' & conifer=='no'), 'grassland + scrub',
                                                   ifelse((grassland=='yes' & scrubby=='no' & deciduous=='yes' & conifer=='no'), 'grassland + deciduous',
                                                          ifelse((grassland=='yes' & scrubby=='no' & deciduous=='no' & conifer=='yes'), 'grassland + conifer',
                                                                 ifelse((grassland=='no' & scrubby=='yes' & deciduous=='yes' & conifer=='no'), 'scrub + deciduous',
                                                                        ifelse((grassland=='no' & scrubby=='yes' & deciduous=='no' & conifer=='yes'), 'scrub + conifer', 
                                                                               ifelse((grassland=='no' & scrubby=='no' & deciduous=='yes' & conifer=='yes'), 'deciduous + conifer', 
                                                                                      ifelse((grassland=='no' & scrubby=='yes' & deciduous=='yes' & conifer=='yes'), 'scrub + deciduous + conifer',
                                                                                             ifelse((grassland=='yes' & scrubby=='no' & deciduous=='yes' & conifer=='yes'), 'grassland + deciduous + conifer',
                                                                                                    ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='yes' & conifer=='no'), 'grassland + scrub + deciduous',
                                                                                                           ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='yes' & conifer=='yes'), 'all', 'unknown')))))))))))))))

# recode simplified habitat covariates for use in mobr
habitat_simple <- with(aviurba$mil,
                       ifelse((grassland=='yes' & scrubby=='no' & deciduous=='no' & conifer=='no'), 'grassland',
                              ifelse((grassland=='no' & scrubby=='yes' & deciduous=='no' & conifer=='no'), 'scrub',
                                     ifelse((grassland=='no' & scrubby=='no' & deciduous=='yes' & conifer=='no'), 'tree',
                                            ifelse((grassland=='no' & scrubby=='no' & deciduous=='no' & conifer=='yes'), 'tree',
                                                   ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='no' & conifer=='no'), 'mixed',
                                                          ifelse((grassland=='yes' & scrubby=='no' & deciduous=='yes' & conifer=='no'), 'mixed',
                                                                 ifelse((grassland=='yes' & scrubby=='no' & deciduous=='no' & conifer=='yes'), 'mixed',
                                                                        ifelse((grassland=='no' & scrubby=='yes' & deciduous=='yes' & conifer=='no'), 'mixed',
                                                                               ifelse((grassland=='no' & scrubby=='yes' & deciduous=='no' & conifer=='yes'), 'mixed', 
                                                                                      ifelse((grassland=='no' & scrubby=='no' & deciduous=='yes' & conifer=='yes'), 'mixed', 
                                                                                             ifelse((grassland=='no' & scrubby=='yes' & deciduous=='yes' & conifer=='yes'), 'mixed',
                                                                                                    ifelse((grassland=='yes' & scrubby=='no' & deciduous=='yes' & conifer=='yes'), 'mixed',
                                                                                                           ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='yes' & conifer=='no'), 'mixed',
                                                                                                                  ifelse((grassland=='yes' & scrubby=='yes' & deciduous=='yes' & conifer=='yes'), 'mixed', 'unknown')))))))))))))))

# add indicator covariate for habitat heterogeneity (i.e., number of different habitats)
rs <- rowSums(cbind(as.numeric(aviurba$mil$grassland), as.numeric(aviurba$mil$scrubby), 
                    as.numeric(aviurba$mil$deciduous), as.numeric(aviurba$mil$conifer)))

habitat_hetero <- ifelse(rs==8, '0',
                         ifelse(rs==7, '1',
                                ifelse(rs==6, '2',
                                       ifelse(rs==5, '3', '4'))))

# add covariate for buildings
buildings <- with(aviurba$mil,
                  ifelse((small.bui=='yes' & high.bui=='no'), 'small',
                         ifelse((small.bui=='no' & high.bui=='yes'), 'high',
                                ifelse((small.bui=='yes' & high.bui=='yes'), 'both', 'none'))))

environ <- data.frame(habitat = habitat, 
                      simple_habitat = habitat_simple,
                      heterogeneity = habitat_hetero, 
                      noise = aviurba$mil$noisy,
                      buildings = buildings,
                      x = x, 
                      y = y)

# combine all data into one dataframe for visual inspection
# wide to long
sppSite <- aviurba$fau %>%
  mutate(site = 1:nrow(aviurba$fau) %>% as.character())

sppSite_long <- sppSite %>%
  gather(species, value = abundance, Sp1:Sp40) %>%
  as_tibble()

traits <- aviurba$traits %>% 
  mutate(species = rownames(aviurba$traits)) %>%
  as_tibble()

environ <- environ %>%
  mutate(site = rownames(environ)) %>%
  as_tibble()

# put 'em all together
birds_long <- inner_join(sppSite_long, traits, by = 'species')
birds_long <- inner_join(birds_long, environ, by = 'site')

setwd('~/Dropbox/4teaching/MLU_teaching2018/data/')
save(aviurba, environ, birds_long, file = 'birdData.Rdata')
