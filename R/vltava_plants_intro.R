# code to explore Vltava river valley vegetation data: intro to GLM, GAM, GLMM...

rm(list=ls())
library(tidyverse)
library(cowplot)
#-------load data and prepare it for visualisation and modelling-------------------
load (url ('http://www.davidzeleny.net/anadat-r/lib/exe/fetch.php/data:vltava.r'))
str(vltava)

# separate into site x species and environment dataframes
env <- vltava$env %>% as_tibble() %>%
  mutate(site = 1:nrow(env))
# pairs(env)


comm <- vltava$spe %>% as_tibble() 

# simplify comm into N (perCov > 100?) and S (per site), convert to long format first
comm_long <- comm %>%
  mutate(site = 1:nrow(comm)) %>%
  gather(species, value = perCov, Abiealb23:Violtri1) %>%
  as_tibble()

comm_summary <- comm_long %>%
  filter(perCov > 0) %>%
  group_by(site) %>%
  summarise(
    N = sum(perCov),
    S = n_distinct(species)
  )

comm_presence <- comm_long %>%
  group_by(site, species) %>%
  summarise(
    occurrence = ifelse(perCov==0, 0, 1)) %>%
  ungroup() %>%
  group_by(site) %>%
  summarise(
    n_spp_present = sum(occurrence),
    spp_pool = n_distinct(species)) %>% 
  ungroup()
  

site_occupancy <- comm_long %>%
  group_by(site, species) %>%
  summarise(
    occurrence = ifelse(perCov==0, 0, 1))

vdat <- bind_cols(comm_summary, env)

site_occupancy <- inner_join(env, site_occupancy, by = 'site') %>%
  select(-TRANSECT, -LIGHT,  -TEMP,  -CONT, -MOIST, -REACT, -NUTR)

comm_presence <- inner_join(env, comm_presence, by = 'site') %>%
  select(-TRANSECT, -LIGHT,  -TEMP,  -CONT, -MOIST, -REACT, -NUTR)

setwd('~/Dropbox/4teaching/MLU_teaching2018/data/')
save(vdat, vltava,  comm_presence, site_occupancy, file = 'vltava.Rdata')
##-------- linear model example: S ~ elevation--------------------
fig1 <- vdat %>%
  ggplot() +
  geom_point(aes(x = ELEVATION, y = S)) +
  stat_smooth(method = 'lm', se = F,
              aes(x = ELEVATION, y = S)) +
  ylab('Species richness') +
  xlab('Elevation (metres)') +
  theme_bw()
  
lm_eqn = expression(paste(italic(y)[i], ' = ', beta, italic(x)[i], ' + ', epsilon[i], sep = ''))
ggdraw(fig1) + draw_label(lm_eqn, x = 0.7, y = 0.8, size = 22)


vdat %>%
  ggplot() +
  geom_point(aes(x = pH, y = S)) + 
  stat_smooth(method = 'lm', se = F, aes(x = pH, y = S), lty = 2) +
  stat_smooth(aes(x = pH, y = S), se = F)

vdat %>%
  ggplot() +
  geom_point(aes(x = SOILDPT, y = S)) + 
  stat_smooth(method = 'lm', se = F, aes(x = SOILDPT, y = S), lty = 2) +
  stat_smooth(aes(x = SOILDPT, y = S), se = F)

vdat %>%
  ggplot() +
  geom_point(aes(x = ELEVATION, y = S)) + 
  stat_smooth(method = 'lm', se = F, aes(x = ELEVATION, y = S, group = LITHIC), lty = 1) +
  stat_smooth(method = 'lm', se = F, aes(x = ELEVATION, y = S), 
              formula = y ~ x + I(x^2), lty = 2) +
  stat_smooth(method = 'lm', se = F, aes(x = ELEVATION, y = S), 
              formula = y ~ x + I(x^2) + I(x^3), lty = 3) +
  stat_smooth(method = 'lm', se = F, aes(x = ELEVATION, y = S), 
              formula = y ~ x + I(x^2) + I(x^3) + I(x^4), lty = 4) +
  stat_smooth(aes(x = ELEVATION, y = S), se = F, colour = 'black')

vdat %>%
  ggplot() +
  geom_point(aes(x = ELEVATION, y = S, colour = factor(LITHIC))) + 
  stat_smooth(method = 'lm', se = F, aes(x = ELEVATION, y = S, group = LITHIC, colour = factor(LITHIC)), lty = 1) +
  # stat_smooth(method = 'gam', se = F, aes(x = ELEVATION, y = S), 
  #             formula = y ~ s(x, bs = 'cr'), lty = 2) + 
  # stat_smooth(method = 'gam', se = F, aes(x = ELEVATION, y = S), 
  #             formula = y ~ s(x, k = 4, bs = 'cr'), lty = 3) + 
  # stat_smooth(method = 'gam', se = F, aes(x = ELEVATION, y = S), 
  #             formula = y ~ s(x, k = 8, bs = 'cr'), lty = 4) +
  stat_smooth(method = 'gam', se = F, aes(x = ELEVATION, y = S, group = LITHIC, colour = factor(LITHIC)), 
            formula = y ~ s(x, bs = 'cs'), lty = 2)  +
  

  #---------things to think about when using multiple predictors--------------
# collinearity: visual inspection
source('~/Dropbox/3literature/Zuur_R_functions_AED.R')
Mypairs(vdat %>% select(ELEVATION:HEAT.LOAD, SOILDPT, pH))
