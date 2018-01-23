# code to do some visual inspection and analysis of bird data from ade4 package
rm(list=ls())

# mobr
library(mobr)
library(ade4)

# bird data
load('')

# fix rownames of community matrix (to avoid warning)
rownames(aviurba$fau) <- 1:nrow(aviurba$fau)
# create mob-in object
bird_mob_in <- make_mob_in(aviurba$fau, environ)

# visual inspection of rarefaction curves
plot_rarefaction(bird_mob_in, 'heterogeneity', '0', pooled = F, method = 'indiv')
plot_rarefaction(bird_mob_in, 'heterogeneity', '0', pooled = T, method = 'indiv')

# visual inspection of rarefaction curves
plot_rarefaction(bird_mob_in, 'simple_habitat', 'unknown', pooled = F, method = 'indiv')
plot_rarefaction(bird_mob_in, 'simple_habitat', 'unknown', pooled = T, method = 'indiv')

# visual inspection of rarefaction curves
plot_rarefaction(bird_mob_in, 'noise', 'no', pooled = F, method = 'indiv')
plot_rarefaction(bird_mob_in, 'noise', 'no', pooled = T, method = 'indiv')

# discrete analyses
birds_discrete <- get_mob_stats(bird_mob_in, 'heterogeneity')
plot(birds_discrete, multi_panel = T)

birds_discrete2 <- get_mob_stats(bird_mob_in, 'simple_habitat')
plot(birds_discrete2, multi_panel = T)

birds_discrete3 <- get_mob_stats(bird_mob_in, 'noise')
plot(birds_discrete3, multi_panel = T)

# continuous analyses
birds_contin <- get_delta_stats(bird_mob_in, 'simple_habitat', ref_group = 'mixed')
plot(birds_contin, trt_group = 'unknown', ref_group = 'mixed')
plot(birds_contin, trt_group = 'scrub', ref_group = 'mixed')
plot(birds_contin, trt_group = 'tree', ref_group = 'mixed')

birds_contin_noise <- get_delta_stats(bird_mob_in, 'noise', ref_group = 'no')
plot(birds_contin_noise, trt_group = 'yes', ref_group = 'no')
overlap_effects(birds_contin_noise, trt_group = 'yes')
