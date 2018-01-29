# code to do some visual inspection and analysis of bird data from ade4 package
rm(list=ls())

# mobr
library(mobr)
library(tidyverse)

# bird data
load(url('https://github.com/sablowes/MLU_teaching2018/raw/master/data/birdData.Rdata'))

# some exploratory visualisations of the data...

# how are the different covariates distributed in space?

birds_long %>%
  ggplot() +
  geom_text(aes(x = x, y = y, label = site, colour = noise))

## looks like something is going on for noise...calculate some summary stats
noise_summary <- birds_long %>%
  # remove unobserved species
  filter(abundance > 0) %>%
  group_by(site) %>%
  summarise(
    N = sum(abundance),
    S = n_distinct(species),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    noise = unique(noise)) %>%
  ungroup()


noise_summary %>%
  ggplot() +
  #  facet_wrap(~feed.hab, scales = 'free') +
  geom_boxplot(aes(x = noise, y = N)) +
  theme_bw()

noise_summary %>%
  ggplot() +
  #  facet_wrap(~feed.hab, scales = 'free') +
  geom_boxplot(aes(x = noise, y = S)) +
  theme_bw()

noise_summary %>%
  ggplot() +
  #  facet_wrap(~feed.hab, scales = 'free') +
  geom_boxplot(aes(x = noise, y = ENSPIE)) +
  theme_bw()

# what about traits x noise?
noise_traits_summary <- birds_long %>%
  # remove unobserved species
  filter(abundance > 0) %>%
  group_by(site) %>%
  summarise(
    N = sum(abundance),
    S = n_distinct(species),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    noise = unique(noise)) %>%
  ungroup()

# what traits do we have
birds_long %>% distinct(feed.strat)
# simple habitat
simpHat_summary <- birds_long %>%
  filter(abundance > 0) %>%
  group_by(simple_habitat) %>%
  summarise(
    N_plots = n_distinct(site),
    N = sum(abundance),
    S = n_distinct(species),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    # noise = unique(noise)
    ) %>%
  ungroup()

simpHat_summary %>%
  ggplot() +
  #  facet_wrap(~noise, scales = 'free') +
  geom_boxplot(aes(x = simple_habitat, y = N))

simpHat_summary %>%
  ggplot() +
  #  facet_wrap(~noise, scales = 'free') +
  geom_boxplot(aes(x = simple_habitat, y = S))


# simple habitat x trait
simpHat_trait_summary <- birds_long %>%
  filter(abundance > 0) %>%
  group_by(site, simple_habitat, feed.hab, feed.strat) %>%
  summarise(
    N = sum(abundance),
    S = n_distinct(species),
    ENSPIE = vegan::diversity(abundance, index = 'invsimpson'),
    noise = unique(noise)) %>%
  ungroup()

simpHat_trait_summary %>%
  ggplot() +
#  facet_wrap(~noise, scales = 'free') +
  geom_boxplot(aes(x = interaction(simple_habitat, feed.hab), y = N))

simpHat_trait_summary %>%
  ggplot() +
  #  facet_wrap(~noise, scales = 'free') +
  geom_boxplot(aes(x = interaction(simple_habitat, feed.hab), y = S))


# or, prepare for mobr analyses...
# fix rownames of community matrix (to avoid warning)
rownames(aviurba$fau) <- 1:nrow(aviurba$fau)
# create mob-in object
bird_mob_in <- make_mob_in(aviurba$fau, environ)

