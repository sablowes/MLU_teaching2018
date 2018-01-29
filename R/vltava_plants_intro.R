# code to explore Vltava river valley vegetation data: intro to GLM, GAM, GLMM...

rm(list=ls())
library(tidyverse)

load (url ('http://www.davidzeleny.net/anadat-r/lib/exe/fetch.php/data:vltava.r'))
str(vltava)

# separate into site x species and environment dataframes
env <- vltava$env %>% as_tibble()
pairs(env)


comm <- vltava$spe %>% as_tibble()
