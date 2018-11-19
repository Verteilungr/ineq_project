# Data Preparation Hungary
# Authors: Azad, Zoeldi
# Description: Connection to WU Database, creat variables, output for analysis 
# in hun_report.Rmd. We loop the process along the EU-SILC because many
# variables are not avalilable for the full time-frame or have changed.
# Output: Dataframes "hun_p" and "hun_h"

library(doBy)

### read script for connection: password needed
source('R/_connection.R')

### the whole script is a loop 
for (year in c('05', '06', '07', '08', '09', '10', '11', '12', '13')){
  
  ### soc. contrbution variables first in 2007
  if (year %in% c('05', '06')) {
    soccontrib = NULL
  } else {
    soccontrib = c('py030g', 'py031g')
  }
  
  ### hh production first in 2010:
  if (year %in% c('05', '06', '07', '08', '09')) {
    hhprod = NULL
  } else {
    hhprod = 'hy170g'
  }
  
  ### alimonies first in 2007
  if (year %in% c('05', '06')) {
    alimreceive = NULL
  } else {
    alimreceive = 'hy081g'
  }
  
  if (year %in% c('05', '06')) {
    alimpaid = NULL
  } else {
    alimpaid = 'hy131g'
  }
  
  ### load data for HUN in every year 
  silc.p = tbl(pg, paste0('c', year, 'p')) %>% 
    filter(pb020 == 'HU') %>%
    select(pb010, pb020, px030, pb030, pb040, pb140,
           # year, country, household ID, personal ID, pweight, birthdate
           py010g, py050g,
           #labour income (1)
           py080g, py100g,
           # old-age benefit, private pension (4)
           py090g, py110g, py120g, py130g, py140g,
           #unemployment, survivor, sickness, disability, education (5/1)
           soccontrib, py035g
           # employers social contribution, same optional, contribution private
           # pension (6/1)
           ) %>%
    collect(n=Inf)
  
  ### load household data for HUN in every year
  silc.h <- tbl(pg, paste0('c', year, 'h')) %>%
    filter(hb020 == 'HU') %>%
    select(hb010, hb020, hb030,
           # year, country, household ID
           hy040g, hhprod, 
           # property (2), own produdction (drom 2010) (3)
           hy010, hy020, hx090,
           # allinone (1:5), postax (1:6), equi disposable 
           hy050g, hy060g, hy070g, hy080g, alimreceive, hy090g, hy100g, hy110g,
           # family allowance, social else, housing allowance, interhh transfer,
           # alimonies (from 2007), capital income, repayment on mortgages,
           # income from member less 16 (5/2)
           hy120g, hy130g, alimpaid, hy140g,
           # tax wealth, interhh payment, alimonies payed (from 2007), tax on
           # income and social controbution (6/2)
           hx050
           # equi hh size
           ) %>%
    collect(n=Inf)
  
  ### load household register for HUN in every year
  silc.d <- tbl(pg, paste0('c', year, 'd')) %>%
    filter(db020 == 'HU') %>%
    select(db010, db020, db030, db040, db090
           #year, country, household ID, region, hweight
           ) %>%
    collect(n=Inf)
  
  ### uniqe hh-ID
  silc.p = silc.p %>% mutate(id_h = paste0(pb010, px030))
  silc.h = silc.h %>% mutate(id_h = paste0(hb010, hb030))
  silc.d = silc.d %>% mutate(id_h = paste0(db010, db030))
  
  ### bindig personal and hh observations
  silc.hd = left_join(silc.d, silc.h, by = 'id_h')
  silc.phd = left_join(silc.hd, silc.p, by = 'id_h')
  
  ### collapse personal data by hh and bind to hh data
  silc.hdp = silc.p %>% 
    group_by(id_h) %>% 
    summarise(py010g_hh = sum(py010g),
              py050g_hh = sum(py050g),
              py080g_hh = sum(py080g),
              py100g_hh = sum(py100g)) %>%
    left_join(silc.hd, by = 'id_h')
  
  ### create dataframes that we use for the analysis
  if (year == '05') {
    hun_p = silc.phd
    hun_h = silc.hdp
  } else {
    hun_p = bind_rows(silc.phd, hun_p)
    hun_h = bind_rows(silc.hdp, hun_h)
  }
  
  #Message
  print(paste("SILC", year, "for Hungary is ready"))
  if (year== '13') {
    print("READYYY!!!")
  }
  
}

rm(list=setdiff(ls(), c('hun_h', 'hun_p')))
