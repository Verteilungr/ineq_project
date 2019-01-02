# Data Preparation Hungary
# Authors: Azad, Zoeldi
# Description: select variables in every years and merge to our local dataframe
# Output: dataframes 'hun_{p,h,d,r}'


### read script for connection: password needed
source('R/_connection.R')

### the whole script is a loop 
for (year in c('04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14',
               '15', '16', '17')){
  
  ### company car till 2006 as py020g then as py021g
  if (year %in% c('04', '05', '06')) {
    companycar = 'py020g'
  } else {
    companycar = 'py021g'
  }
  
  ### numbe of month in work changed in 2009
  if (year %in% c('04', '05', '06', '07', '08')) {
    month = c('pl070', 'pl072')
  } else {
    month = c('pl073', 'pl074')
  }
  
  ### isco code changed in 2011
  if (year %in% c('04', '05', '06', '07', '08', '09', '10')) {
    isco = 'pl050'
  } else {
    isco = 'pl051'
  }
  
  ### nace code changed in 2008
  if (year %in% c('04', '05', '06', '07')) {
    nace = 'pl110'
  } else {
    nace = 'pl111'
  }
  ### load data for HUN in every year 
  silc_p = tbl(pg, paste0('c', year, 'p')) %>% 
    filter(pb020 == 'HU') %>%
    select(pb010, pb020, px030, pb030, pb040, pb140,
           # year, country, household ID, personal ID, pweight, birthdate
           py010g, companycar, py050g,
           # employee cash, company car, self-employed cash
           py080g, py100g,
           # old-age benefit, private pension
           py090g, py110g, py120g, py130g, py140g,
           #unemployment, survivor, sickness, disability, education
           pb150, pb200, pe040, pe030,
           # sex, consensual union, education, education left
           pl200, pl190, month, pl060, pl100, isco, nace, pl130, pl140
           # years of exper, years since first job, month, hours first, hours
           # second, isco, nace, firm size, contract type
           ) %>%
    collect(n=Inf)
  
  ### load household data for HUN in every year
  silc_h <- tbl(pg, paste0('c', year, 'h')) %>%
    filter(hb020 == 'HU') %>%
    select(hb010, hb020, hb030,
           # year, country, household ID
           hy040g, 
           # property (2)
           hy010, hy020, hx090,
           # allinone (1:5), postax (1:6), equi disposable 
           hy050g, hy060g, hy070g, hy080g, hy090g, hy110g,
           # family allowance, social else, housing allowance, interhh transfer,
           # capital income
           # income from member less 16 (5/2)
           hy120g, hy130g, hy140g,
           # tax wealth, interhh payment, tax on
           # income and social controbution
           hx050
           # equi hh size
           ) %>%
    collect(n=Inf)
  
  ### load household register for HUN in every year
  silc_d <- tbl(pg, paste0('c', year, 'd')) %>%
    select(db010, db020, db030, db040, db090, db100) %>%
    filter(db020 == 'HU') %>% 
    collect(n=Inf)
  
  ### load personal register for HUN in every year
  silc_r <- tbl(pg, paste0('c', year, 'r')) %>%
    select(rb010, rb020, rb030, rb050, rb080, rx030) %>%
    filter(rb020 == 'HU') %>% 
    collect(n=Inf)
 
  
  ### create dataframes that we use for the analysis
  if (year == '04') {
    hun_p = silc_p
    hun_h = silc_h
    hun_d = silc_d
    hun_r = silc_r
  } else {
    hun_p = bind_rows(silc_p, hun_p)
    hun_h = bind_rows(silc_h, hun_h)
    hun_d = bind_rows(silc_d, hun_d)
    hun_r = bind_rows(silc_r, hun_r)
  }
  
  ### Message
  print(paste("SILC", year, "for Hungary is ready"))
  if (year== '17') {
    print("READYYY!!!")
  }
  
}

rm(list=setdiff(ls(), c('hun_p', 'hun_h', 'hun_d', 'hun_r')))

#save(hun_p, file = 'C:/Users/zoeldi/Desktop/Data/SILC/todelete/hun_p.RData')
#save(hun_r, file = 'C:/Users/zoeldi/Desktop/Data/SILC/todelete/hun_r.RData')
#save(hun_d, file = 'C:/Users/zoeldi/Desktop/Data/SILC/todelete/hun_d.RData')
#save(hun_h, file = 'C:/Users/zoeldi/Desktop/Data/SILC/todelete/hun_h.RData')
