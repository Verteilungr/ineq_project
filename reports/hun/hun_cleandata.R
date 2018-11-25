# Data Cleaning Hungary
# Authors: Azad, Zoeldi
# Description: create income variables according to several concepts, label 
# factor variables 
# Output: Cleaned Dataframes "hun_p" and "hun_h" according to income concepts



### read raw data for 2005-2013 (see also script)
source('reports/hun/hun_prepdata.R')

########################
### Individual level ###
########################

### Age (age)
hun_p$age = hun_p$pb010 - hun_p$pb140

### Age >=20 Dummy (age_20)
hun_p$age_20 = 0
hun_p$age_20[hun_p$age >= 20] = 1

### Number obs. over 20 per hh (hhm)
hun_p = hun_p %>% group_by(id_h) %>% mutate(hhm = sum(age_20))

### Set NA to 0 in income variables
varinc_p = c('py010g', 'py050g', 'py080g', 'py100g', 'py090g', 'py110g',
             'py120g', 'py130g', 'py140g', 'py035g', 'py030g', 'py031g',
             'hy040g', 'hy170g', 'hy010', 'hy020', 'hx090', 'hy050g', 'hy060g',
             'hy070g', 'hy080g', 'hy081g', 'hy090g', 'hy100g', 'hy110g',
             'hy120g', 'hy130g', 'hy131g', 'hy140g')

hun_p[varinc_p][is.na(hun_p[varinc_p])] <- 0

### Income aggregates
hun_p = hun_p %>% mutate(inc_labor = py010g + py050g,
                         inc_property = hy040g / hhm,
                         inc_ownprod = hy170g / hhm,
                         inc_pension = py080g + py100g,
                         inc_other = py090g + py110g + py120g + 
                           py130g + py140g + (hy050g + hy060g +
                           hy070g + hy080g + hy081g + hy090g + 
                           hy100g + hy110g) / hhm,
                         inc_tax = (hy120g + hy140g) / hhm)

### Income concepts
hun_p = hun_p %>% mutate(pretax_factor = inc_labor + inc_property + inc_ownprod,
                         pretax_nation = pretax_factor + inc_pension,
                         postax_dispos = pretax_nation - inc_tax)

### Filter obs. with postive income over 20 
hun_p_factor = hun_p %>% filter(age >= 20 & pretax_factor > 0)
hun_p_nation = hun_p %>% filter(age >= 20 & pretax_nation > 0)
hun_p_dispos = hun_p %>% filter(age >= 20 & postax_dispos > 0)

#######################
### Household level ###
#######################

### Set NA to 0 in income variables
varinc_h = c('py010g_hh', 'py050g_hh', 'py080g_hh', 'py100g_hh', 'hy040g',
             'hy170g', 'hy010', 'hy020', 'hx090', 'hy050g', 'hy060g', 'hy070g', 
             'hy080g', 'hy081g', 'hy090g', 'hy100g', 'hy110g', 'hy120g', 
             'hy130g', 'hy131g', 'hy140g')

hun_h[varinc_h][is.na(hun_h[varinc_h])] <- 0

### Income aggregates 
hun_h = hun_h %>% mutate(inc_labor = py010g_hh + py050g_hh,
                         inc_property = hy040g,
                         inc_ownprod = hy170g,
                         inc_pension = py080g_hh + py100g_hh)

### Income concepts
hun_h = hun_h %>% mutate(pretax_factor = inc_labor + inc_property + inc_ownprod,
                         pretax_nation = pretax_factor + inc_pension,
                         postax_dispos = hy020)

### Filter obs. with positiv income
hun_h_factor = hun_h %>% filter(pretax_factor > 0)
hun_h_nation = hun_h %>% filter(pretax_nation > 0)
hun_h_dispos = hun_h %>% filter(postax_dispos > 0)

