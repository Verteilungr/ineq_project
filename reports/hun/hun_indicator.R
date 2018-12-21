# Inequality Indicators Hungary 2005 - 2017
# Authors: Azad, Zoeldi
# Description: calculate mean, median, gini, p80/p20, top10-share
# Output: dataframes index_p_p1, index_h_p1, index_p_p2, index_h_p2

library(survey)
library(convey)

### survey design
# Individual level (P1)
svy_hun_rh = svydesign(ids = ~idp,
                       strata = ~db040,
                       weights = ~rb050,
                       data = hun_rh) %>% 
  convey_prep()

# Individual level (P2)
svy_hun_ph = svydesign(ids = ~idp,
                       strata = ~db040,
                       weights = ~pb040,
                       data = hun_ph[hun_ph$age_20==1,]) %>%
  convey_prep()

# Houshold level (P1 and P2)
svy_hun_dhp = svydesign(ids = ~id,
                        strata = ~db040,
                        weights = ~db090,
                        data = hun_dhp) %>%
  convey_prep()

################################################################################
###################### Idicators: Individual level (P1) ########################
################################################################################

for (var in c('pretax_factor_eq', 'pretax_nation_eq', 'posttax_disp_eq')) {
  # Mean
stat_p = svyby(as.formula(paste("~", var)), 
               by = ~rb010, 
               svy_hun_rh, 
               svymean) %>%
  mutate(stat = "Mean") %>%
  select(rb010, var, stat)

# Median
for (y in c(2005:2013)) {
  temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                    subset(svy_hun_rh, 
                           rb010 == y), 
                    quantile = 0.5, 
                    se = T)))
  
  names(temp) <- var
  
  temp = temp %>% 
    mutate(rb010 = y, 
           stat = 'Q50') %>%
    select(rb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
}

# Gini
temp=data.frame(svyby(as.formula(paste("~", var)), 
                      by = ~rb010, 
                      svy_hun_rh, 
                      svygini, 
                      keep.names = F)) %>% 
  mutate(stat = 'Gini') %>%
  select(rb010, stat, var)

stat_p = bind_rows(stat_p, temp)

#P80/P20
temp = svyby(as.formula(paste("~", var)), 
             by = ~rb010, 
             svy_hun_rh, 
             svyqsr, 
             alpha1 = 0.2, 
             alpha2 = (1-0.2), 
             keep.names = F) %>% 
  mutate(stat= 'P80/P20') %>%
  select(rb010, stat, var)

stat_p = bind_rows(stat_p, temp)

# Top 10
for(y in c(2005:2013)) {
  sub = subset(svy_hun_rh, rb010==y)
  
  temp = svytotal(as.formula(paste("~", var)), 
                  subset(sub, 
                         get(var) >= 
                           as.numeric(svyquantile(as.formula(paste("~", var)), 
                                                  sub, quantile=c(0.9))))) / 
    svytotal(as.formula(paste("~", var)), sub)
  
  temp = data.frame(as.numeric(temp))
  
  names(temp) <- var
    
  temp = temp %>% mutate(rb010 = y, stat='Top10')
  
  stat_p = bind_rows(stat_p, temp)
}

if (var=='pretax_factor_eq') {
  indicators_p_p1 = stat_p} else{
    indicators_p_p1 = stat_p %>%
      right_join(indicators_p_p1, by = c('stat', 'rb010'))
  }
}

################################################################################
###################### Idicators: Household level (P1) ########################
################################################################################

for (var in c('pretax_factor_eq', 'pretax_nation_eq', 'posttax_disp_eq')) {
  # Mean
  stat_p = svyby(as.formula(paste("~", var)), 
                 by = ~hb010, 
                 svy_hun_dhp, 
                 svymean) %>%
    mutate(stat = "Mean") %>%
    select(hb010, var, stat)
  
  # Median
  for (y in c(2005:2013)) {
    temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                                            subset(svy_hun_dhp, 
                                                   hb010 == y), 
                                            quantile = 0.5, 
                                            se = T)))
    
    names(temp) <- var
    
    temp = temp %>% 
      mutate(hb010 = y, 
             stat = 'Q50') %>%
      select(hb010, stat, var)
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  # Gini
  temp=data.frame(svyby(as.formula(paste("~", var)), 
                        by = ~hb010, 
                        svy_hun_dhp, 
                        svygini, 
                        keep.names = F)) %>% 
    mutate(stat = 'Gini') %>%
    select(hb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  #P80/P20
  temp = svyby(as.formula(paste("~", var)), 
               by = ~hb010, 
               svy_hun_dhp, 
               svyqsr, 
               alpha1 = 0.2, 
               alpha2 = (1-0.2), 
               keep.names = F) %>% 
    mutate(stat= 'P80/P20') %>%
    select(hb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  # Top 10
  for(y in c(2005:2013)) {
    sub = subset(svy_hun_dhp, hb010==y)
    
    temp = svytotal(as.formula(paste("~", var)), 
                    subset(sub, get(var) >= 
                             as.numeric(svyquantile(as.formula(paste("~", var)), 
                                                    sub, quantile=c(0.9))))) / 
      svytotal(as.formula(paste("~", var)), sub)
    
    temp = data.frame(as.numeric(temp))
    
    names(temp) <- var
    
    temp = temp %>% mutate(hb010 = y, stat='Top10')
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  if (var=='pretax_factor_eq') {
    indicators_h_p1 = stat_p} else{
      indicators_h_p1 = stat_p %>%
        right_join(indicators_h_p1, by = c('stat', 'hb010'))
    }
}

################################################################################
###################### Idicators: Individual level (P2) ########################
################################################################################

for (var in c('pretax_factor_20', 'pretax_nation_20', 'posttax_disp_20')) {
  # Mean
  stat_p = svyby(as.formula(paste("~", var)), 
                 by = ~pb010, 
                 svy_hun_ph, 
                 svymean) %>%
    mutate(stat = "Mean") %>%
    select(pb010, var, stat)
  
  # Median
  for (y in c(2005:2013)) {
    temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                                            subset(svy_hun_ph, 
                                                   pb010 == y), 
                                            quantile = 0.5, 
                                            se = T)))
    
    names(temp) <- var
    
    temp = temp %>% 
      mutate(pb010 = y, 
             stat = 'Q50') %>%
      select(pb010, stat, var)
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  # Gini
  temp=data.frame(svyby(as.formula(paste("~", var)), 
                        by = ~pb010, 
                        svy_hun_ph, 
                        svygini, 
                        keep.names = F)) %>% 
    mutate(stat = 'Gini') %>%
    select(pb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  #P80/P20
  temp = svyby(as.formula(paste("~", var)), 
               by = ~pb010, 
               svy_hun_ph, 
               svyqsr, 
               alpha1 = 0.2, 
               alpha2 = (1-0.2), 
               keep.names = F) %>% 
    mutate(stat= 'P80/P20') %>%
    select(pb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  # Top 10
  for(y in c(2005:2013)) {
    sub = subset(svy_hun_ph, pb010==y)
    
    temp = svytotal(as.formula(paste("~", var)), 
                    subset(sub, get(var) >= 
                             as.numeric(svyquantile(as.formula(paste("~", var)), 
                                                    sub, quantile=c(0.9))))) / 
      svytotal(as.formula(paste("~", var)), sub)
    
    temp = data.frame(as.numeric(temp))
    
    names(temp) <- var
    
    temp = temp %>% mutate(pb010 = y, stat='Top10')
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  if (var=='pretax_factor_20') {
    indicators_p_p2 = stat_p} else{
      indicators_p_p2 = stat_p %>%
        right_join(indicators_p_p2, by = c('stat', 'pb010'))
    }
}

################################################################################
###################### Idicators: Household level (P2) ########################
################################################################################

for (var in c('pretax_factor_20_hh', 
              'pretax_nation_20_hh', 
              'posttax_disp_20_hh')) {
  # Mean
  stat_p = svyby(as.formula(paste("~", var)), 
                 by = ~hb010, 
                 svy_hun_dhp, 
                 svymean) %>%
    mutate(stat = "Mean") %>%
    select(hb010, var, stat)
  
  # Median
  for (y in c(2005:2013)) {
    temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                                            subset(svy_hun_dhp, 
                                                   hb010 == y), 
                                            quantile = 0.5, 
                                            se = T)))
    
    names(temp) <- var
    
    temp = temp %>% 
      mutate(hb010 = y, 
             stat = 'Q50') %>%
      select(hb010, stat, var)
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  # Gini
  temp=data.frame(svyby(as.formula(paste("~", var)), 
                        by = ~hb010, 
                        svy_hun_dhp, 
                        svygini, 
                        keep.names = F)) %>% 
    mutate(stat = 'Gini') %>%
    select(hb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  #P80/P20
  temp = svyby(as.formula(paste("~", var)), 
               by = ~hb010, 
               svy_hun_dhp, 
               svyqsr, 
               alpha1 = 0.2, 
               alpha2 = (1-0.2), 
               keep.names = F) %>% 
    mutate(stat= 'P80/P20') %>%
    select(hb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  # Top 10
  for(y in c(2005:2013)) {
    sub = subset(svy_hun_dhp, hb010==y)
    
    temp = svytotal(as.formula(paste("~", var)), 
                    subset(sub, get(var) >= 
                             as.numeric(svyquantile(as.formula(paste("~", var)), 
                                                    sub, quantile=c(0.9))))) / 
      svytotal(as.formula(paste("~", var)), sub)
    
    temp = data.frame(as.numeric(temp))
    
    names(temp) <- var
    
    temp = temp %>% mutate(hb010 = y, stat='Top10')
    
    stat_p = bind_rows(stat_p, temp)
  }
  
  if (var=='pretax_factor_20_hh') {
    indicators_h_p2 = stat_p} else{
      indicators_h_p2 = stat_p %>%
        right_join(indicators_h_p2, by = c('stat', 'hb010'))
    }
}

rm(stat_p, sub, temp, var, y)
