# Inequality Indicators Hungary 2005 - 2017
# Authors: Azad, Zoeldi
# Description: calculate mean, median, gini, p80/p20, top10-share
# Output: dataframes index_p_p1, index_h_p1, index_p_p2, index_h_p2

library(survey)
library(convey)

### survey design
# Eurostat (P1)
svy_hun_p1 = svydesign(ids = ~idp,
                       strata = ~db040,
                       weights = ~rb050,
                       data = subset(hun_p1, pretax_factor_eq >= 0)) %>% 
  convey_prep()

# wid.world (P2)
svy_hun_p2 = svydesign(ids = ~idp,
                       strata = ~db040,
                       weights = ~pb040,
                       data = subset(hun_p2, pretax_factor_20 >= 0)) %>% 
  convey_prep()

################################################################################
###################### Idicators: Eurostat (P1) ################################
################################################################################

for (var in c('pretax_factor_eq', 'pretax_nation_eq', 'posttax_disp_eq')) {
  # Mean
stat_p = svyby(as.formula(paste("~", var)), 
               by = ~rb010, 
               svy_hun_p1, 
               svymean) %>%
  mutate(stat = "Mean") %>%
  select(rb010, var, stat)

# Median
for (y in c(2005:2017)) {
  temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                    subset(svy_hun_p1, 
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
                      svy_hun_p1, 
                      svygini, 
                      keep.names = F)) %>% 
  mutate(stat = 'Gini') %>%
  select(rb010, stat, var)

stat_p = bind_rows(stat_p, temp)

#P80/P20
if (var == 'pretax_factor_eq') {
  temp = data.frame(rb010 = as.numeric(c(2005:2017)), 
                    stat = c(rep('P80/P20', 13)), 
                    pretax_factor_eq = c(rep(NA, 13)))
} else {
  temp = svyby(as.formula(paste("~", var)), 
             by = ~rb010, 
             svy_hun_p1, 
             svyqsr,
             alpha1 = 0.22,
             alpha2 = (1-0.22),
             keep.names = F) %>% 
  mutate(stat= 'P80/P20') %>%
  select(rb010, stat, var)
}

stat_p = bind_rows(stat_p, temp)

# Top 10
for(y in c(2005:2017)) {
  sub = subset(svy_hun_p1, rb010==y)
  
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
  indicators_p1 = stat_p} else{
    indicators_p1 = stat_p %>%
      right_join(indicators_p1, by = c('stat', 'rb010'))
  }
}


################################################################################
###################### Idicators: wid.world (P2) ###############################
################################################################################

for (var in c('pretax_factor_20', 'pretax_nation_20', 'posttax_disp_20')) {
  # Mean
  stat_p = svyby(as.formula(paste("~", var)), 
                 by = ~pb010, 
                 svy_hun_p2, 
                 svymean) %>%
    mutate(stat = "Mean") %>%
    select(pb010, var, stat)
  
  # Median
  for (y in c(2005:2017)) {
    temp= data_frame(as.numeric(svyquantile(as.formula(paste("~", var)), 
                                            subset(svy_hun_p2, 
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
                        svy_hun_p2, 
                        svygini, 
                        keep.names = F)) %>% 
    mutate(stat = 'Gini') %>%
    select(pb010, stat, var)
  
  stat_p = bind_rows(stat_p, temp)
  
  #P80/P20
  if (var == 'pretax_factor_20') {
    temp = data.frame(pb010 = as.numeric(c(2005:2017)), 
                            stat = c(rep('P80/P20', 13)), 
                            pretax_factor_20 = c(rep(NA, 13)))
  } else {
  temp = svyby(as.formula(paste("~", var)), 
               by = ~pb010, 
               svy_hun_p2, 
               svyqsr, 
               alpha1 = 0.20, 
               alpha2 = (1-0.20), 
               keep.names = F) %>% 
    mutate(stat= 'P80/P20') %>%
    select(pb010, stat, var)
  }
  
  stat_p = bind_rows(stat_p, temp)
  
  # Top 10
  for(y in c(2005:2017)) {
    sub = subset(svy_hun_p2, pb010==y)
    
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
    indicators_p2 = stat_p} else{
      indicators_p2 = stat_p %>%
        right_join(indicators_p2, by = c('stat', 'pb010'))
    }
}

rm(stat_p, sub, temp, var, y)

################################################################################
################################################################################
################################################################################

descript = hundat %>% 
  group_by(pb010) %>% 
  summarise(mean = wtd.mean(log(hwage), weights = pb040),
            q25 = wtd.quantile(hwage, weights = pb040, probs = 0.25),
            q50 = wtd.quantile(hwage, weights = pb040, probs = 0.50),
            q75 = wtd.quantile(hwage, weights = pb040, probs = 0.75))

descript = hundat %>% 
  filter(region2 == 'East') %>% 
  group_by(pb010) %>%
  summarise(mean_E = wtd.mean(log(hwage), weights = pb040),
            q25_E = wtd.quantile(log(hwage), weights = pb040, probs = 0.25),
            q50_E = wtd.quantile(log(hwage), weights = pb040, probs = 0.50),
            q75_E = wtd.quantile(log(hwage), weights = pb040, probs = 0.75)) %>%
  left_join(descript, by='pb010')

descript = hundat %>% 
  filter(region2 == 'West') %>% 
  group_by(pb010) %>%
  summarise(mean_W = wtd.mean(log(hwage), weights = pb040),
            q25_W = wtd.quantile(log(hwage), weights = pb040, probs = 0.25),
            q50_W = wtd.quantile(log(hwage), weights = pb040, probs = 0.50),
            q75_W = wtd.quantile(log(hwage), weights = pb040, probs = 0.75)) %>%
  left_join(descript, by='pb010')

descript = descript %>% mutate(dmean = mean_W - mean_E,
                               dq25 = q25_W - q25_E,
                               dq50 = q50_W - q50_E,
                               dq75 = q75_W - q75_E)


#descript = melt(descript, id = 'pb010')


#ggplot(data = descript %>% filter(variable %in% c('dmean', 'dq25', 'dq50', 'dq75')), 
#aes(x = pb010, y = value, colour = variable)) + 
#geom_line()
