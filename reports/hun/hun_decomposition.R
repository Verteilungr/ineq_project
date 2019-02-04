set.seed(1352408)
hundat$region3 = NA
hundat$region3[hundat$region2 == 'West'] = 0
hundat$region3[hundat$region2 == 'East'] = 1

hundat2 = hundat %>% select(-region, -region2, -idp, -idh)
hundat2 = hundat2 %>% mutate(expersq = exper^2)
hundat2 = data.frame(hundat2)

### Function for decomposition
fundec <- function(formula = formula, data, year, indices) {
  d <- data[indices,]
  
  d0 <- d %>% filter(region3 == 0 & pb010 == year)
  d0 <- dummy.data.frame(d0, 
                         sep='_', 
                         names = c('female', 
                                   'educ', 
                                   'union', 
                                   'urban', 
                                   'isco', 
                                   'sector', 
                                   'firmsize', 
                                   'contract'))
  
  d1 <- d %>% filter(region3 == 1 & pb010 == year)
  d1 <- dummy.data.frame(d1, 
                         sep='_', 
                         names = c('female', 
                                   'educ', 
                                   'union', 
                                   'urban', 
                                   'isco', 
                                   'sector', 
                                   'firmsize', 
                                   'contract'))
  #Mean
  mean0 = data.frame(rbind(colWeightedMeans(as.matrix(d0), w=d0$pb040)))
  mean1 = data.frame(rbind(colWeightedMeans(as.matrix(d1), w=d1$pb040)))
  
  #OLS
  lm0 <- lm(formula, data= d0, weights = pb040)
  lm1 <- lm(formula, data= d1, weights = pb040)
  
  #Predicted
  wage00 = predict(lm0, newdata = mean0)
  wage11 = predict(lm1, newdata = mean1)
  
  #Counterfactual
  wage10 = predict(lm1, newdata = mean0)
  wage01 = predict(lm0, newdata = mean1)
  
  #Dekomposition
  diff = weighted.mean(log(d0$hwage), d0$pb040) - 
    weighted.mean(log(d1$hwage), d1$pb040)
  x = wage00 - wage01
  p = wage01 - wage11

    return(rbind(diff, x, p)) 
} 

### Extended model with replicates
for (y in c(2006:2017)) {
  bs = boot(formula = log(hwage) ~ exper + 
            expersq + 
            educ_High + 
            educ_Middle + 
            isco_Skilled + 
            female_Female +
            sector_PUB +
            sector_SERV +
            firmsize_Big +
            contract_Permanent + 
            urban_Densely +
            union_Couple, data = hundat2, year = y, statistic = fundec, R=100)
  
  bsci_diff = boot.ci(bs, type = 'norm', index = 1)
  bsci_x = boot.ci(bs, type =  'norm', index = 2)
  bsci_p = boot.ci(bs, type = 'norm', index = 3)
  
  temp = data.frame(year = y,
                    stat = c('Difference (D)', 'Endowment (X)', 'Price (P)'),
                    value = c(bsci_diff$t0, 
                             bsci_x$t0, 
                             bsci_p$t0),
                    lower = c(as.numeric(bsci_diff$normal[2]), 
                              as.numeric(bsci_x$normal[2]), 
                              as.numeric(bsci_p$normal[2])),
                    upper = c(as.numeric(bsci_diff$normal[3]), 
                              as.numeric(bsci_x$normal[3]), 
                              as.numeric(bsci_p$normal[3])))
  if (y == '2006') {
    results = temp
  } else {
    results = bind_rows(results, temp)
  }
}

### Restricted model with replicates
for (y in c(2006:2017)) {
  bs = boot(formula = log(hwage) ~ exper + 
              expersq + 
              educ_High + 
              educ_Middle +
              female_Female +
              union_Couple, data = hundat2, year = y, statistic = fundec, R=100)
  
  bsci_diff = boot.ci(bs, type = 'norm', index = 1)
  bsci_x = boot.ci(bs, type =  'norm', index = 2)
  bsci_p = boot.ci(bs, type = 'norm', index = 3)
  
  temp = data.frame(year = y,
                    stat = c('Difference (D)', 'Endowment (X)', 'Price (P)'),
                    value = c(bsci_diff$t0, 
                              bsci_x$t0, 
                              bsci_p$t0),
                    lower = c(as.numeric(bsci_diff$normal[2]), 
                              as.numeric(bsci_x$normal[2]), 
                              as.numeric(bsci_p$normal[2])),
                    upper = c(as.numeric(bsci_diff$normal[3]), 
                              as.numeric(bsci_x$normal[3]), 
                              as.numeric(bsci_p$normal[3])))
  if (y == '2006') {
    results2 = temp
  } else {
    results2 = bind_rows(results2, temp)
  }
}
  
  
  
  
  
