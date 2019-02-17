### Table 1
tab1 = hundat %>%
  group_by(Variable = contract) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())

tab1 = hundat %>%
  group_by(Variable = firmsize) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = sector) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = isco) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = urban) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = union) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = educ) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = hundat %>%
  group_by(Variable = female) %>%
  summarise('Mean East' = wtd.mean(hwage[region2 == 'East'], pb040[region2 == 'East']),
            'Mean West' = wtd.mean(hwage[region2 == 'West'], pb040[region2 == 'West']),
            'N' = n(),
            'N(East)/N' = length(hwage[region2 == 'East']) / n())  %>%
  bind_rows(tab1)

tab1 = kable(tab1, 
             'latex',
             caption = "Pooled Sample nach Datenfilterung und Stundenlohnunterschiede zwischen Westen und Osten", 
             booktabs = T,
             escape = F,
             col.names = c("Variable", linebreak(c("Mean hwage\nEast", "Mean hwage\nWest"), align = "r"), "N", "N(East)/N")) %>%
  kable_styling(font_size = 10, full_width = F) %>%
  group_rows("Sex", 1, 2) %>%
  group_rows("Education", 3, 5) %>%
  group_rows("Union", 6, 7) %>%
  group_rows("Urbanisation", 8, 9) %>%
  group_rows("ISCO", 10, 11) %>%
  group_rows("Sector", 12, 14) %>%
  group_rows("Firmsize", 15, 16) %>%
  group_rows("Contract", 17, 18) %>%
  footnote(general = 'EU-SILC 2005-2017, eigene Berechnungen.',
           footnote_as_chunk = T,
           general_title = "Quelle:")

### Table 2 

tab2 = indicators_p1 %>% melt(id = c('rb010', 'stat'))
tab2$rb010 = tab2$rb010
tab2 = tab2 %>% spread(rb010, value)
tab2$stat = factor(tab2$stat, levels = c('Mean', 
                                         'Q50', 
                                         'Gini', 
                                         'P80/P20', 
                                         'Top10'))
tab2$variable = factor(tab2$variable, 
                       levels = c('pretax_factor_eq', 
                                  'pretax_nation_eq', 
                                  'posttax_disp_eq'),
                       labels = c('Factor', 'Nation', 'Disposable'))
tab2 = tab2[order(tab2$stat, tab2$variable),]

tab2 = tab2 %>% 
  select(-stat) %>%
  kable('latex',
        booktabs = T,
        caption = "Ungleichheitsindikatoren nach Eurostat",
        row.names = FALSE,
        col.names = c("", c(2005:2017))) %>%
  kable_styling(latex_options = 'scale_down') %>%
  group_rows("Mean", 1, 3) %>%
  group_rows("Median", 4, 6) %>%
  group_rows("Gini", 7, 9) %>%
  group_rows("P80/P20", 10, 12) %>%
  group_rows("Top10", 13, 15) %>%
  footnote(general = 'EU-SILC 2005-2017, eigene Berechnungen.',
           footnote_as_chunk = T,
           general_title = "Quelle:") %>%
  landscape()

### Table 3 

tab3 = indicators_p2 %>% melt(id = c('pb010', 'stat'))
tab3$pb010 = tab3$pb010
tab3 = tab3 %>% spread(pb010, value)
tab3$stat = factor(tab3$stat, levels = c('Mean', 
                                         'Q50', 
                                         'Gini', 
                                         'P80/P20', 
                                         'Top10'))
tab3$variable = factor(tab3$variable, 
                       levels = c('pretax_factor_20', 
                                  'pretax_nation_20', 
                                  'posttax_disp_20'),
                       labels = c('Factor', 'Nation', 'Disposable'))
tab3 = tab3[order(tab3$stat, tab3$variable),]

tab3 = tab3 %>% 
  select(-stat) %>%
  kable('latex',
        booktabs = T,
        caption = "Ungleichheitsindikatoren nach WID World",
        row.names = FALSE,
        col.names = c("", c(2005:2017))) %>%
  kable_styling(latex_options = 'scale_down') %>%
  group_rows("Mean", 1, 3) %>%
  group_rows("Median", 4, 6) %>%
  group_rows("Gini", 7, 9) %>%
  group_rows("P80/P20", 10, 12) %>%
  group_rows("Top10", 13, 15) %>%
  footnote(general = 'EU-SILC 2005-2017, eigene Berechnungen.',
           footnote_as_chunk = T,
           general_title = "Quelle:") %>%
  landscape()

### Table 4
tab4 = results2
tab4$year = tab4$year
tab4$lower = round(tab4$lower, digits = 4)
tab4$upper = round(tab4$upper, digits = 4)
tab4$value = round(tab4$value, digits = 4)
tab4 = unite(tab4, CI, lower, upper, sep = "-")
tab4 = melt(tab4, id = c('year', 'stat'))
tab4 = spread(tab4, year, value)
tab4$variable = factor(tab4$variable, levels = c('value', 'CI'), labels = c('Estimate', 'CI'))

tab4 = tab4 %>%
  select(-stat) %>%
  kable('latex',
        booktabs = T,
        caption = "Dekomposition - Einfaches Model",
        row.names = FALSE,
        col.names = c("", c(2006:2017))) %>%
  kable_styling(latex_options = 'scale_down') %>%
  group_rows('Diffrence (D)', 1, 2) %>%
  group_rows('Endowment (X)', 3, 4) %>%
  group_rows('Price (P)', 5, 6) %>%
  footnote(general = 'EU-SILC 2005-2017, eigene Berechnungen.',
           footnote_as_chunk = T,
           general_title = "Quelle:") %>%
  landscape()

### Table 5
tab5 = results
tab5$year = tab5$year 
tab5$lower = round(tab5$lower, digits = 4)
tab5$upper = round(tab5$upper, digits = 4)
tab5$value = round(tab5$value, digits = 4)
tab5 = unite(tab5, CI, lower, upper, sep = "-")
tab5 = melt(tab5, id = c('year', 'stat'))
tab5 = spread(tab5, year, value)
tab5$variable = factor(tab5$variable, 
                       levels = c('value', 'CI'), 
                       labels = c('Estimate', 'CI'))

tab5 = tab5 %>%
  select(-stat) %>%
  kable('latex',
        booktabs = T,
        caption = "Dekomposition - Erweitertes Model",
        row.names = FALSE,
        col.names = c("", c(2006:2017))) %>%
  kable_styling(latex_options = 'scale_down') %>%
  group_rows('Diffrence (D)', 1, 2) %>%
  group_rows('Endowment (X)', 3, 4) %>%
  group_rows('Price (P)', 5, 6) %>%
  footnote(general = 'EU-SILC 2005-2017, eigene Berechnungen.',
           footnote_as_chunk = T,
           general_title = "Quelle:") %>%
  landscape()