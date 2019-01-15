### Abbildung 1: Ungleichheitsindikatoren nach der Eurostat-Berechnung (P1)

indicators_p1_2 = melt(indicators_p1, id.vars =  c('rb010', 'stat'))
indicators_p1_2$var = NA
indicators_p1_2$var[
  indicators_p1_2$variable == 'posttax_disp_eq'] <- 'Disposable'
indicators_p1_2$var[
  indicators_p1_2$variable == 'pretax_nation_eq'] <- 'National'
indicators_p1_2$var[
  indicators_p1_2$variable == 'pretax_factor_eq'] <- 'Factor'

# Mean
abb1_1 = ggplot(subset(indicators_p1_2, stat == 'Mean'), 
              aes(x = rb010-1, 
                  y = value,
                  colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Durchschnittseinkommen") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(2000, 7000)) +
  ggsave('reports/hun/hun_img/abb1_1.png')

# Median
abb1_2 = ggplot(subset(indicators_p1_2, stat == 'Q50'), 
                aes(x = rb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Medianeinkommen") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(2000, 7000)) +
  ggsave('reports/hun/hun_img/abb1_2.png')

# Gini
abb1_3 = ggplot(subset(indicators_p1_2, stat == 'Gini'), 
                aes(x = rb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Gini-Koeffizient") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(0.2, 0.6)) +
  ggsave('reports/hun/hun_img/abb1_3.png')

# P80/P20
abb1_4 = ggplot(subset(indicators_p1_2, stat == 'P80/P20'), 
                aes(x = rb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "P80/P20") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 12)) +
  ggsave('reports/hun/hun_img/abb1_4.png')

# Top10
abb1_5 = ggplot(subset(indicators_p1_2, stat == 'Top10'), 
                aes(x = rb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Top 10% Anteil") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0.2, 0.4)) +
  ggsave('reports/hun/hun_img/abb1_5.png')

################################################################################
### Abbildung 2: Ungleichheitsindikatoren nach der wid.world Berechnung (P2)

indicators_p2_2 = melt(indicators_p2, id.vars =  c('pb010', 'stat'))
indicators_p2_2$var = NA
indicators_p2_2$var[
  indicators_p2_2$variable == 'posttax_disp_20'] <- 'Disposable'
indicators_p2_2$var[
  indicators_p2_2$variable == 'pretax_nation_20'] <- 'National'
indicators_p2_2$var[
  indicators_p2_2$variable == 'pretax_factor_20'] <- 'Factor'

# Mean
abb2_1 = ggplot(subset(indicators_p2_2, stat == 'Mean'), 
                aes(x = pb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Durchschnittseinkommen") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0,6000, 1000)) +
  ggsave('reports/hun/hun_img/abb2_1.png')

# Median
abb2_2 = ggplot(subset(indicators_p2_2, stat == 'Q50'), 
                aes(x = pb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Medianeinkommen") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0,6000, 1000)) +
  ggsave('reports/hun/hun_img/abb2_2.png')

# Gini
abb2_3 = ggplot(subset(indicators_p2_2, stat == 'Gini'), 
                aes(x = pb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Gini-Koeffizient") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0.3, 0.7)) +
  ggsave('reports/hun/hun_img/abb2_3.png')

# P80/P20
abb2_4 = ggplot(subset(indicators_p2_2, stat == 'P80/P20'), 
                aes(x = pb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "P80/P20") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) + 
  scale_y_continuous(limits = c(0, 210)) +
  ggsave('reports/hun/hun_img/abb2_4.png')

# Top10
abb2_5 = ggplot(subset(indicators_p2_2, stat == 'Top10'), 
                aes(x = pb010-1, 
                    y = value,
                    colour = var)) + 
  geom_line(size = 2) + 
  scale_x_continuous(breaks=seq(2004,2017,1)) +
  labs(y = "Top 10% Anteil") +
  scale_color_brewer(palette="Paired")+ 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        legend.position = 'top',
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(0.2, 0.5)) +
  ggsave('reports/hun/hun_img/abb2_5.png')

################################################################################
### Abbildung 3/4:

abb4 = ggplot(data = results,aes(year-1)) + 
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper,
                  fill = stat), 
              alpha = 0.3) +
  scale_fill_manual(values = c('black', 'green', 'red')) +
  geom_line(aes(y=value,
                colour = stat),
            size = 2) +
  scale_color_manual(values = c('black', 'green', 'red')) +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(2005,2017,1)) +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'top',
        legend.title = element_blank())


abb3 = ggplot(data = results2,aes(year-1)) + 
  geom_ribbon(aes(ymin = lower, 
                  ymax = upper,
                  fill = stat), 
              alpha = 0.3) +
  scale_fill_manual(values = c('black', 'green', 'red')) +
  geom_line(aes(y=value,
                colour = stat),
            size = 2) +
  scale_color_manual(values = c('black', 'green', 'red')) +
  theme_minimal() + 
  scale_x_continuous(breaks=seq(2005,2017,1)) +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'top',
        legend.title = element_blank())
  
  
