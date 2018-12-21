# Data Cleaning Hungary
# Authors: Azad, Zoeldi
# Description: create income variables according to several concepts, label 
# factor variables 
# Output: see end of script


### calcualte na-fractions
p_isna = hun_p %>% 
  group_by(pb010) %>% 
  summarise_each(funs(sum(is.na(.)) / length(.) * 100))

h_isna = hun_h %>% 
  group_by(hb010) %>% 
  summarise_each(funs(sum(is.na(.)) / length(.) * 100))

d_isna = hun_d %>% 
  group_by(db010) %>% 
  summarise_each(funs(sum(is.na(.)) / length(.) * 100))

r_isna = hun_r %>% 
  group_by(rb010) %>% 
  summarise_each(funs(sum(is.na(.)) / length(.) * 100))

################################################################################
################################################################################
################################################################################

### ID ###
hun_p = hun_p %>% mutate(id = paste0(pb010, px030))
hun_h = hun_h %>% mutate(id = paste0(hb010, hb030))
hun_d = hun_d %>% mutate(id = paste0(db010, db030))
hun_r = hun_r %>% mutate(id = paste0(rb010, rx030))

hun_p = hun_p %>% mutate(idp = paste0(pb010, pb030))
hun_r = hun_r %>% mutate(idp = paste0(rb010, rb030))

### Income ###
# Company car: 'companycar' untill 2006 with py020g, from 2007 with py021g
hun_p$companycar = 0
hun_p$companycar[hun_p$pb010 <= 2006] <- hun_p$py020g[hun_p$pb010 <= 2006]
hun_p$companycar[hun_p$pb010 > 2006] <- hun_p$py021g[hun_p$pb010 > 2006]

# Set NA in incvars to 0
incvar_p = c('py010g', 'companycar', 'py050g', 'py080g', 'py090g', 'py100g',
             'py110g', 'py120g', 'py130g', 'py140g')
hun_p[incvar_p][is.na(hun_p[incvar_p])] <- 0

incvar_h = c('hy110g', 'hy040g', 'hy090g', 'hy050g', 'hy060g', 'hy070g',
             'hy080g', 'hy120g', 'hy130g', 'hy140g')
hun_h[incvar_h][is.na(hun_h[incvar_h])] <- 0


### Age, 20-Dummy, Number of Ind. >= 20 ###
# Age and Age-Dummy
hun_p$age = hun_p$pb010 - hun_p$pb140
hun_p$age_20 = 0
hun_p$age_20[hun_p$age >= 20] = 1

# Number obs. over 20 per hh (hhm)
hun_p = hun_p %>% group_by(id) %>% mutate(hhm = sum(age_20))


################################################################################
############################# P1 (Eurostat) ####################################
################################################################################

# Create dh-File
hun_dh = hun_d %>%
  select(id, db040, db090) %>%
  left_join(hun_h, by = 'id')

# Create personal income collapsed at hh-level and join to dh-File
hun_dhp = hun_p %>% 
  group_by(id) %>% 
  summarise(py010g_hh = sum(py010g),
            companycar_hh = sum(companycar),
            py050g_hh = sum(py050g),
            py080g_hh = sum(py080g),
            py090g_hh = sum(py090g),
            py100g_hh = sum(py100g),
            py110g_hh = sum(py110g),
            py120g_hh = sum(py120g),
            py130g_hh = sum(py130g),
            py140g_hh = sum(py140g)) %>%
  left_join(hun_dh, by = 'id')

# Create income concepts at hh-level
hun_dhp = hun_dhp %>%
  mutate(pretax_factor = 
           (py010g_hh + 
           companycar_hh + 
           py050g_hh + 
           hy110g +
           hy040g + 
           hy090g + 
           py080g_hh),
         pretax_nation = 
           (pretax_factor + 
           py090g_hh + 
           py100g_hh),
         posttax_disp = 
           (pretax_nation + 
           py110g_hh + 
           py120g_hh + 
           py130g_hh +
           py140g_hh + 
           hy050g + 
           hy060g + 
           hy070g +
           hy080g -
           hy120g -
           hy130g -
           hy140g),
         pretax_factor_eq = 
           pretax_factor / hx050,
         pretax_nation_eq = 
           pretax_nation / hx050,
         posttax_disp_eq = 
           posttax_disp / hx050)

# Merge income concepts to ALL individuals
hun_rh = hun_dhp %>%
  select(id, db040, pretax_factor_eq, pretax_nation_eq, posttax_disp_eq) %>%
  left_join(hun_r, by = 'id')

### Output
# On the individul level in data frame 'hun_rh' with variables 
  # pretax_factor_eq, pretax_nation_eq, posttax_disp_eq
# On the household level in data frame 'hun_dhp' with variables
  # pretax_factor_eq, pretax_nation_eq, posttax_disp_eq

################################################################################
############################### P2 (wid.world) #################################
################################################################################

# link hh-income components to p-File
hun_ph = hun_dhp %>%
  select(id, db040, incvar_h) %>%
  left_join(hun_p, by = 'id')
# Income aggregates in ph-file
hun_ph = hun_ph %>%
  mutate(pretax_factor_20 = 
           (py010g + 
              companycar + 
              py050g + 
              hy110g / hhm +
              hy040g / hhm + 
              hy090g / hhm + 
              py080g),
         pretax_nation_20 = 
           (pretax_factor_20 + 
              py090g + 
              py100g),
         posttax_disp_20 = 
           (pretax_nation_20 + 
              py110g + 
              py120g + 
              py130g +
              py140g + 
              hy050g / hhm + 
              hy060g / hhm + 
              hy070g / hhm +
              hy080g / hhm -
              hy120g / hhm -
              hy130g / hhm -
              hy140g / hhm))

# Collapse these income aggregates by hh and linkt them to hun_dhp
hun_dhp = hun_ph %>%
  filter(age_20 == 1) %>% 
  select(id, pretax_factor_20, pretax_nation_20, posttax_disp_20) %>%
  group_by(id) %>%
  summarise(pretax_factor_20_hh = sum(pretax_factor_20),
            pretax_nation_20_hh = sum(pretax_nation_20),
            posttax_disp_20_hh = sum(posttax_disp_20)) %>%
  right_join(hun_dhp, by = 'id')

# Set NA in hun_dhp to (20 obs, hhs with individuals only age <20)
incvar_h_2 = c('pretax_factor_20_hh', 'pretax_nation_20_hh', 
               'posttax_disp_20_hh')
hun_dhp[incvar_h_2][is.na(hun_dhp[incvar_h_2])] <- 0

### Output
# On indivudial level in dataframe 'hun_ph' variables 'pretax_factor_20, 
  # pretax_nation_20, posttax_disp_20.
# On hh-lvel in dataframe 'hun_dhp' variables 'pretax_factor_20_hh, 
  # pretax_nation_20_hh, posttax_disp_20_hh.


################################################################################
######################### Data for Decomposition ###############################
################################################################################

# Copy hun_p
hundat = hun_p

# Sex
hundat$female = 0
hundat$female[hundat$pb150 == 2] <- 1
hundat$female = factor(hundat$female, labels = c('Male', 'Female'))

# Consensual Union
hundat$union = 0
hundat$union[hundat$pb200 %in% c(1, 2)] <- 1 
hundat$union = factor(hundat$union, labels = c('Single', 'Couple'))

# Region
hundat = hun_d %>% 
  select(id, db040) %>%
  left_join(hundat, by = 'id')

hundat$region = 0  
hundat$region[hundat$db040 == 'HU1'] <- 1
hundat$region[hundat$db040 == 'HU2'] <- 2
hundat$region[hundat$db040 == 'HU3'] <- 3
hundat$region = factor(hundat$region, labels = c('Middle', 'West', 'East'))
table(hundat$region, hundat$pb010)

# Urbanisation
hundat = hun_d %>%
  select(id, db100) %>%
  left_join(hundat, by = 'id')

hundat$urban = 0
hundat$urban[hundat$db100 == 1] <- 1
hundat$urban = factor(hundat$urban, labels = c('Thinly', 'Densely'))

# Experience (Years since education left)
hundat$exper = hundat$pb010 - hundat$pe030

# Education
hundat$educ = NA
hundat$educ[hundat$pe040 %in% c(0, 1, 2)] <- 1
hundat$educ[hundat$pe040 %in% c(3, 4)] <- 2
hundat$educ[hundat$pe040 %in% 5] <- 3
hundat$educ = factor(hundat$educ, labels = c('Low', 'Middle', 'High'))

# ISCO
hundat$isco = 0
hundat$isco[hundat$pl050 <= 39 & hundat$pb010 <= 2009] <- 1
hundat$isco[hundat$pl051 <= 39 & hundat$pb010 >= 2010] <- 1
hundat$isco = factor(hundat$isco, labels = c('Unskilled', 'Skilled'))

# NACE
hundat$nace = NA
hundat$nace[hundat$pb010 <= 2007] <- hundat$pl110[hundat$pb010 <= 2007]
hundat$nace[hundat$pb010 >= 2008] <- hundat$pl111[hundat$pb010 >= 2008]

hundat$sector = NA
hundat$sector[hundat$nace %in% c( 'a+b',
                                  'c+d+e',
                                  'f',
                                  'a',
                                  'b - e')] <- 1
hundat$sector[hundat$nace %in% c( 'g',
                                  'h',
                                  'i',
                                  'j',
                                  'k',
                                  'o+p+q',
                                  'l - n',
                                  'r - u')] <- 2
hundat$sector[hundat$nace %in% c( 'l',
                                  'm',
                                  'n',
                                  'o',
                                  'p',
                                  'q')] <-3
hundat$sector = factor(hundat$sector, labels = c('AIC', 'SERV', 'PUB'))

# Type of contract
hundat$contract = 0
hundat$contract[hundat$pl140 == 2] <- 1
hundat$contract = factor(hundat$contract, labels = c('Permanent', 'Temporary'))

# Firm size
hundat$firmsize = 0
hundat$firmsize[hundat$pl130 == 13] <- 1
hundat$firmsize = factor(hundat$firmsize, labels = c('Small', 'Big'))

# Hourly wage (py010g / (total month in year * 4 * weekly hours))
hwage_var = c('pl073', 'pl074', 'pl070', 'pl072', 'pl060', 'pl100')
hundat[hwage_var][is.na(hundat[hwage_var])] <- 0
rm(hwage_var)

hundat = hundat %>% 
  mutate(hwage = py010g / 
           ((pl070 + pl072 + pl073 + pl074) * 4 * (pl060 + pl100)),
         hwage = coalesce(hwage, 0))

hundat$hwage[is.infinite(hundat$hwage)] <- 0

# Filter and select final sample
hundat = hundat %>%
  select(idp, id, pb010, female, union, region, urban, exper, educ, isco, 
         sector, firmsize, contract, hwage, pb040) %>%
  filter(hwage > 0) %>%
  na.omit()
