library(devtools)
library(roxygen2)
library(tidyverse)
# library(dplyr)
library(magrittr)
library(tidyr)
library(usethis)

# use_package('EnvStats')

## 1) Read in 2000s animal-contact dataset
# 2007 MPX survey data
mpx1b = read.csv('MPX1b_almostfinal.csv', as.is=TRUE) %>%
  mutate(F01F04 = dmy(F01F04),
         LastUp = dmy(LastUp),
         approxAge = 2007-F01B01YY) %>%
  rename(cooke_cascan = Cooke_cascan, cooke_cwolfi = Cooke_cwolfi,
         cooke_bat = Cooke_bat, cooke_elephant = Cooke_elephant,
         cooke_monkeyNI = Cooke_monkeyNI, cooke_boar = Cooke_boar,
         cooke_reptile = Cooke_reptile, anyrodent_cooke = anyrodent_Cooke,
         cooke_cchryso = Cooke_cchryso, cooke_cnicit = Cooke_cnicit,
         cooke_laterr = Cooke_laterr, cooke_shrew = Cooke_shrew,
         cooke_mouse = Cooke_mouse, cooke_porc = Cooke_porc,
         cooke_squirrel = Cooke_squirrel, anynhp_cooke = anynhp_Cooke,
         cooke_cangol = Cooke_cangol, cooke_ptholl = Cooke_ptholl,
         cooke_ppan = Cooke_ppan, cooke_galago = Cooke_galago,
         cooke_other = Cooke_other, cooke_potto = Cooke_potto,
         cooke_bird = Cooke_bird, anyeuth_cooke = anyeuth_Cooke,
         cooke_cnegl = Cooke_cnegl, cooke_admouse = Cooke_admouse,
         cooke_duiker = Cooke_duiker, cooke_gamrat = Cooke_gamrat,
         cooke_pang = Cooke_pang, cooke_rat = Cooke_rat,
         cooke_cat = Cooke_cat, anyloris_cooke = anyloris_Cooke) %>%
  filter(!is.na(Huntf_cascan)) # Remove missing values

# Selects NetID, FormID, contact columns, and approxAge
mpx1b = mpx1b[,c(1,2,178:990,1029)]

usethis::use_data(mpx1b, overwrite = TRUE)

# Import 1980s dataset
data1980control = read.csv('1980s animal contact data_controls.csv', as.is=TRUE) %>%
  filter(Type == 'C') %>%
  select(Serum.ID, Age, Anim1, Freq1, Anim2, Freq2, Anim3, Freq3, Squirrel)

usethis::use_data(data1980control, overwrite = T)

## 2) Specify which animal group a given name should be assigned to
# names used for the different animal species in the original dataset
orig.animal.names = c('cascan',# - red-tailed monkey (NHP) - Cercopithecus ascanius
                      'cchryso',# - golden-bellied mangabey (NHP) - Cercocebus chrysogaster
                      'cangol',# - Angola colobus (NHP) - Colobus angolensis
                      'cnegl',# - De Brazza's monkey (NHP) - Cercopithecus neglectus
                      'cwolfi',# - Wolf's mona monkey (NHP) - Cercopithecus wolfi
                      'cnicit',# - greater spot-nosed monkey (NHP) - Cercopithecus nicitans
                      'ptholl',# - Thollon's red colobus (NHP) - Procolobus tholloni
                      'admouse',# - African dormouse (rodent)
                      'bat',# - bat (other)
                      'laterr',# - Black crested mangabey (NHP) - Lophocebus aterrimus
                      'ppan',# - bonobo (NHP) - Pan paniscus
                      'duiker',# - (antelope)
                      'elephant',# - elephant (other)
                      'shrew',# - shrew (rodent)
                      'galago',# - bush baby (NHP)
                      'gamrat',# - Gambian Rat (rodent)
                      'monkeyNI',# - monkey (NHP of an unidentified species)
                      'mouse',# - mouse (rodent)
                      'other',# - some other animal (other)
                      'pang',# - pangolin (other)
                      'boar',# - (boar)
                      'porc',# - porcupine (rodent)
                      'potto',# - (NHP)
                      'rat',# - rat (rodent)
                      'reptile',# - reptile (other)
                      'squirrel',# - squirrel (rodent)
                      'bird',# - bird (other)
                      'cat')# - wild cat (other)
                      # 'squirrel')# - squirrel (added twice to classify squirrels separately and with rodents)

usethis::use_data(orig.animal.names, overwrite =T)

# for each of those names, what group should that animal belong to?
animal.groups = c('NHP','NHP','NHP','NHP','NHP','NHP','NHP','rodent','other','NHP',
                  'NHP','antelope','other','rodent','NHP','rodent','NHP','rodent','other',
                  'other','boar','rodent','NHP','rodent','other','rodent','other','other')

usethis::use_data(animal.groups, overwrite = T)

# names of the animal groups
animal.group.names = unique(animal.groups)

usethis::use_data(animal.group.names, overwrite = T)
