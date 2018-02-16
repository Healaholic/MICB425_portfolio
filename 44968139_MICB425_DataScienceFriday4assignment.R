#Saved commands
library(tidyverse)
select(metadata,02)
select(metadata,O2_uM)
metadata %>%
  select(O2_uM)

#Day 2
library(tidyverse)#you need to do this each time to load packages 
data %>% function()
  function(data)#same as before allows for quick and easy reading functions

Saanich.metadata %>%
  select(O2_uM)

View(Saanich.metadata)

oxygen = Saanich.metadata %>%
  select(02)

View(oxygen)

Saanich.metadata %>%
  select(matches("02|oxygen"))

#Filter rows (samples) where oxygen = 0
metadata %>%
  filter(02 = 0)

metadata %>%
  filter(O2_uM==0) %>%
  select(Depth_m)

metadata %>% 
  filter(O2_uM ==0) %>% 
  select(Depth_m)
#Josh's method
metadata %>% 
  select(matches("Depth_m|Temperature_C|CH4_nM|methane")) %>% 
  filter(CH4_nM > 100 & Temperature_C < 10) 
#Prof's method
metadata %>%
  select(matches("Temp"))

#variables are CH4_nM and Temperature_C

metadata %>%
  filter(CH4_nM > 100) %>% 
  filter(Temperature_C < 10) %>% 
  select(Depth_m,Temperature_C,CH4_nM)
#You can put the commands in any order. This command style of the prof doesnt include the Std column because it isnt a match

#next function is mutate
metadata %>%
  mutate(N2O_uM = N2O_nM/1000) %>% 
  select(N2O_uM, N2O_nM)
#you can convert all the uM to nM from this function









  




