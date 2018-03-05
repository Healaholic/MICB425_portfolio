#Commands from tutorial 1 
install.packages("tidyverse")
library(tidyverse)
read.table(file="Saanich.metadata.txt")
read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t", na.strings="NAN")
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")


#Which is equivalent to

metadata <- read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")




#Troubleshooting
#setwd("C:\Users\Ian\Documents\MICB425_portfolio")
#setwd("../")
#getwd()


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

#working with data handout exercise 3

#example mutation plot
metadata %>% 
  mutate(N2O_uM = N2O_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=N2O_uM))

#plotting in nM this time (looks much better)
metadata %>% 
  mutate(N2O_uM = N2O_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=N2O_nM))

# first we will need to view all variables with nM in them

metadata %>%
  select(matches("nM"))
#so N2o, and CH4 are in nM. Now we have to make them uM
#plotting mutated CH4 data
metadata %>% 
  mutate(CH4_uM = CH4_nM/1000) %>% 
  ggplot() + geom_point(aes(x=Depth_m, y=CH4_uM))



#DS assignment 4 commands

#Load all these in as well please
library(tidyverse)
library(phyloseq)
metadata = read.table(file="Saanich.metadata.txt", header=TRUE, row.names=1, sep="\t")
load("phyloseq_object.RData")
physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))

source("https://bioconductor.org/biocLite.R")

biocLite("phyloseq")

library(phyloseq)
#loading the phyloseq data
load("phyloseq_object.RData")

#dot plots
ggplot(metadata, aes(x=O2_uM, y=Depth_m))

ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point()
#equivalent to 
ggplot(metadata) +
  geom_point(aes(x=O2_uM, y=Depth_m))

#how to change colour or size of plot 
ggplot(metadata, aes(x=O2_uM, y=Depth_m, color="blue")) +
  geom_point()#this is not right as gg plot cannot target the specific points this way
#this is correct
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(color="blue")  
#changing the shapes to square
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(shape="square")
#changing the size of the plot dots
ggplot(metadata, aes(x=O2_uM, y=Depth_m)) +
  geom_point(size=10)#thats really big btw
#where size equals to an actual variable
ggplot(metadata, aes(x=O2_uM, y=Depth_m, size=OxygenSBE_V)) +
  geom_point()


#Excercise 1
ggplot(metadata, aes(x=NO2_uM, y=Depth_m)) +
  geom_point(color="purple", shape=17)  #17= triangle
#guide to shapes http://sape.inf.usi.ch/sites/default/files/ggplot2-shape-identity.png
#shift click to follow link


#Excersise 2
metadata %>%
  select(matches("Temp"))

metadata %>% 
  mutate(Temperature_F = Temperature_C*1.8+32) %>% 
  ggplot(aes(x=Temperature_F, y=Depth_m)) + 
  geom_point()


physeq
#something about how convenient phyloseq is because it plays well with the program. 
#by doing the next step we can plot a bar graph according to phylum level of communities
plot_bar(physeq, fill="Phylum")

#Phyloseq's transform_sample_counts function is used for a common problem, which is to mutate OTU table values into percentages of total 
physeq_percent = transform_sample_counts(physeq, function(x) 100 * x/sum(x))

#now we can plot it accordingly
plot_bar(physeq_percent, fill="Phylum")

#the next thing we're gonna do is to merge all portions of bars that are the same colour together
plot_bar(physeq_percent, fill="Phylum") + 
  geom_bar(aes(fill=Phylum), stat="identity")

#Exercise 3
plot_bar(physeq, fill="Family")
plot_bar(physeq_percent, fill="Class")
#plotting class cause its easier but filling in the blacks too
#Filling in the labels
plot_bar(physeq_percent, fill="Class") + 
  geom_bar(aes(fill=Class), stat="identity")+
  ggtitle("Classes from 10 to 200m in the Saanich inlet")+
  xlab("Sample Depth")+
  ylab("Percent Relative Abundance")

#Faceting can be done with the following:
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum)
#The scales for each graph can be adjusted to fit each category so that we can visualize relative values
#We can also remove the legend since it is not relevant
plot_bar(physeq_percent, fill="Phylum") +
  geom_bar(aes(fill=Phylum), stat="identity") +
  facet_wrap(~Phylum, scales="free_y") +
  theme(legend.position="none")

#Exercise 4

#so it says to use gather to allow for manipulation of the data
metadata %>%
  gather("O2_uM,PO4_uM,SiO2_uM,NO3_uM,NH4_uM,NO2_uM")

#first we're gonna remade a simple ggplot with Depth as X axis and O2 as y axis
ggplot(metadata, aes(x=Depth_m, y=O2_uM)) +
  geom_point()+
  geom_line()

#This works for only 1 nutrient. We will thus create multiple designations for each nutrient type 

#gather allows you to group all the nutrients under a new column called Nutrients_uM, with units in uM
metadata %>% 
  gather(Nutrients_uM, uM, NH4_uM, NO2_uM, NO3_uM, O2_uM, PO4_uM, SiO2_uM) %>%
  ggplot() +geom_point(aes(x=Depth_m, y=uM)) + geom_line(aes(x=Depth_m, y=uM)) +
  facet_wrap(~Nutrients_uM, scales="free_y")


#Problem set 04

#Part 01
library(kableExtra)
library(tidyverse)
library(knitr)

example_data1 = data.frame(
    number = c(1,2,3),
    name = c("lion", "tiger", "bear"),
    characteristics = c("brown cat", "striped cat", "not a cat"),
    occurences = c(2, 4, 1)
  )  

bin1 = data.frame(
  Number = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
  Name = c("Rigoa","Skittles","MandMs","MikeandIkes","Gummybears","Lego","Gumdrops","fruitgummies","macrophage","cokebottles","Gummywhitedrops","Watermelon","RedGreenFish","Kisses","Redsnakes"),
  Characteristics = c("long gummies","sour candy with shell","Chocolates with shell","Long chewy beans","Bear shaped gummies","Brick shaped hard candy","large round chewy candy with hard shell","fruit shaped gummies","octopus shaped gummies coated sugar","coke bottle shaped gummies","striped disk gummies coated suger","watermelon coloured and sphere shaped gummies","red and green fish shaped gummies","teardrop shaped chocolates","long thin red snake gummies"),
  Occurences = c(7,197,218,199,91,18,24,2,6,3,3,1,1,16,13)
 )  
  
bin1 %>% 
  kable("html") %>%
  kable_styling(bootstrap_options = "striped", font_size = 10, full_width = F)
#The "organisms" found in this table takes into account all candy given to us and leaves none out. Rare or unclassifiable species were given their own bin with descriptions on how they differed. 


#Part 02
bin2 = data.frame(
  x = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141),
  y = c(1,2,2,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8)
)

ggplot(bin2, aes(x=x, y=y)) +
  geom_point() +
  geom_smooth() +
  labs(x="Cumulative number of individuals classified", y="Cumulative number of species observed")
#The curve produced is observed to have flattened out after 56 cells have been sampled.
#The cap on number of species observed is limited by the sampling size: only about half of all species have been observed from this small sample

#Part 03

Spec1 = 7/799
Spec2 = 197/799
Spec3 = 218/799
Spec4 = 199/799
Spec5 = 91/799
Spec6 = 18/799
Spec7 = 24/799
Spec8 = 2/799
Spec9 = 6/799
Spec10 = 3/799
Spec11 = 3/799
Spec12 = 1/799
Spec13 = 1/799
Spec14 = 16/799
Spec15 = 13/799

1/(Spec1^2 + Spec2^2 + Spec3^2 + Spec4^2 + Spec5^2 + Spec6^2 + Spec7^2 + Spec8^2 + Spec9^2 + Spec10^2 + Spec11^2 + Spec12^2 + Spec13^2 + Spec14^2 + Spec15^2)

#The simpson reciprocal index for my total original community is 4.706271.

s1 = 31/141
s2 = 6/141
s3 = 29/141
s4 = 46/141
s5 = 17/141
s6 = 5/141
s7 = 3/141
s8 = 4/141

1/(s1^2+ s2^2 + s3^2 + s4^2 +s5^2 + s6^2 + s7^2 + s8^2)
#The simpson reciprocal index for my own sample is 4.631027.

8 
# The chao1 estimate for my sample is 8

15+2^2/26

#The chao1 estimate for the total original community is 15.15385.

#Part04
library(vegan)

bin1_diversity = 
  bin1 %>% 
  select(Name, Occurences) %>% 
  spread(Name, Occurences)

bin3_diversity = 
  bin3 %>% 
  select(Name, Occurences) %>% 
  spread(Name, Occurences)

bin3 = data.frame(
  Number = c(1,2,3,4,5,6,7,8),
  Name = c("Skittles","Bigballs","M&Ms","Jellybeans","Gummybears","Bricks","Kisses","Redsnakes"),
  Characteristics = c("sour candy with shell","large round chewy candy with hard shell","Chocolates with shell","Long chewy beans","Bear shaped gummies","Brick shaped hard candy","teardrop shaped chocolates","long thin red snake gummies"),
  Occurences = c(31,6,29,46,17,5,3,4)
)  


diversity(bin1_diversity, index="invsimpson")
#Simpson Reciprocal Index for the total organism pool is 2.333333.
specpool(bin1_diversity)
# Species chao chao.se jack1 jack1.se jack2 boot boot.se n
#All      15   15       0    15        0    15   15       0 1
diversity(bin3_diversity, index="invsimpson")
#Simpson Reciprocal Index for my sample is 4.631027.
specpool(bin3_diversity)
#    Species chao chao.se jack1 jack1.se jack2 boot boot.se n
#All       8    8       0     8        0     8    8       0 1

# The values match the previous calculations
