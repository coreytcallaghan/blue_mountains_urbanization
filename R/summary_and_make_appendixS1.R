### An R script to create appendix 1 
### A list of all the species and the 
### number of point counts that they occurred on

# packages
library(readr)
library(dplyr)

# read in data
data <- read_csv("Data/survey_data.csv")

# number of point counts that took place
number_points <- nrow(data %>% 
        select(Date, Observer, Point_ID) %>%
        distinct(.))

# total time spent surveying
number_points*5

# get list of species and number of records
species_list_abundance <- data %>%
  dplyr::select(Date, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Point_ID, Species) %>%
  distinct(.) %>%
  group_by(Species) %>%
  summarise(count = n()) %>%
  filter(Species != "Satin/Leaden Flycatcher") %>%
  mutate(points = number_points) %>%
  mutate(percent_points = (count/points)*100) %>%
  dplyr::select(Species, percent_points) %>%
  mutate_if(is.numeric, funs(round(., 2))) %>%
  arrange(desc(percent_points))
    
# write out as csv to make look good in 
write_csv(species_list_abundance, "Supplementary information/AppendixS1/AppendixS1.csv")



