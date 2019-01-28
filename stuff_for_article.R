## An R script to make a few things for the AWS Magazine article

# packages
library(readr)
library(dplyr)
library(ggplot2)

# read in data
data <- read_csv("Data/survey_data.csv")

# read in urban data
urban_data <- read_csv("Data/Point_ID-Data.csv")

# read in exotic status
exotic_status <- read_csv("Data/exotic_status.csv")

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



# plot the species richness average for each point
data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  group_by(Point_ID, Transect, Point, Date, Observer) %>%
  summarise(SR=length(unique(Species))) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect) %>%
  summarise(mean_SR=mean(SR),
            sd_SR=sd(SR),
            urbanness_imperv=mean(Percent_impervious),
            urbanness_houses=mean(Number_houses)) %>%
  ggplot(., aes(x=urbanness_imperv, y=mean_SR, group=1)) +
  geom_smooth(se=FALSE)+
  geom_point(color="green4", size=1.8)+
  xlab("Urbanization (% impervious)")+
  ylab("Mean species richness")+
  theme_classic()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))

ggsave(filename="C:/Users/CTC/Desktop/richness_trend.png")


## Plot the same thing but separate lines for each transect
data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  group_by(Point_ID, Transect, Point, Date, Observer) %>%
  summarise(SR=length(unique(Species))) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect) %>%
  summarise(mean_SR=mean(SR),
            sd_SR=sd(SR),
            urbanness_imperv=mean(Percent_impervious),
            urbanness_houses=mean(Number_houses)) %>%
  ggplot(., aes(x=urbanness_imperv, y=mean_SR, group=Transect, color=Transect)) +
  geom_smooth(se=FALSE)+
  geom_point(size=1.8)+
  xlab("Urbanization (% impervious)")+
  ylab("Mean species richness")+
  theme_classic()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))

ggsave(filename="C:/Users/CTC/Desktop/richness_trend_per_transect.png")

# Plot just the rainbow lorikeet
data %>%
  dplyr::filter(Species == "Common Myna") %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) %>%
  ggplot(., aes(x=urbanness_imperv, y=N))+
  geom_point()+
  geom_smooth()



RL <- data %>%
  dplyr::filter(Species == "Rainbow Lorikeet") %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) 

SCC <- data %>%
  dplyr::filter(Species == "Sulphur-crested Cockatoo") %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect,Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) 

WTT <- data %>%
  dplyr::filter(Species == "White-throated Treecreeper") %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) 

CM <- data %>%
  dplyr::filter(Species == "Common Myna") %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect,Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) 


plot <- bind_rows(RL, SCC, CM, WTT) %>%
  spread(Species, N, fill=0) %>%
  gather(., key="Species", value="N", 4:7)

ggplot(plot, aes(x=urbanness_imperv, y=N, color=Species, group=Species))+
  geom_point(size=2.3)+
  geom_smooth(se=FALSE,size=1.3)+
  facet_wrap(~Species, scales="free_y")+
  xlab("Urbanization (% impervious)")+
  ylab("Number of obs.")+
  theme_classic()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  guides(color=FALSE)


ggsave(filename="C:/Users/CTC/Desktop/some_species.png")


## Look at some trends for exotic and native species, respectively
# native
native <- data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  inner_join(., exotic_status, by="Species") %>%
  group_by(Point_ID, Transect, Point, Date, Observer, exotic_status) %>%
  summarise(SR=length(unique(Species))) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  dplyr::filter(exotic_status=="Native") %>%
  group_by(Point, Transect) %>%
  summarise(mean_SR=mean(SR),
            sd_SR=sd(SR),
            urbanness_imperv=mean(Percent_impervious),
            urbanness_houses=mean(Number_houses)) %>%
  mutate(exotic_status="Native")

urban_point_transect <- native %>%
  dplyr::select(Point, Transect, urbanness_imperv, urbanness_houses)

# same thing for exotic
exotic <- data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  inner_join(., exotic_status, by="Species") %>%
  group_by(Point_ID, Transect, Point, Date, Observer, exotic_status) %>%
  summarise(SR=length(unique(Species))) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  dplyr::filter(exotic_status=="Exotic") %>%
  group_by(Point, Transect) %>%
  summarise(mean_SR=mean(SR),
            sd_SR=sd(SR),
            urbanness_imperv=mean(Percent_impervious),
            urbanness_houses=mean(Number_houses)) %>%
  mutate(exotic_status="Exotic")

# put both together and plot
exotic2 <- exotic %>%
  mutate(test="yes") %>%
  dplyr::select(Point, Transect, test) %>%
  right_join(., native, by=c("Point", "Transect")) %>%
  dplyr::select(Point, Transect, test) %>%
  replace_na(list(test=0)) %>%
  left_join(., exotic, by=c("Point", "Transect")) %>%
  dplyr::select(-test, -urbanness_imperv, -urbanness_houses) %>%
  replace_na(list(mean_SR=0, exotic_status="Exotic")) %>%
  inner_join(., urban_point_transect, by=c("Point", "Transect"))


bind_rows(native, exotic2) %>%
  ggplot(., aes(x=urbanness_imperv, y=mean_SR, group=exotic_status, color=exotic_status)) +
  geom_smooth(se=FALSE)+
  geom_point(color="green4", size=1.8)+
  xlab("Urbanization (% impervious)")+
  ylab("Mean species richness")+
  theme_classic()+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  guides(color=guide_legend(title=""))+
  theme(legend.position="top")


ggsave(filename="C:/Users/CTC/Desktop/exotic_native.png")



