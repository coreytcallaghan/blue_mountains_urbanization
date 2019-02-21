# packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)
library(lme4)
library(patchwork)

# read in data
data <- read_csv("Data/survey_data.csv")

# read in urban data
urban_data <- read_csv("Data/Point_ID-Data.csv")

# read in exotic status
exotic_status <- read_csv("Data/exotic_status.csv")

# read in urbanness scores
urban_scores <- read_csv("Data/urbanness_scores.csv")


species_example <- data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) %>%
  replace_na(list(Species="none")) %>%
  dplyr::filter(Species != "none") %>%
  spread(Species, N, fill=0) %>%
  gather(., key="Species", value="N", 4:98) %>%
  dplyr::filter(Species=="Eastern Spinebill")

hist(species_example$N)

test_model <- lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
                          data=species_example, family=poisson(link="log"))

summary(test_model)




# Apply a function to each species
species_nested <- data %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species) %>%
  group_by(Date, Observer, Species, Transect, Point_ID, Point) %>%
  distinct(.) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect, Species) %>%
  summarise(N=n(),
            urbanness_imperv=mean(Percent_impervious)) %>%
  replace_na(list(Species="none")) %>%
  dplyr::filter(Species != "none") %>%
  spread(Species, N, fill=0) %>%
  gather(., key="Species", value="N", 4:98) %>%
  group_by(Species) %>%
  nest()

species_obs <- data %>%
  group_by(Species) %>%
  summarise(N=n())

apply_glm <- function(df){
  lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
              data=df, family=poisson(link="log"))      
}


nested_models <- species_nested %>%
  mutate(glm = map(.f=apply_glm, .x=data))

nested_tidy <- nested_models %>%
  mutate(tidy = map(.f=tidy, .x=glm)) %>%
  mutate(deviance = map(.f=deviance, .x=glm))

unnested_models <- nested_tidy %>%
  unnest(tidy) %>%
  inner_join(., select(nested_tidy, Species, deviance) %>% unnest(deviance), by="Species") %>%
  dplyr::filter(term == "urbanness_imperv") %>%
  mutate(sign=ifelse(.$estimate > 0, 1, 0)) %>%
  mutate(significant=ifelse(.$p.value < 0.05, "significant", "not-significant")) %>%
  right_join(., urban_scores, by="Species") %>%
  left_join(., species_obs, by="Species")


ggplot(unnested_models, aes(x=deviance, y=N))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()


ggplot(unnested_models, aes(x=deviance))+
  geom_histogram()+
  theme_classic()


ggplot(unnested_models, aes(x=estimate))+
  geom_histogram()+
  theme_classic()

ggplot(unnested_models, aes(x=log(urban_score)))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("Urban scores (log)")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("Number of species:", length(unique(unnested_models$Species))))

ggsave(filename="large_vs_local_scale_response/Figures/urban_score_histogram.png", height=4, width=5.5, units="in")

ggplot(filter(unnested_models, deviance>15), aes(x=estimate, y=log(urban_score)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))

ggsave(filename="large_vs_local_scale_response/Figures/continental_vs_local.png", height=4, width=5.5, units="in")

mod <- lm(estimate ~ log(urban_score), data=filter(unnested_models, deviance > 15))

plot(mod)

summary(mod)


mod2 <- glm(sign ~ log(urban_score), data=unnested_models, family="binomial")
summary(mod2)

ggplot(unnested_models, aes(x=sign, y=log(urban_score)))+
  geom_point()


## repeat the above but with 'stronger' models
## to show if the relationship strengthens with better fitted models
model_function <- function(x) {
  
  mod_data <- unnested_models %>%
    dplyr::filter(deviance > x)
  
  mod <- lm(estimate ~ log(urban_score), data=mod_data)
  
  df <- tidy(mod) %>%
    mutate(r2 = summary(mod)$adj.r.squared) %>%
    mutate(df = mod$df.residual) %>%
    mutate(sample_size = nrow(mod_data))
}


## now create a vector of deviance
list_dev <- seq.int(15, 75, 5)


list <- lapply(list_dev, function(x) {model_function(x)})

more_models <- bind_rows(list) %>%
  dplyr::filter(term=="log(urban_score)")




# do the same thing but for the statistic instead of the 'estimate'
ggplot(filter(unnested_models, N>5), aes(x=statistic, y=log(urban_score)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))

# maybe try to run models for that as well

# Now look above the species-level
# first for species richness
# get a dataset of unique surveys
number_points <- nrow(data %>% 
                        select(Date, Observer, Point_ID) %>%
                        distinct(.))

number_points

surveys_unique <- data %>% 
  select(Date, Observer, Point_ID, Transect, Point) %>%
  distinct(.)

# now for 'urbanness' of a community
community_urbanness_survey <- data %>%
  left_join(., urban_scores, by="Species") %>%
  dplyr::filter(Species != "Satin/Leaden Flycatcher") %>%
  replace_na(list(Species = "None", urban_score=100)) %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species, urban_score) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  group_by(Point_ID, Transect, Point, Date, Observer) %>%
  summarise(SR = length(unique(Species)),
            urbanness = quantile(urban_score, 0.25)) %>%
  full_join(., surveys_unique, by=c("Date", "Observer", "Point_ID", "Transect", "Point")) %>%
  replace_na(list(SR=0)) %>%
  inner_join(., urban_data, by="Point_ID") %>%
  group_by(Point, Transect) %>%
  summarise(mean_SR=mean(SR),
            sd_SR=sd(SR),
            mean_urbanness=mean(urbanness, na.rm=TRUE),
            sd_urbanness=sd(urbanness, na.rm=TRUE),
            urbanness_imperv=mean(Percent_impervious),
            urbanness_houses=mean(Number_houses))

SR_plot <- ggplot(community_urbanness_survey, aes(x=urbanness_imperv, y=mean_SR, group=1)) +
  geom_smooth(method="lm", se=FALSE)+
  geom_point(aes(color=Transect), size=1.8)+
  xlab("")+
  ylab("Mean species richness")+
  theme_classic()+
  theme(axis.text=element_text(size=10, color="black"))+
  theme(axis.title.y=element_text(size=12, color="black"))+
  guides(color=FALSE)


urbanness_plot <- ggplot(community_urbanness_survey, aes(x=urbanness_imperv, y=mean_urbanness, group=1)) +
  geom_smooth(method="lm", se=FALSE)+
  geom_point(aes(color=Transect), size=1.8)+
  xlab("Urbanization (% impervious)")+
  ylab("Mean community urbanness")+
  theme_classic()+
  theme(axis.text=element_text(size=10, color="black"))+
  theme(axis.title.x=element_text(size=14, color="black"))+
  theme(axis.title.y=element_text(size=12, color="black"))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())

SR_plot + urbanness_plot + plot_layout(ncol=1)

ggsave(filename="C:/Users/CTC/Desktop/SR_and_urbanness.png", height=6, width=5.5, units="in")


data %>%
  left_join(., urban_scores, by="Species") %>%
  dplyr::filter(Species != "Satin/Leaden Flycatcher") %>%
  replace_na(list(Species = "None", urban_score=100)) %>%
  dplyr::select(Date, Point, Transect, Observer, Point_ID, Species, urban_score) %>%
  group_by(Date, Observer, Transect, Point_ID, Point, Species) %>%
  distinct(.) %>%
  group_by(Point_ID, Transect, Point, Date, Observer) %>%
  summarise(SR = length(unique(Species)),
            urbanness = quantile(urban_score, 0.25)) %>%
  ggplot(., aes(x=urbanness, y=SR))+
  geom_point()+
  xlab("Community urbanness")+
  ylab("Species richness")+
  theme_classic()+
  theme(axis.text=element_text(size=10, color="black"))+
  theme(axis.title.x=element_text(size=12, color="black"))+
  theme(axis.title.y=element_text(size=12, color="black"))


ggsave(filename="C:/Users/CTC/Desktop/richness_vs_urbanness.png", height=4, width=5.5, units="in")

