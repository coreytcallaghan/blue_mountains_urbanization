### An analysis script
### The purpose is to test whether or not
### continental-scale urbanness predicts local-scale response to urbanization
### This relies on broad-scale urbanness measures
### and local-scale surveys in small-cities

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


# first an example of how to calculate 
# local-level response to urbanization
# just for a single species to start out with...
species_example <- data %>%
  replace_na(list(Flyover="in_count")) %>%
  dplyr::filter(Flyover == "in_count") %>%
  dplyr::filter(Distance.m < 250) %>%
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


# I think a glmm might be the best way to model these data
# for each species
# i messed around with a few other options, but
# they didn't seem to work as well.
# Open to opinions on this though!!
test_model <- lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
                          data=species_example, family=poisson(link="log"))

summary(test_model)



# Now apply this function to each species
# in order to calculate a local-level response to urbanization for each species
# create a nested df
species_nested <- data %>%
  replace_na(list(Flyover="in_count")) %>%
  dplyr::filter(Flyover == "in_count") %>%
  dplyr::filter(Distance.m < 250) %>%
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

# calculate number of obs for each species for later
species_obs <- data %>%
  group_by(Species) %>%
  summarise(N=n())

# glmer function to apply to each species
# might want to try other options that make more sense?
apply_glm <- function(df){
  lme4::glmer(N ~ urbanness_imperv + (1|Transect), 
              data=df, family=poisson(link="log"))      
}

# apply the function to each species
nested_models <- species_nested %>%
  mutate(glm = map(.f=apply_glm, .x=data))

# extract model results for each species
# then also calculate 'deviance' for each species
# will be important later on - but also might be
# better, more sophisticated options by which to
# limit the species included in the final model approach
nested_tidy <- nested_models %>%
  mutate(tidy = map(.f=tidy, .x=glm)) %>%
  mutate(deviance = map(.f=deviance, .x=glm))


# unnest the models to investigate results
# i add whether a response is positive or negative
# as well as significant or not
# also join with the number of obs for each species
# and the continental urban score for each species
unnested_models <- nested_tidy %>%
  unnest(tidy) %>%
  inner_join(., select(nested_tidy, Species, deviance) %>% unnest(deviance), by="Species") %>%
  dplyr::filter(term == "urbanness_imperv") %>%
  mutate(sign=ifelse(.$estimate > 0, 1, 0)) %>%
  mutate(significant=ifelse(.$p.value < 0.05, "significant", "not-significant")) %>%
  right_join(., urban_scores, by="Species") %>%
  left_join(., species_obs, by="Species")

# plot of the relationship between deviance of a model and sample size
# makes sense...
ggplot(unnested_models, aes(x=deviance, y=N))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  theme_classic()

# histogram of deviance
ggplot(unnested_models, aes(x=deviance))+
  geom_histogram()+
  theme_classic()

# histogram of model estimates
ggplot(unnested_models, aes(x=estimate))+
  geom_histogram()+
  theme_classic()

# histogram of continental scale urban scores
# save this one to possibly include as supp figure
ggplot(unnested_models, aes(x=log(urban_score)))+
  geom_histogram(fill="black", color="white")+
  theme_classic()+
  xlab("Urban scores (log)")+
  ylab("Count")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))+
  ggtitle(paste0("Number of species:", length(unique(unnested_models$Species))))

ggsave(filename="large_vs_local_scale_response/Figures/urban_score_histogram.png", height=4, width=5.5, units="in")


#############################################################
######## Now I want to test the relationship between ########
#### continental and local scale urbanization responses #####
#############################################################

# first try a linear model
# but there are lots of outliers for the local-scale urbanness response
# so I messed around with deviance value to get rid of the outliers
# need to better justify this! Not sure how yet though
# this will be main figure to write around
# so I export it
ggplot(filter(unnested_models, deviance>15), aes(x=estimate, y=log(urban_score)))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))

ggsave(filename="large_vs_local_scale_response/Figures/continental_vs_local.png", height=4, width=5.5, units="in")

# now run a linear model on this
mod <- lm(estimate ~ log(urban_score), data=filter(unnested_models, deviance > 15))

par(mfrow = c(2, 2))
plot(mod)

summary(mod)

# I think that perhaps a binomial model could also work?
# I messed around with it real quick
# but not sure if this makes conceptual sense.
# could be a supp figure though just to illustrate the point a bit more?
# I also am including ALL local-scale results in this one for now.
mod2 <- glm(sign ~ log(urban_score), data=unnested_models, family="binomial")
par(mfrow = c(2, 2))
plot(mod2)
summary(mod2)

ggplot(unnested_models, aes(x=sign, y=log(urban_score)))+
  geom_point()+
  theme_classic()+
  ylab("Continental-scale urbanness")+
  xlab("Local-scale urbanness (Positive or negative)")+
  theme(axis.text=element_text(size=12, color="black"))+
  theme(axis.title=element_text(size=15, color="black"))



#### THOUGHTS?!?!
