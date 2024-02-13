### Submission

### Analysis

setwd("../Data")
dat_sum_temp <- read.csv("pla_dat_clean.csv")

dat_sum_temp$time <- as.POSIXct(strptime(dat_sum_temp$Date, "%m/%d/%Y"))
dat_sum_temp$Months <- format(strptime(dat_sum_temp$Date, "%m/%d/%Y"), '%m') 
dat_sum_temp$Months <- as.numeric(dat_sum_temp$Months)

## Determining bird frugivory interaction rates in areas with and without mammals
shapiro.test(dat_sum_temp$Bird^0.5)  #Bird rates need to be square root transformed
shapiro.test(dat_sum_temp$mammal.visits)
shapiro.test(dat_sum_temp$mammal.visits^0.5)
shapiro.test(log(dat_sum_temp$mammal.visits+0.01))  
shapiro.test((dat_sum_temp$mammal.visits+0.01)^-1)    #transforming doesn't help much
shapiro.test(log(dat_sum_temp$Interact))

dat_sum_temp$log_Interact <- log(dat_sum_temp$Interact)

dat_sum_temp$sqrt_bird <- dat_sum_temp$Bird^0.5

library(mblm)
library(nlme)
library(rcompanion)
library(MuMIn)
library(sjPlot)
library(effects)
library(ggplot2)
library(car)

## Do mammal visits help predict the number of frugivorous interactions?
mod1_pre <- lme(log_Interact ~ mammal.visits + Continent + Group + heavy_rain, random=~1|island, data=dat_sum_temp)
anova(mod1_pre)     #F=3.35, df=12, p = 0.04

mod1 <- lme(log_Interact ~ mammal.visits, random=~1|island, data=dat_sum_temp)   #Removing Continent, Group, heavy rain to improve degrees of freedom. None were significant.
anova(mod1)     #F=3.35, df=12, p = 0.04
plot(mod1)

summary(mod1)

temp <- resid(mod1)
shapiro.test(temp)   #Residuals are normal

r.squaredGLMM(mod1)

effects_temp_1 <- effects::effect(term="mammal.visits", mod= mod1)
summary(effects_temp_1)
x_temp_1 <- as.data.frame(effects_temp_1)

### FIGURE 3

plot1 <- ggplot() + 
  geom_point(data=dat_sum_temp, aes(mammal.visits, log_Interact), size =3) + 
  geom_line(data=x_temp_1, aes(x=mammal.visits, y=fit), color="black", size=1.5) +
  geom_ribbon(data= x_temp_1, aes(x=mammal.visits, ymin=lower, ymax=upper), alpha= 0.3, fill="grey") +
  ylim(2,6)+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="# of mammal visits", y="# of frugivorous interactions (log)")
plot1

## Do mammal visits help predict the number of frugivorous interactions?
mod2 <- lme(sqrt_bird ~ mammal.visits, random=~1|island, data=dat_sum_temp)
anova(mod2)    #F=7.11, df=12, p=0.0363
plot(mod2)

res2 <- resid(mod2)
shapiro.test(res2)   #Residuals are normal

r.squaredGLMM(mod2)

effects_temp_2 <- effects::effect(term="mammal.visits", mod= mod2)
summary(effects_temp_2)
x_temp_2 <- as.data.frame(effects_temp_2)

plot2 <- ggplot() + 
  geom_point(data=dat_sum_temp, aes(mammal.visits, sqrt_bird), size=3) + 
  geom_line(data=x_temp_2, aes(x=mammal.visits, y=fit), color="black", size=1.5) +
  geom_ribbon(data= x_temp_2, aes(x=mammal.visits, ymin=lower, ymax=upper), alpha= 0.3, fill="grey") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="# of mammal visits", y="# of bird interactions (sqrt)")
plot2

library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

library(dplyr)

dat_sum <- read.csv("Kim_dat_plast_fruit Collapsed.csv")

shapiro.test(dat_sum$Canopy)         #Not normal
shapiro.test(log(dat_sum$GDP_capita))       #Normal
shapiro.test(dat_sum$mammal.visits^0.5)        #Normal
shapiro.test(log(dat_sum$Area))              #Normal
shapiro.test(dat_sum$Bird.diversity^-1)      #Normal
shapiro.test(dat_sum$per_built)           #Normal
shapiro.test(log(dat_sum$per_crops+0.001))        #Normal
shapiro.test(log(dat_sum$road_length))    #Normal
shapiro.test(dat_sum$near_length)         #Normal
shapiro.test(dat_sum$elevation_range)      #Normal
shapiro.test(log(dat_sum$number_of_tourists))       #Normal
shapiro.test(dat_sum$bird.visits^0.5)       #Not normal
shapiro.test(log(dat_sum$mass_effect))      #Not normal
shapiro.test(log(dat_sum$Age))             #Normal
shapiro.test(dat_sum$Elevation)            #Normal
shapiro.test(dat_sum$Dist_continent)        #Not normal
shapiro.test(dat_sum$Number_neighbors)     #Normal
shapiro.test(dat_sum$Reptile_diversity)     #Normal
shapiro.test(dat_sum$per_forest)     #Normal
shapiro.test(log(dat_sum$dist_road))     #Normal
shapiro.test(log(dat_sum$dist_built))     #Normal
shapiro.test(dat_sum$Native_plants)     #Normal
shapiro.test(dat_sum$per_built)         #Normal
shapiro.test(log(dat_sum$Interact))          #Normal

#transformations made: GDP_per capita (log), mammal visits (sqrt), Area (log), Bird richness (inverse), per crops (log), road length (log), number of tourists (log), Age (log), dist_road (log)
#transformations made although it doesn't make them fully normal: Bird visits (sqrt), mass effect (log)

dat_sum$log_GPC <- log(dat_sum$GDP_capita)
dat_sum$sqrt_mammal.visits <- dat_sum$mammal.visits^0.5
dat_sum$log_Area <- log(dat_sum$Area)
dat_sum$inv_bird_diversity <- dat_sum$Bird.diversity^-1
dat_sum$log_per_crop <- log(dat_sum$per_crops+0.001)
dat_sum$log_road_length <- log(dat_sum$road_length)
dat_sum$log_tourist <- log(dat_sum$number_of_tourists)
dat_sum$log_Age <- log(dat_sum$Age)
dat_sum$sqrt_bird_visits <- dat_sum$bird.visits^0.5
dat_sum$log_mass_effect <- log(dat_sum$mass_effect)
dat_sum$log_dist_road <- log(dat_sum$dist_road)
dat_sum$log_dist_built <- log(dat_sum$dist_built)
dat_sum$log_Interact <- log(dat_sum$Interact)

dat_sum_cor <- dplyr::select(dat_sum, Interact, Canopy, Lat, Elevation, Dist_continent, Number_neighbors, Reptile_diversity, per_built, log_per_crop, log_Area, log_Age, sqrt_mammal.visits, 
                             per_forest, inv_bird_diversity, log_GPC, log_dist_road, log_dist_built, Native_plants, log_road_length, near_length, elevation_range, log_tourist, sqrt_bird_visits, 
                             log_mass_effect)

dat_sum_cor <- sapply(dat_sum_cor, as.numeric)   #need to clean the data so everything is numeric

dat.cor = cor(dat_sum_cor) #Use correlation matrix of just independent variables to determine which ones to pick

write.csv(dat.cor, "correlation_matrix.csv")

dat_total <- dplyr::select(dat_sum, log_Interact, Interact,
                           Elevation,	Number_neighbors,	Reptile_diversity,	per_built,	sqrt_mammal.visits,	per_forest,	inv_bird_diversity,	log_GPC,	log_dist_road,	Native_plants,	near_length,	log_tourist,	sqrt_bird_visits)

#dat_total <- dplyr::select(dat_sum, Interact, 
#                           Elevation, Number_neighbors, Reptile_diversity, per_built, sqrt_mammal.visits, per_forest, inv_bird_diversity, log_GPC, log_dist_road, Native_plants, near_length, log_tourist)

fit_total <- glm(log_Interact ~ . , data = dat_total, na.action = na.fail)
summary(fit_total)

#Stepwise model selection
lm <- glm(log_Interact ~ 1, data=dat_total)
step(lm, scope= list(upper=glm(log_Interact~., data=dat_total)), direction = "forward")

#Step:  AIC=122.65
#Interact ~ sqrt_mammal.visits + Elevation + Reptile_diversity

forward_model <- lm(Interact ~ sqrt_mammal.visits + Elevation + Reptile_diversity, data=dat_total)
forward_model_2 <- lm(log_Interact ~ sqrt_mammal.visits + Elevation, data=dat_total)

lmfull <- glm(log_Interact~., data=dat_total)
step(lmfull, scope=list(lower=lm), direction = "backward")

backward_model <- glm(formula = log_Interact ~ Elevation + Number_neighbors + Reptile_diversity + 
                        sqrt_mammal.visits + inv_bird_diversity + log_GPC + log_dist_road + 
                        Native_plants + near_length + log_tourist, data = dat_total)

step(forward_model, scope=list(upper=lmfull, lower=lm), direction="both")
fit_model <- lm(log_Interact ~ sqrt_mammal.visits + Elevation + Reptile_diversity, data=dat_total)
summary(fit_model)
r.squaredGLMM(forward_model_2)   #R2 of 0.64

fit_model1 <- lm(log_Interact ~ sqrt_mammal.visits + sqrt_bird_visits + Elevation, data=dat_total)
summary(fit_model1)
r.squaredGLMM(fit_model1)

#model.sel(lm, forward_model_2, backward_model, fit_model)
model.sel(forward_model_2, backward_model, fit_model, fit_model1)

temp <- resid(forward_model_2)
shapiro.test(temp)   #Residuals are normal

summary(forward_model_2)
summary(fit_model)

library(relaimpo)
a1 <- calc.relimp(fit_model1, rela = TRUE)
a2 <- as.data.frame(a1@lmg)
a2

library(car)
vif(fit_model)

######### Mammal visits
fit_total <- lm(sqrt_mammal.visits ~ . - Interact, - log_Interact , data = dat_total, na.action = na.fail)
summary(fit_total)

#Stepwise model selection
lm <- lm(sqrt_mammal.visits ~ 1, data=dat_total)
step(lm, scope= list(upper=glm(sqrt_mammal.visits~. - Interact, data=dat_total)), direction = "forward")

#Step:  AIC=122.65
#Interact ~ sqrt_mammal.visits + Elevation + Reptile_diversity

forward_model <- lm(formula = sqrt_mammal.visits ~ near_length + log_dist_road, 
                    data = dat_total)

forward_model_2 <- lm(sqrt_mammal.visits ~ near_length, data=dat_total)

lmfull <- lm(sqrt_mammal.visits~. - Interact, data=dat_total)
step(lmfull, scope=list(lower=lm), direction = "backward")

backward_model <- lm(formula = sqrt_mammal.visits ~ Elevation + Number_neighbors + 
                       Reptile_diversity + per_forest + inv_bird_diversity + log_tourist, 
                     data = dat_total)

step(forward_model, scope=list(upper=lmfull, lower=lm), direction="both")
fit_model <- lm(sqrt_mammal.visits ~ near_length + log_dist_road, data=dat_total)
summary(fit_model)
summary(forward_model_2)

model.sel(lm, forward_model_2, backward_model, fit_model)

temp <- resid(fit_model)
shapiro.test(temp)    #Residuals are normal

library(relaimpo)
a1 <- calc.relimp(fit_model, rela = TRUE)
a2 <- as.data.frame(a1@lmg)
a2

vif(fit_model)
