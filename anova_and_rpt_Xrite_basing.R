#digital images standardised with two Xrite grey patches: 1.99 and 92.96 % reflectance

library(readxl)
library(dplyr)
library(ggplot2)
library(ggsignif)
library(ggpubr)
library(rstatix)
library(xlsx)
library(stringr)
library(rptR)

# EGGS ----------------------------------------------------------

## prepare df ---------------------------------------------------

setwd("D:/Science/Eggs/PhD/variable_light/photos/measurements/Xrite_basing") #adjust if other

### eggs photographed in different illumination ------------------------------
### Eggs were photographed in natural outdoor conditions during sunny and uniformly overcast weather and at different elevation angle of sun: 10, 20, 30, 40 and 55 degrees

df_eggs <- read_excel("eggs_xrite_with_granularity.xlsx") %>%
  filter(str_detect(Label, "egg"))


egg_ID <- rep(1:36, times = 10)
cloud <- rep("cloud", times = 180)
sun <- rep("sun", times = 180)
weather <- c(cloud, sun)
degrees <- rep(rep(c(10,20,30,40,55), each = 36), times = 2)

df_eggs$egg_ID <- egg_ID
df_eggs$weather <- weather
df_eggs$degrees <- degrees

eggs <- df_eggs %>%
  transmute(egg_ID = as.factor(egg_ID),
            weather = as.factor(weather),
            degrees = as.factor(degrees),
            R = as.numeric(`visible:R:NormalisedMean`),
            G = as.numeric(`visible:G:NormalisedMean`),
            B = as.numeric(`visible:B:NormalisedMean`),
            sumPower = as.numeric(sumPower),
            maxPower = as.numeric(maxPower),
            propPower = as.numeric(propPower),
            maxFreq = as.numeric(maxFreq)) %>%
  mutate(brightness = (R+G+B),
         transformed_brightness = sqrt(brightness),
         Rchroma = R / (R+G+B),
         transformed_propPower = 1/(propPower),
         logMaxFreq = log2(maxFreq),
         level = as.factor(paste(as.character(weather), as.character(degrees), sep = "")))

### eggs human cone catch - RG opponency basing on DeLacey et al. 2022 ---------------------------------
### Eggs photographed in natural outdoor conditions (as above), but converted to human cone-catch model

df_eggs_human <- read_excel("eggs_xrite_humanConeModel.xlsx") %>%
  filter(str_detect(Label, "egg"))


egg_ID <- rep(1:36, times = 10)
cloud <- rep("cloud", times = 180)
sun <- rep("sun", times = 180)
weather <- c(cloud, sun)
degrees <- rep(rep(c(10,20,30,40,55), each = 36), times = 2)

df_eggs_human$egg_ID <- egg_ID
df_eggs_human$weather <- weather
df_eggs_human$degrees <- degrees

eggs_human <- df_eggs_human %>%
  transmute(egg_ID = as.factor(egg_ID),
            weather = as.factor(weather),
            degrees = as.factor(degrees),
            lum = as.numeric(lumMean),
            lw = as.numeric(lwMean),
            mw = as.numeric(mwMean),
            sw = as.numeric(swMean),
            RGopponency = (lw - mw) / (lw + mw)) %>%
  mutate(level = as.factor(paste(as.character(weather), as.character(degrees), sep = "")))


### eggs in artificial light - two samples -------------------------------------
### A subsample of 18 eggs photographed twice in artificial light conditions (Iwasaki eyeColour arc lamp MT70D E27 6500K)

df_artificial_twoSamples <- read_excel("eggs_artificial_xrite.xlsx") %>%
  filter(str_detect(Label, "egg"))

df_artificial_twoSamples <- df_artificial_twoSamples[c(1:18, 37:54),]
df_artificial_twoSamples$egg_ID <- rep(1:18, times = 2)
df_artificial_twoSamples$sample <- rep(c("sample1", "sample2"), each = 18)

eggs_twoSamples <- df_artificial_twoSamples %>%
  transmute(egg_ID = as.factor(egg_ID),
            sample = as.factor(sample),
            R = as.numeric(`visible:R:NormalisedMean`),
            G = as.numeric(`visible:G:NormalisedMean`),
            B = as.numeric(`visible:B:NormalisedMean`),
            sumPower = as.numeric(sumPower),
            maxPower = as.numeric(maxPower),
            propPower = as.numeric(propPower),
            maxFreq = as.numeric(maxFreq)) %>%
  mutate(brightness = (R+G+B),
         transformed_brightness = sqrt(brightness),
         Rchroma = R / (R+G+B),
         RG = R/G,
         RGopponency = (R-G) / (R+G),
         logMaxFreq = log2(maxFreq))


### eggs in artificial light - human cone catch - two samples ---------------------------------
### A subsample of 18 eggs photographed twice in artificial light conditions (Iwasaki eyeColour arc lamp MT70D E27 6500K) and converted into human cone-catch model

df_eggs_artificial_human <- read_excel("eggs_artificial_xrite_humanConeModel.xlsx") %>%
  filter(str_detect(Label, "egg"))

df_artificial_human_twoSamples <- df_eggs_artificial_human[c(1:18, 37:54),]
df_artificial_human_twoSamples$egg_ID <- rep(1:18, times = 2)
df_artificial_human_twoSamples$sample <- rep(c("sample1", "sample2"), each = 18)

eggs_human_twoSamples <- df_artificial_human_twoSamples %>%
  transmute(egg_ID = as.factor(egg_ID),
            sample = as.factor(sample),
            lw = as.numeric(lwMean),
            mw = as.numeric(mwMean)) %>%
  mutate(RGopponency = (lw-mw) / (lw+mw))


## repeated-measures ANOVA -----------------------------------------------
## We used repeated-measures ANOVA to check how comparable measurements taken under different illumination are

### brightness ----------------------------------------------

###ASSUMPTIONS OF REPEATED MEASURE ANOVA

###1.Dependent variable should be normally distributed in each cell of the design

eggs %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(transformed_brightness) #normal distribution in all cells after sqrt() transformation

ggpubr:: ggqqplot(eggs, "transformed_brightness", facet.by = "level")
ggpubr:: gghistogram(eggs, "transformed_brightness", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(transformed_brightness) # no outliers at all

ggpubr:: ggboxplot(eggs, x = "degrees", y = "transformed_brightness", color = "weather")
ggpubr:: ggboxplot(eggs, x = "degrees", y = "brightness", color = "weather")


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_brightness <- rstatix::anova_test(data = eggs,
                                        dv = transformed_brightness,
                                        wid = egg_ID,
                                        within = c(weather, degrees))

df_anova_brightness <- rstatix:: get_anova_table(anova_brightness)
write.xlsx(df_anova_brightness, "df_anova_brightness.xlsx")

### PAIR-WISE COMPARISONS

df_effectDegrees_brightness <- eggs %>% #it checks if there is an effect of degrees for every weather level separately (and let to compare if effect of degree is higher for one weather level than for another)
  group_by(weather) %>%
  anova_test(dv = transformed_brightness,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_brightness

write.xlsx(df_effectDegrees_brightness, "df_effectDegrees_brightness.xlsx")


brightness_pair1 <- eggs %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = transformed_brightness ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(brightness_pair1)

brightness_pair2 <- eggs %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = transformed_brightness ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(brightness_pair2)


### Rchroma ----------------------------------------------------

##ASSUMPTIONS OF REPEATED MEASURE ANOVA

###1.Dependent variable should be normally distributed in each cell of the design

eggs %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(Rchroma) #normal distribution in all cells

ggpubr:: ggqqplot(eggs, "Rchroma", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(Rchroma) # one outlier, but not extreme


ggpubr:: ggboxplot(eggs, x = "degrees", y = "Rchroma", color = "weather")


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_Rchroma <- rstatix::anova_test(data = eggs,
                                     dv = Rchroma,
                                     wid = egg_ID,
                                     within = c(weather, degrees))

df_anova_Rchroma <- rstatix:: get_anova_table(anova_Rchroma)
xlsx::write.xlsx(df_anova_Rchroma, "df_anova_Rchroma.xlsx")


### PAIR-WISE COMPARISONS

df_effectDegrees_Rchroma <- eggs %>%
  group_by(weather) %>%
  anova_test(dv = Rchroma,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_Rchroma

write.xlsx(df_effectDegrees_Rchroma, "df_effectDegrees_Rchroma.xlsx")


Rchroma_pair1 <- eggs %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = Rchroma ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(Rchroma_pair1)

Rchroma_pair2 <- eggs %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = Rchroma ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(Rchroma_pair2)


### HUMAN RGopponency - basing on lw and mw cone catch--------------------------------------------

##ASSUMPTIONS OF REPEATED MEASURE ANOVA

###1.Dependent variable should be normally distributed in each cell of the design

eggs_human %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(RGopponency) #normal distribution in all cells

ggpubr:: ggqqplot(eggs_human, "RGopponency", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs_human %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(RGopponency) # one outlier - not extreme

ggpubr:: ggboxplot(eggs_human, x = "degrees", y = "RGopponency", color = "weather")


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_human_RGopponency <- rstatix::anova_test(data = eggs_human,
                                         dv = RGopponency,
                                         wid = egg_ID,
                                         within = c(weather, degrees))

df_anova_human_RGopponency <- rstatix:: get_anova_table(anova_human_RGopponency)
xlsx::write.xlsx(df_anova_human_RGopponency, "df_anova_human_RGopponency.xlsx")

### PAIR-WISE COMPARISONS

df_effectDegrees_humanRGoppoenency <- eggs_human %>%
  group_by(weather) %>%
  anova_test(dv = RGopponency,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_humanRGoppoenency

write.xlsx(df_effectDegrees_humanRGoppoenency, "df_effectDegrees_humanRGoppoenency.xlsx")


RGopponency_human_pair1 <- eggs_human %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = RGopponency ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(RGopponency_human_pair1)

RGopponency_human_pair2 <- eggs_human %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = RGopponency ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(RGopponency_human_pair2)

### sumPower -----------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7) #egg 7 is outlier

##ASSUMPTIONS OF REPEATED MEASURE ANOVA

###1.Dependent variable should be normally distributed in each cell of the design

eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(sumPower) #normal distribution in all cells

ggpubr:: ggqqplot(eggs_filtered, "sumPower", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(sumPower) # 13 outliers (eggs 6 and 7), but no one is extreme -> egg 7 removed to acquire normal distribution

ggpubr:: ggboxplot(eggs_filtered, x = "degrees", y = "sumPower", color = "weather")


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_sumPower <- rstatix::anova_test(data = eggs_filtered,
                                      dv = sumPower,
                                      wid = egg_ID,
                                      within = c(weather, degrees))

df_anova_sumPower <- rstatix:: get_anova_table(anova_sumPower)
write.xlsx(df_anova_sumPower, "df_anova_sumPower.xlsx")

### PAIR-WISE COMPARISONS

df_effectDegrees_sumPower <- eggs_filtered %>%
  group_by(weather) %>%
  anova_test(dv = sumPower,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_sumPower

write.xlsx(df_effectDegrees_sumPower, "df_effectDegrees_sumPower.xlsx")


sumPower_pair1 <- eggs_filtered %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = sumPower ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(sumPower_pair1)

sumPower_pair2 <- eggs_filtered %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = sumPower ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(sumPower_pair2)


### maxPower ------------------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7) #egg 7 is outlier

###1.Dependent variable should be normally distributed in each cell of the design

eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(maxPower) # when egg no 7 was filtered out -> distribution in all cells is normal

ggpubr:: ggqqplot(eggs, "maxPower", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(maxPower) # after egg 7 was removed only two outliers - not extreme (egg 7 was removed to acquire normal distribution)


ggpubr:: ggboxplot(eggs_filtered, x = "degrees", y = "maxPower", color = "weather") #one outlier - no extreme


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_maxPower <- rstatix::anova_test(data = eggs_filtered,
                                      dv = maxPower,
                                      wid = egg_ID,
                                      within = c(weather, degrees))

df_anova_maxPower <- rstatix:: get_anova_table(anova_maxPower)
write.xlsx(df_anova_maxPower, "df_anova_maxPower.xlsx")


### PAIR-WISE COMPARISONS

df_effectDegrees_maxPower <- eggs_filtered %>%
  group_by(weather) %>%
  anova_test(dv = maxPower,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_maxPower

write.xlsx(df_effectDegrees_maxPower, "df_effectDegrees_maxPower.xlsx")


maxPower_pair1 <- eggs_filtered %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = maxPower ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(maxPower_pair1)

maxPower_pair2 <- eggs_filtered %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = maxPower ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(maxPower_pair2)


### propPower --------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 12) #removing one outlier
 
###1.Dependent variable should be normally distributed in each cell of the design

eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix:: shapiro_test(transformed_propPower) # in two cells still not normal distribution (but close)

ggpubr:: ggqqplot(eggs_filtered, "transformed_propPower", facet.by = "level")
ggpubr:: ggdensity(eggs_filtered, "transformed_propPower", facet.by = "level")

###2. No extreme outliers in each cell

outliers <- eggs_filtered %>%
  group_by(weather, degrees) %>%
  rstatix::identify_outliers(transformed_propPower) #egg 12 removed to acquire (more) normal distribution

ggpubr:: ggboxplot(eggs_filtered, x = "degrees", y = "transformed_propPower", color = "weather") #no outliers


###3. sphericity - deviation from sphericity is correcred by the function "get_anova_table()" (Greenhouse-Geisser sphericity correction)

anova_propPower <- rstatix::anova_test(data = eggs_filtered,
                                       dv = transformed_propPower,
                                       wid = egg_ID,
                                       within = c(weather, degrees))

df_anova_propPower <- rstatix:: get_anova_table(anova_propPower)
write.xlsx(df_anova_propPower, "df_anova_propPower.xlsx")


### PAIR-WISE COMPARISONS

df_effectDegrees_propPower <- eggs_filtered %>%
  group_by(weather) %>%
  anova_test(dv = transformed_propPower,
             wid = egg_ID,
             within = degrees) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")

df_effectDegrees_propPower

write.xlsx(df_effectDegrees_propPower, "df_effectDegrees_propPower.xlsx")


propPower_pair1 <- eggs_filtered %>%
  group_by(weather) %>%
  rstatix::pairwise_t_test(formula = transformed_propPower ~ degrees, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(propPower_pair1)

propPower_pair2 <- eggs_filtered %>%
  group_by(degrees) %>%
  rstatix::pairwise_t_test(formula = transformed_propPower ~ weather, 
                           paired = TRUE, 
                           p.adjust.method = "bonferroni")

data.frame(propPower_pair2)


### maxFreq - Friedman test -----------------------------------------
### on log values of maxFreq to return to a linear scale

ggpubr:: ggqqplot(eggs, "logMaxFreq", facet.by = "level")
ggpubr::ggdensity(eggs, "logMaxFreq", facet.by = "degrees", color = "weather")
ggpubr:: gghistogram(eggs, "logMaxFreq", facet.by = "degrees", color = "weather")
  

friedman.test(formula = logMaxFreq ~ level | egg_ID, data = eggs)

eggs %>% 
  friedman_effsize(logMaxFreq ~ level | egg_ID) #small effect of illumination

PMCMRplus:: frdAllPairsConoverTest(y = eggs$logMaxFreq, 
                                   groups = eggs$level,
                                   blocks = eggs$egg_ID,
                                   p.adjust.method = "bonf") #no differences, but non-parametric tests have usually smaller power to detect differences


## REPEATABILITY ----------------------------------------------------
## Calculated as inter-correlation coefficient using LMM-basing approach for Gaussian data in rptR package (Nakagawa and Schielzeth, 2010; Stoffel et al., 2017)

eggs_sun <- eggs %>%
  filter(weather == "sun")

eggs_cloud <- eggs %>%
  filter(weather == "cloud")

### brightness ---------------------------------------------------

rep_brightness <- rpt(transformed_brightness ~ (1 | egg_ID), grname = "egg_ID", data = eggs, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_brightness) # R = 0.736, CI: 0.624-0.812

rep_brightness_sun <- rpt(transformed_brightness ~ (1 | egg_ID), grname = "egg_ID", data = eggs_sun, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_brightness_sun) # R = 0.689, CI: 0.537-0.797

rep_brightness_cloud <- rpt(transformed_brightness ~ (1 | egg_ID), grname = "egg_ID", data = eggs_cloud, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_brightness_cloud) # R = 0.879, CI: 0.810-0.921

### Rchroma ------------------------------------------------------

rep_Rchroma<- rpt(Rchroma ~ (1 | egg_ID), grname = "egg_ID", data = eggs, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_Rchroma) # R = 0.918, CI: 0.862-0.945

rep_Rchroma_sun <- rpt(Rchroma ~ (1 | egg_ID), grname = "egg_ID", data = eggs_sun, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_Rchroma_sun) # R = 0.873, CI: 0.793-0.914

rep_Rchroma_cloud <- rpt(Rchroma ~ (1 | egg_ID), grname = "egg_ID", data = eggs_cloud, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_Rchroma_cloud) # R = 0.984, CI: 0.971-0.990


### HUMAN RGopponency ---------------------------------------------------

eggs_human_sun <- eggs_human %>%
  filter(weather == "sun")

eggs_human_cloud <- eggs_human %>%
  filter(weather == "cloud")

rep_RGopponency_human <- rpt(RGopponency ~ (1 | egg_ID), grname = "egg_ID", data = eggs_human, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_RGopponency_human) # R = 0.795, CI: 0.680-0.861

rep_RGopponency_human_sun <- rpt(RGopponency ~ (1 | egg_ID), grname = "egg_ID", data = eggs_human_sun, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_RGopponency_human_sun) # R = 0.703, CI: 0.545-0.801

rep_RGopponency_human_cloud <- rpt(RGopponency ~ (1 | egg_ID), grname = "egg_ID", data = eggs_human_cloud, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_RGopponency_human_cloud) # R = 0.950, CI: 0.915-0.969


### sumPower ------------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7)

eggs_sun_filtered <- eggs_filtered %>%
  filter(weather == "sun")

eggs_cloud_filtered <- eggs_filtered %>%
  filter(weather == "cloud")

rep_sumPower <- rpt(sumPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_sumPower) # R = 0.416, CI: 0.292-0.536

rep_sumPower_sun <- rpt(sumPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_sun_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_sumPower_sun) # R = 0.347, CI: 0.175-0.494

rep_sumPower_cloud <- rpt(sumPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_cloud_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_sumPower_cloud) # R = 0.608, CI: 0.460-0.733

### maxPower -------------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7)

eggs_sun_filtered <- eggs_filtered %>%
  filter(weather == "sun")

eggs_cloud_filtered <- eggs_filtered %>%
  filter(weather == "cloud")

rep_maxPower <- rpt(maxPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_maxPower) # R = 0.612, CI: 0.462-0.719

rep_maxPower_sun <- rpt(maxPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_sun_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_maxPower_sun) # R = 0.558, CI: 0.377-0.695

rep_maxPower_cloud <- rpt(maxPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_cloud_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_maxPower_cloud) # R = 0.794, CI: 0.690-0.867

### propPower ------------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 12) #removing one outlier

eggs_sun_filtered <- eggs_filtered %>%
  filter(weather == "sun")

eggs_cloud_filtered <- eggs_filtered %>%
  filter(weather == "cloud")

rep_propPower <- rpt(propPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_propPower) # R = 0.887, CI: 0.811-0.925

rep_propPower_sun <- rpt(propPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_sun_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_propPower_sun) # R = 0.896, CI: 0.822-0.932

rep_propPower_cloud <- rpt(propPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_cloud_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_propPower_cloud) # R = 0.888, CI: 0.815-0.927


## REPEATABILITY - TWO SAMPLES -----------------------------------------

eggs_twoSamples %>%
  group_by(sample) %>%
  rstatix:: shapiro_test(brightness, Rchroma, sumPower, maxPower, propPower)

outliers <- eggs_twoSamples %>%
  group_by(sample) %>%
  rstatix::identify_outliers(propPower) #egg7 is outlier in sumPower and maxPower; egg12 is outlier in propPower

eggs_twoSamples %>%
  filter(egg_ID != 12) %>%
  group_by(sample) %>%
  rstatix::shapiro_test(propPower) #after removing egg12, distribution is normal


### brightness ---------------------------------------------

rep_brightness_artificial <- rpt(brightness ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_brightness_artificial) # R = 0.994, CI: 0.985-0.998

### Rchroma ----------------------------------------------

rep_Rchroma_artificial <- rpt(Rchroma ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_Rchroma_artificial) # R = 0.998, CI: 0.995-0.999

### RGopponency -------------------------------------------

rep_RGopponency_artificial <- rpt(RGopponency ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_RGopponency_artificial) # R = 0.998, CI: 0.995 - 0.999

### HUMAN RGopponency ------------------------------------

rep_human_RGopponency_artificial <- rpt(RGopponency ~ (1 | egg_ID), grname = "egg_ID", data = eggs_human_twoSamples, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_human_RGopponency_artificial) # R = 0.995, CI: 0.987 - 0.998

### sumPower ----------------------------------------------

eggs_twoSamples_filtered <- eggs_twoSamples %>%
  filter(egg_ID != 7)

rep_sumPower_artificial <- rpt(sumPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_sumPower_artificial) # R = 0.980, CI: 0.940 - 0.993

### maxPower -------------------------------------------

eggs_twoSamples_filtered <- eggs_twoSamples %>%
  filter(egg_ID != 7)

rep_maxPower_artificial <- rpt(maxPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_maxPower_artificial) # R = 0.942, CI: 0.853-0.978

### propPower -----------------------------------------

eggs_twoSamples_filtered <- eggs_twoSamples %>%
  filter(egg_ID != 12)

rep_propPower_artificial <- rpt(propPower ~ (1 | egg_ID), grname = "egg_ID", data = eggs_twoSamples_filtered, datatype = "Gaussian", nboot = 500, npermut = 0)
print(rep_propPower_artificial) # R = 0.907, CI: 0.759-0.959


## PLOTS with pair-wise comparisons -----------------------------------------------------------------

### brightness --------------------------------------------

g1 <- ggplot(eggs, aes(y = transformed_brightness, x = degrees, fill = weather)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  theme_bw() +
  ylim(c(4, 12)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "40")),
    annotations = "ns",
    y_position = 11.2,
    xmin = 0.8,
    xmax = 3.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("10", "55")),
    annotations = "ns",
    y_position = 11.8,
    xmin = 0.8,
    xmax = 4.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("40", "55")),
    annotations = "ns",
    y_position = 10.7,
    xmin = 3.8,
    xmax = 4.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 4.8,
    xmin = 2.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.5,
    color = "orange") +
  geom_signif(#comparisons = list(c("30", "30")),
              annotations = "ns",
              y_position = 9.9,
              xmin = 2.9,
              xmax = 3.1,
              tip_length = 0.05,
              color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "brightness (square root)") +
  theme(legend.position = "empty")

g1

### Rchroma -----------------------------------------------

g2 <- ggplot(eggs, aes(y = Rchroma, x = degrees, fill = weather)) +
  geom_boxplot() +
  theme_bw() +
  ylim(c(0.25, 0.6)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "20")),
    annotations = "ns",
    y_position = 0.51,
    xmin = 0.8,
    xmax = 1.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 0.57,
    xmin = 1.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("30", "40")),
    annotations = "ns",
    y_position = 0.51,
    xmin = 2.8,
    xmax = 3.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("30", "55")),
    annotations = "ns",
    y_position = 0.54,
    xmin = 2.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("10", "55")),
    annotations = "ns",
    y_position = 0.27,
    xmin = 1.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.0,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "30")),
    annotations = "ns",
    y_position = 0.36,
    xmin = 2.2,
    xmax = 3.2,
    tip_length = -0.05,
    vjust = 2.0,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 0.3,
    xmin = 2.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.0,
    color = "orange") +
  geom_signif(#comparisons = list(c("30", "55")),
    annotations = "ns",
    y_position = 0.34,
    xmin = 3.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.0,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "20")),
    annotations = "ns",
    y_position = 0.51,
    xmin = 1.9,
    xmax = 2.1,
    tip_length = 0.05,
    color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "red chroma") +
  theme(legend.position = "empty")

g2


### HUMAN RGopponency --------------------------------------------------------

g3 <- ggplot(eggs_human, aes(y = RGopponency, x = degrees, fill = weather)) +
  geom_boxplot() +
  theme_bw() +
  ylim(c(-0.015, 0.09)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "20")),
    annotations = "ns",
    y_position = 0.06,
    xmin = 0.8,
    xmax = 1.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("10", "55")),
    annotations = "ns",
    y_position = 0.086,
    xmin = 0.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "40")),
    annotations = "ns",
    y_position = 0.070,
    xmin = 1.8,
    xmax = 3.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 0.078,
    xmin = 1.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("30", "40")),
    annotations = "ns",
    y_position = 0.06,
    xmin = 2.8,
    xmax = 3.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("40", "55")),
    annotations = "ns",
    y_position = 0.066,
    xmin = 3.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = 0,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("10", "20")),
    annotations = "ns",
    y_position = 0.012,
    xmin = 1.2,
    xmax = 2.2,
    tip_length = -0.05,
    vjust = 2.2,
    color = "orange") +
  geom_signif(#comparisons = list(c("10", "30")),
    annotations = "ns",
    y_position = 0.005,
    xmin = 1.2,
    xmax = 3.2,
    tip_length = -0.05,
    vjust = 2.2,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "30")),
    annotations = "ns",
    y_position = 0.012,
    xmin = 2.2,
    xmax = 3.2,
    tip_length = -0.05,
    vjust = 2.2,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = -0.006,
    xmin = 2.2,
    xmax = 5.2,
    tip_length = -0.04,
    vjust = 2.2,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "20")),
              annotations = "ns",
              y_position = 0.06,
              xmin = 1.9,
              xmax = 2.1,
              tip_length = 0.05,
              color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "human red-green opponency") +
  theme(legend.position = "empty")

g3

### sumPower ------------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7)

g4 <- ggplot(eggs_filtered, aes(y = sumPower, x = degrees, fill = weather)) +
  geom_boxplot() +
  theme_bw() +
  ylim(c(0, 40)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "40")),
    annotations = "ns",
    y_position = 35,
    xmin = 0.8,
    xmax = 3.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("10", "55")),
    annotations = "ns",
    y_position = 38,
    xmin = 0.8,
    xmax = 4.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("40", "55")),
    annotations = "ns",
    y_position = 33,
    xmin = 3.8,
    xmax = 4.8,
    tip_length = 0.05,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "30")),
    annotations = "ns",
    y_position = 10,
    xmin = 2.2,
    xmax = 3.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 3,
    xmin = 2.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
    geom_signif(#comparisons = list(c("30", "55")),
    annotations = "ns",
    y_position = 7,
    xmin = 3.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
  geom_signif(#comparisons = list(c("10", "10")),
    annotations = "ns",
    y_position = 30,
    xmin = 0.9,
    xmax = 1.1,
    tip_length = 0.05,
    color = "black") +
  geom_signif(#comparisons = list(c("30", "30")),
              annotations = "ns",
              y_position = 26,
              xmin = 2.9,
              xmax = 3.1,
              tip_length = 0.05,
              color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "sumPower") +
  theme(legend.position = "empty")

g4

### maxPower -------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 7)

g5 <- ggplot(eggs_filtered, aes(y = maxPower, x = degrees, fill = weather)) +
  geom_boxplot() +
  theme_bw() +
  ylim(c(0, 4.6)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "40")),
    annotations = "ns",
    y_position = 4.2,
    xmin = 0.8,
    xmax = 3.8,
    tip_length = 0.05,
    vjust = -0.1,
    color = "royalblue1") +
   geom_signif(#comparisons = list(c("40", "55")),
    annotations = "ns",
    y_position = 3.8,
    xmin = 3.8,
    xmax = 4.8,
    tip_length = 0.05,
    vjust = -0.1,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "30")),
    annotations = "ns",
    y_position = 0.9,
    xmin = 2.2,
    xmax = 3.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
  geom_signif(#comparisons = list(c("20", "55")),
    annotations = "ns",
    y_position = 0.2,
    xmin = 2.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
  geom_signif(#comparisons = list(c("30", "55")),
    annotations = "ns",
    y_position = 0.7,
    xmin = 3.2,
    xmax = 5.2,
    tip_length = -0.05,
    vjust = 2.3,
    color = "orange") +
  geom_signif(#comparisons = list(c("10", "10")),
    annotations = "ns",
    y_position = 3.4,
    xmin = 0.9,
    xmax = 1.1,
    tip_length = 0.05,
    vjust = -0.1,
    color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "maxPower") +
  theme(legend.position = "empty")

g5

###propPower ------------------------------------------

eggs_filtered <- eggs %>%
  filter(egg_ID != 12) #removing one outlier

g6 <- ggplot(eggs_filtered, aes(y = transformed_propPower, x = degrees, fill = weather)) +
  geom_boxplot() +
  theme_bw() +
  ylim(c(5, 15)) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  geom_signif(#comparisons = list(c("10", "30")),
    annotations = "*",
    y_position = 5.5,
    xmin = 0.8,
    xmax = 2.8,
    tip_length = -0.05,
    vjust = 2.3,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("20", "30")),
    annotations = "*",
    y_position = 6.7,
    xmin = 1.8,
    xmax = 2.8,
    tip_length = -0.05,
    vjust = 2.3,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("30", "55")),
    annotations = "*",
    y_position = 6.1,
    xmin = 2.8,
    xmax = 4.8,
    tip_length = -0.05,
    vjust = 2.3,
    color = "royalblue1") +
  geom_signif(#comparisons = list(c("40", "55")),
    annotations = "*",
    y_position = 13.2,
    xmin = 4.2,
    xmax = 5.2,
    tip_length = 0.05,
    vjust = 0.4,
    color = "orange") +
  geom_signif(#comparisons = list(c("30", "30")),
              annotations = "**",
              y_position = 12.4,
              xmin = 2.9,
              xmax = 3.1,
              tip_length = 0.05,
              vjust = 0.4,
              color = "black") +
  geom_signif(#comparisons = list(c("55", "55")),
              annotations = "*",
              y_position = 12.7,
              xmin = 4.9,
              xmax = 5.1,
              tip_length = 0.05,
              vjust = 0.4,
              color = "black") +
  labs(x = "height of the sun (in degrees)",
       y = "propPower (reciprocal)") +
  theme(legend.position = "empty")

g6

### arrange all plots --------------------------------

plots <- ggarrange(g1, g2, g3, g4, g5, g6,
                   ncol = 2, nrow = 3) +  bgcolor("white")
plots

ggsave("comparisons_xrite-basing_humanRGopponency_final.png", plots, height = 8000, width = 6000, units = "px", dpi = 600)

### maxFreq -------------------------------------------

g <- ggplot(eggs, aes(x = logMaxFreq, fill = weather)) +
  geom_bar(alpha = 0.7, position = position_dodge(width = 0.25)) +
  facet_wrap(~degrees, ncol = 2) +
  scale_fill_manual(values = c("royalblue1", "orange")) +
  theme_bw() +
  theme(legend.position = "bottom",
        title = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),) +
  labs(title = "Comparison of different spot size categories occurences \n measured in different light conditions",
       x = "spot size category",
       y = "number of occurences of spot size categories",
       caption = "spot size category = log2(maxFreq)")

g 

ggsave("maxFreq_comparison.png", plot = g, width = 2000, height = 2500, units = "px", dpi = 400)


## PLOT - repeatability ----------------------------------------------------

rpt <- read_excel("repeatability/repeatability_results_XRite_basing.xlsx") %>%
  transmute(weather = factor(`light conditions`, levels = c("natural", "sunny", "cloudy", "artificial")),
            trait = factor(trait, levels = c("brightness", "red chroma", "human red-green opponency", "sumPower", "maxPower", "propPower")),
            repeatability = as.numeric(repeatability),
            lowerCI = as.numeric(lowerCI),
            upperCI = as.numeric(upperCI))
  
g_rep <- ggplot(data = rpt, aes(y = repeatability, x = weather, colour = weather)) +
  geom_errorbar(ymin = rpt$lowerCI, ymax = rpt$upperCI, size = 0.8, width = 0.2, colour = c("grey30")) +
  geom_point(size = 1.5, alpha = 0.8) +
  facet_wrap(~ trait, ncol = 2) +
  ylim(0,1) +
  theme_bw() +
  scale_color_manual(values = c("yellowgreen", "orange1", "lightskyblue3", "grey50"), name = "Light conditions:") +
  labs(title = "Repeatability of different traits of eggshell pigmentation \nmeasured in different light conditions",
       x = "Light conditions",
       y = "Repeatability with 95% CL",
       legend = "x") +
  theme(title = element_text(size = 9),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

g_rep

ggsave("repeatability_xrite_basing_humanRGopponency.png", plot = g_rep, width = 2000, height = 2500, units = "px", dpi = 400)
