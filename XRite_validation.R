# EGGS AND VARIABLE LIGHT
# ADDITIONAL ANALYSIS AFTER REVIEWS - HOW LIGHT AFFECTS GREY AND COLOUR STANDARDS?
# 2023-03-27

library(dplyr)
library(xlsx)
library(ggplot2)
library(stringr)


# COLOUR X-RITE PATCHES --------------------------------------------------------

  ## prepare colour squares database ----------------------------------------------

  setwd("D:/Science/Eggs/PhD/variable_light/photos/measurements/Xrite_basing") #adjust if other
  
  standards <- read.xlsx("eggs_xrite_with_granularity.xlsx", sheetIndex = 1) %>%
    filter(str_detect(Label, "_b")) %>%
    transmute(Label = as.character(Label),
              R = as.numeric(`visible.R.NormalisedMean`),
              G = as.numeric(`visible.G.NormalisedMean`),
              B = as.numeric(`visible.B.NormalisedMean`)) %>%
    mutate(brightness = R + G + B,
           Rchroma = R / brightness)
  
  standards$weather <- as.factor(c(rep("cloud", times = 90), rep("sun", times = 90)))
  standards$degrees <- as.factor(rep(rep(c(10,20,30,40,55), each = 18), times = 2))
  standards$level <- as.factor(paste(standards$weather, standards$degrees, sep = ""))
  standards$standard_ID <- as.factor(rep(c("blue", "green", "red"), times = 60))
  
  standards <- standards[,c(10,7:9,1:6)]
  str(standards)
  
  standards_human <- read.xlsx("eggs_xrite_humanConeModel.xlsx", sheetIndex = 1) %>%
    filter(str_detect(Label, "_b")) %>%
    transmute(Label = as.character(Label),
              lw = as.numeric(`lwMean`),
              mw = as.numeric(`mwMean`)) %>%
    mutate(RGopponency = (lw - mw) / (lw + mw))
  
  standards_all <- standards %>%
    inner_join(standards_human, by = "Label") %>%
    select(-Label, -R, -G, -B, -lw, -mw)
  

  ## calculate means and sd --------------------------------------------

  means_levels <- standards_all %>%
    group_by(level, standard_ID) %>%
    summarise_at(vars(brightness, Rchroma, RGopponency), funs(mean, sd)) %>%
    arrange(desc(standard_ID)) %>%
    ungroup()
  
  means_levels <- means_levels[,c(1,2,3,6,4,7,5,8)]
  
  write.xlsx(x = means_levels, file = "colour_squares_comparison.xlsx", sheetName = "Sheet1")

  means <- standards_all %>%
    group_by(standard_ID) %>%
    summarise_at(vars(brightness, Rchroma, RGopponency), funs(mean, sd))%>%
    arrange(desc(standard_ID))
  
  means <- means[,c(1,2,5,3,6,4,7)]

  
  ## visualise ----------------------------------------------------------------------

    ### brightness --------------------------------------------------
    
    g_brightness <- ggplot(standards_all, aes(x = level, y = brightness)) +
      geom_boxplot() +
      geom_point(size = 1, alpha = 0.3) +
      geom_hline(data = means, aes(yintercept = brightness_mean), colour = "grey50", linetype = "dashed") +
      facet_wrap(~standard_ID) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 8)) +
      labs(y = "brightness",
           x = "weather and elevation angle of the sun")
    
    ### Rchroma ---------------------------------------------------
    
    g_Rchroma <- ggplot(standards_all, aes(x = level, y = Rchroma)) +
      geom_boxplot() +
      geom_point(size = 1, alpha = 0.3) +
      geom_hline(data = means, aes(yintercept = Rchroma_mean), colour = "grey50", linetype = "dashed") +
      facet_wrap(~standard_ID) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 8)) +
      labs(y = "red chroma",
           x = "weather and elevation angle of the sun")
      
    ### RGopponency ----------------------------------------------
  
    g_RGopponency <- ggplot(standards_all, aes(x = level, y = RGopponency)) +
      geom_boxplot() +
      geom_point(size = 1, alpha = 0.3) +
      geom_hline(data = means, aes(yintercept = RGopponency_mean), colour = "grey50", linetype = "dashed") +
      facet_wrap(~standard_ID) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 8)) +
      labs(y = "human red-green opponency",
           x = "weather and elevation angle of the sun")
  
    g_all <- ggpubr::ggarrange(g_brightness, g_Rchroma, g_RGopponency, ncol = 1, nrow = 3)
    ggsave("colour_squares_comparison.png", plot = g_all, width = 3500, height = 4500, units = "px", dpi = 600)
    
    
    
# GREY X-RITE PATCHES ------------------------------------------------------------
  
  ## prepare grey squares database ----------------------------------------------
  
  setwd("D:/Science/Eggs/PhD/variable_light/photos/measurements/Xrite_basing") #adjust if other
  
  greyStandards <- read.xlsx("eggs_xrite_with_granularity.xlsx", sheetIndex = 1) %>%
    filter(str_detect(Label, "_g")) %>%
    transmute(Label = as.character(Label),
              R = as.numeric(`visible.R.NormalisedMean`),
              G = as.numeric(`visible.G.NormalisedMean`),
              B = as.numeric(`visible.B.NormalisedMean`)) %>%
    mutate(meanRGB = (R+G+B)/3)
  
  
  greyStandards$weather <- as.factor(c(rep("cloud", times = 180), rep("sun", times = 180)))
  greyStandards$degrees <- as.factor(rep(rep(c(10,20,30,40,55), each = 36), times = 2))
  greyStandards$level <- as.factor(paste(greyStandards$weather, greyStandards$degrees, sep = ""))
  greyStandards$standard_ID <- as.factor(rep(c("g1", "g2", "g3", "g4", "g5", "g6"), times = 60))
  
  greyStandards <- greyStandards[,c(9,6:8,1,5)]
  str(greyStandards)
  
  greyStandards <- greyStandards %>%
    filter(standard_ID %in% c("g2", "g3", "g4", "g5"))
  
  
  ## calculate mean values of grey standards -----------------------------------------------------
  
  grey_means_levels <- greyStandards %>%
    group_by(level, standard_ID) %>%
    summarise(mean = round(mean(meanRGB), digits = 2),
              sd = round(sd(meanRGB), digits = 2)) %>%
    arrange(standard_ID) %>%
    ungroup()
  
  write.xlsx(x = grey_means_levels, file = "grey_squares_comparison.xlsx", sheetName = "Sheet1")
  
  means <- greyStandards %>%
    group_by(standard_ID) %>%
    summarise(mean = mean(meanRGB),
              sd = sd(meanRGB)) %>%
    arrange(standard_ID)
  
  means <- means[,c(1,2,5,3,6,4,7)]
  
  ## visualise mean values of grey standards ----------------------------------------
  
  my_xrite <- c(1.99, 8.52, 19.03, 37.35, 64.51, 92.96) #reflectance measured with spectrometry
  XRite_colour <- c("#000000", "#333333", "#555555", "#888888", "#aaaaaa", "#cccccc")
  producer_names <- c("black", "neutral 3.5", "neutral 5", "neutral 6.5", "neutral 8", "white")
  
  g_xrite <- ggplot(greyStandards, aes(x = level, y = meanRGB)) +
    geom_hline(yintercept = my_xrite, colour = XRite_colour) +
    geom_point(size = 2, alpha = 0.8) +
    scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
    theme_classic() +
    scale_color_manual(name = "XRite reflectance", values = XRite_colour) +
    annotate("text", x = 10.5, y = my_xrite, label = producer_names, hjust = 0.9, vjust = 1.2, size = 3) +
    labs(x = "Weather and elevation of sun",
         y = "% Reflectance")
  g_xrite
  ggsave("XRite_new.png", g_xrite, width = 3000, height = 2500, dpi = 300, units = "px")
  
  ## compare camera channels ---------------------------------------------
  
  greyStandards %>%
    group_by(standard_ID) %>%
    summarise_at(vars(c(R, G, B, meanRGB)), funs(mean, sd))
