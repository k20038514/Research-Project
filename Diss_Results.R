# STEP 1: Installing and loading required packages
# install.packages('tidyverse')
# install.packages('ggpubr') # Pearson's correlation
# install.packages('lme4') # Mixed model equivalent to the linear model package (lm)
# install.packages('ggdist') # For raincloud plots... and below:
# install.packages('lavaan')
# install.packages('plyr')
# install.packages('cowplot')
# install.packages('caTools') 
# install.packages('bitops')
# install.packages('readr')
# install.packages('gghalves')
# install.packages('emmeans') # For post-hoc
# install.packages('multcompView') # For post-hoc

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(lme4)
library(ggdist)
library(gghalves)
library(emmeans)
library(multcompView)

# Import raw data 
MRS_fulldat = rio::import('GABAGlx_Conc.xlsx') # Import Excel file containing all data

# Create separate dataset for values at 0, 2 and 5mg doses (for QM averaging below)
pla_dat = dplyr::select(MRS_fulldat, c(Dose, GABAplus, Glx, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2))
pla_dat = subset(pla_dat, Dose == "0")

low_dat = dplyr::select(MRS_fulldat, c(Dose, GABAplus, Glx, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2))
low_dat = subset(low_dat, Dose == "2")

high_dat = dplyr::select(MRS_fulldat, c(Dose, GABAplus, Glx, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2))
high_dat = subset(high_dat, Dose == "5")

# Work out the median GABAplus and Glx levels for each dose 
median_pla_GABA = median(pla_dat$GABAplus, na.rm = TRUE)
median_low_GABA = median(low_dat$GABAplus, na.rm = TRUE)
median_high_GABA = median(high_dat$GABAplus, na.rm = TRUE)

median_pla_Glx = median(pla_dat$Glx, na.rm = TRUE)
median_low_Glx = median(low_dat$Glx, na.rm = TRUE)
median_high_Glx = median(high_dat$Glx, na.rm = TRUE)

# Work out mean and SD for each of the quality metrics
# Signal-to-noise ratio mean
SNR_pla_mn = mean(pla_dat$SNR, na.rm = TRUE)
SNR_low_mn = mean(low_dat$SNR, na.rm = TRUE)
SNR_high_mn = mean(high_dat$SNR, na.rm = TRUE) 

# Signal-to-noise ratio SD
SNR_pla_sd = sd(pla_dat$SNR, na.rm = TRUE)
SNR_low_sd = sd(low_dat$SNR, na.rm = TRUE)
SNR_high_sd = sd(high_dat$SNR, na.rm = TRUE) 

# Full-width half maximum mean
FWHM_pla_mn = mean(pla_dat$Cr_FWHM, na.rm = TRUE) 
FWHM_low_mn = mean(low_dat$Cr_FWHM, na.rm = TRUE) 
FWHM_high_mn = mean(high_dat$Cr_FWHM, na.rm = TRUE) 

# Full-width half maximum SD
FWHM_pla_sd = sd(pla_dat$Cr_FWHM, na.rm = TRUE) 
FWHM_low_sd = sd(low_dat$Cr_FWHM, na.rm = TRUE) 
FWHM_high_sd = sd(high_dat$Cr_FWHM, na.rm = TRUE) 

# Residual water amplitude mean
RWA_pla_mn = mean(pla_dat$residual_water_ampl, na.rm = TRUE) 
RWA_low_mn = mean(low_dat$residual_water_ampl, na.rm = TRUE) 
RWA_high_mn = mean(high_dat$residual_water_ampl, na.rm = TRUE) 

# Residual water amplitude SD
RWA_pla_sd = sd(pla_dat$residual_water_ampl, na.rm = TRUE) 
RWA_low_sd = sd(low_dat$residual_water_ampl, na.rm = TRUE) 
RWA_high_sd = sd(high_dat$residual_water_ampl, na.rm = TRUE) 

# Frequency shift mean
FS_pla_mn = mean(pla_dat$freqShift, na.rm = TRUE)
FS_low_mn = mean(low_dat$freqShift, na.rm = TRUE)
FS_high_mn = mean(high_dat$freqShift, na.rm = TRUE)

# Frequency shift SD
FS_pla_sd = sd(pla_dat$freqShift, na.rm = TRUE)
FS_low_sd = sd(low_dat$freqShift, na.rm = TRUE)
FS_high_sd = sd(high_dat$freqShift, na.rm = TRUE)

# Fit residuals mean for GABA and Glx (since I used diff1 spectra to obtain these values)
FR_pla_mn = mean(pla_dat$relResdiff1, na.rm = TRUE) 
FR_low_mn = mean(low_dat$relResdiff1, na.rm = TRUE)
FR_high_mn = mean(high_dat$relResdiff1, na.rm = TRUE) 

# Fit residuals SD
FR_pla_sd = sd(pla_dat$relResdiff1, na.rm = TRUE) 
FR_low_sd = sd(low_dat$relResdiff1, na.rm = TRUE)
FR_high_sd = sd(high_dat$relResdiff1, na.rm = TRUE) 

# Clean dataset
MRS_dat = dplyr::select(MRS_fulldat, -c(GABA, Notes, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2)) # Removing quality metric data, GABA data, notes and AQ as they will not be used in the subsequent analyses
str(MRS_dat) # Check the data type of each object in the dataset
which(is.na(MRS_dat)==T) # Identifies NA values, which is important to know, but these will not be a problem in mixed model analysis

MRS_dat$Dose = factor(MRS_dat$Dose, levels = c(0, 2, 5)) # Treating Dose as a factor so r treats it as a categorical variable
MRS_dat$ID = as.numeric(MRS_dat$ID) # Change the data in the ID column to numeric as they were stored as string

# Looking at the relationship between GABA levels and psilocybin dose through a box plot
GABAbox = boxplot(GABAplus ~ Dose, col=c("white", "lightgray"), MRS_dat)
# Look at medians to see how levels change across conditions:
# You can do this by puttting 'GABAbox' into the command window, and looking at the [3,] row which shows the median values of each condition

# ...and the same for Glx
Glxbox = boxplot(Glx ~ Dose, col=c("white", "lightgray"), MRS_dat)

# Running the linear mixed model for GABA
GABA.model = lmer(GABAplus ~ Dose + Group + (1|ID), data = MRS_dat)
# Significant: means that adding dose to the model improves the fit of the data
# -> Dose or group DOES impact GABA+ levels

# Null model for GABA:
GABA.null = lmer(GABAplus ~ Group + (1|ID), data = MRS_dat)

# Compare models using Chi square
GABA.anova = anova(GABA.model, GABA.null, test = "Chisq")
# Significant -> the inclusion of dose as a predictor in the GABA model significantly improves the fit compared to the null
# This means that dose is an important predictor of GABAplus levels

# Post-hoc to assess directionality of variables
GABA_Ph = emmeans(GABA.model, list(pairwise ~ Dose), adjust = "tukey")
# emmeans calculates the predicted mean values of GABAplus for each level of dose, after accounting for random effect of ID

# Results suggest that there is a trend towards lower GABAplus levels at 2 mg compared to 0 mg, and a trend towards higher GABAplus levels at 5 mg compared to 0 mg, but neither of these are statistically significant
# However, there is a significant difference in mean GABAplus levels between 2 mg and 5 mg, with higher levels at 5 mg compared to 2 mg

# Running the linear mixed model for Glx
Glx.model = lmer(Glx ~ Dose + Group + (1|ID), data = MRS_dat)

# Null model for Glx:
Glx.null = lmer(Glx ~ Group + (1|ID), data = MRS_dat)

# Compare models using Chi square 
Glx.anova = anova(Glx.model, Glx.null, test = "Chisq")

# Post-hoc to assess directionality of variables
Glx_Ph = emmeans(Glx.model, list(pairwise ~ Dose), adjust = "tukey")
print(Glx_Ph)

# Results suggest that mean Glx levels for 5 mg is lower than 0 mg, and the mean Glx level for 5 mg is lower than 2 mg

# Assumption testing for GABA
# 1) Normality of residuals: the differences between predicted and real values should be normally distributed 
hist(residuals(GABA.model))
qqnorm(residuals(GABA.model))
qqline(residuals(GABA.model))
plot(GABA.model)
shapiro.test(residuals(GABA.model))

# Assumption testing for Glx
# 1) Normality of residuals: the differences between predicted and real values should be norally distributed 
hist(residuals(Glx.model))
qqnorm(residuals(Glx.model))
qqline(residuals(Glx.model))
plot(Glx.model)
shapiro.test(residuals(Glx.model))

# Pearson correlation for GABA at 2mg
# Import 2mg dataset
two_mg_dat = rio::import('2mg_dat.xlsx')
GABA_cor_2mg = cor.test(two_mg_dat$GABAplus, two_mg_dat$AQ,  method = "pearson", use = "complete.obs") # Computes pearson correlation for GABA+ and AQ, 'complete.obs' removes missing values so you don't have to manually
Glx_cor_2mg = cor.test(two_mg_dat$Glx, two_mg_dat$AQ,  method = "pearson", use = "complete.obs") 

# Import 5mg dataset
five_mg_dat = rio::import('5mg_dat.xlsx')
GABA_cor_5mg = cor.test(five_mg_dat$GABAplus, five_mg_dat$AQ, method = "pearson", use = "complete.obs")
Glx_cor_5mg = cor.test(five_mg_dat$Glx, five_mg_dat$AQ, method = "pearson", use = "complete.obs")


