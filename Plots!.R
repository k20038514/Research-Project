# install.packages('ggridges')
# install.packages('patchwork')

library(ggplot2)
library(ggridges)
library(patchwork)


# Import raw data 
MRS_fulldat = rio::import('GABAGlx_Conc.xlsx') # Import Excel file containing all data
MRS_low = rio::import('0-2conc.xlsx')
MRS_high = rio::import('0-5conc.xlsx')

# Clean dataset
MRS_dat = dplyr::select(MRS_fulldat, -c(GABA, Notes, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2)) # Removing quality metric data, GABA data, notes and AQ as they will not be used in the subsequent analyses
str(MRS_dat) # Check the data type of each object in the dataset
which(is.na(MRS_dat)==T) # Identifies NA values

MRS_dat$Dose = factor(MRS_dat$Dose, levels = c(0, 2, 5)) # Treating Dose as a factor so r treats it as a categorical variable
MRS_dat$Group = factor(MRS_dat$Group, levels = c(0, 1)) # Treating Group as a factor so r treats it as a categorical variable

MRS_low$Dose = factor(MRS_low$Dose, levels = c(0, 2)) # Treating Dose as a factor so r treats it as a categorical variable
MRS_high$Dose = factor(MRS_high$Dose, levels = c(0, 5)) # Treating Dose as a factor so r treats it as a categorical variable

# For the low condition with GABAplus:
raincloud_low_GABA = ggplot(MRS_low, aes(x = Dose, y = GABAplus, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 2)) + # Note: this needs to be scale_x_continous if using a continous variable!
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("GABA+") +
  ylim(0, 4.5) # Add this line to set the y-limit to 4.5

# Spaghetti plot for the low dose with GABAplus

# create the spaghetti plot
spaghetti_low_GABA = ggplot(MRS_low, aes(x = Dose, y = GABAplus, group = ID, color = factor(ID))) +
  geom_line() +
  geom_point() +
  labs(x = "Dose (mg)", y = "GABA+", color = "Participant ID") + 
  ylim(0, 4.5)

# Combine the two plots
combined_low_GABA = raincloud_low_GABA + spaghetti_low_GABA

# Display the combined plot
combined_low_GABA

#########

# For the high condition with GABAplus:
raincloud_high_GABA = ggplot(MRS_high, aes(x = Dose, y = GABAplus, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 5)) + # Note: this needs to be scale_x_continous if using a continous variable!
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("GABA+") +
  ylim(0, 4.5) # Add this line to set the y-limit to 4.5

# Spaghetti plot for the high dose with GABAplus:
# create the spaghetti plot
spaghetti_high_GABA = ggplot(MRS_high, aes(x = Dose, y = GABAplus, group = ID, color = factor(ID))) +
  geom_line() +
  geom_point() +
  labs(x = "Dose (mg)", y = "GABA+", color = "Participant ID") + 
  ylim(0, 4.5)

# Combine the two plots
combined_high_GABA = raincloud_high_GABA + spaghetti_high_GABA

# Display the combined plot
combined_high_GABA

#########

# For the low condition with Glx:
raincloud_low_Glx = ggplot(MRS_low, aes(x = Dose, y = Glx, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 2)) + # Note: this needs to be scale_x_continous if using a continous variable!
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("Glx") +
  ylim(6, 12) # Add this line to set the y-limit to 4.5

# Spaghetti plot for the low dose with GABAplus

# create the spaghetti plot
spaghetti_low_Glx = ggplot(MRS_low, aes(x = Dose, y = Glx, group = ID, color = factor(ID))) +
  geom_line() +
  geom_point() +
  labs(x = "Dose (mg)", y = "Glx", color = "Participant ID") + 
  ylim(6, 12)

# Combine the two plots
combined_low_Glx = raincloud_low_Glx + spaghetti_low_Glx

# Display the combined plot
combined_low_Glx

#########

# For the high condition with Glx:
raincloud_high_Glx = ggplot(MRS_high, aes(x = Dose, y = Glx, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 5)) + # Note: this needs to be scale_x_continous if using a continous variable!
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("Glx") +
  ylim(6, 12) # Add this line to set the y-limit to 4.5

# Spaghetti plot for the low dose with GABAplus

# create the spaghetti plot
spaghetti_high_Glx = ggplot(MRS_high, aes(x = Dose, y = Glx, group = ID, color = factor(ID))) +
  geom_line() +
  geom_point() +
  labs(x = "Dose (mg)", y = "Glx", color = "Participant ID") + 
  ylim(6, 12)

# Combine the two plots
combined_high_Glx = raincloud_high_Glx + spaghetti_high_Glx

# Display the combined plot
combined_high_Glx
######
# Get rid of NA column
# Send methods and results to Grainne


