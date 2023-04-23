# install.packages('ggridges')
# install.packages('patchwork')

# NOTE: The code for raincloud plots has been adapted from Cedric Scherer (https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/)
# ... Also big thanks to Alice Thomson (InPuts lab, KCL) who shared her raincloud code with me too!

# Load relevant libraries 
library(ggplot2)
library(ggridges)
library(patchwork)

# Import raw data 
MRS_fulldat = rio::import('GABAGlx_Conc.xlsx') # Import Excel file containing all data
MRS_low = rio::import('0-2conc.xlsx') # Import the dataset which contains GABA/Glx data from 0-2mg of psilocybin
MRS_high = rio::import('0-5conc.xlsx') # Import the dataset which contains GABA/Glx data from 0-5mg of psilocybin

# Clean dataset
MRS_dat = dplyr::select(MRS_fulldat, -c(GABA, Notes, SNR,	Cr_FWHM,	water_FWHM,	residual_water_ampl,	freqShift,	relRessum,	relResdiff1,	relResdiff2)) # Removing quality metric data, notes and GABA data as they will not be used here
# ...note, I am removing GABA not GABAplus
str(MRS_dat) # Check the data type of each object in the dataset, so can change if needed (see lines 20-24)
which(is.na(MRS_dat)==T) # Identifies NA values

MRS_dat$Dose = factor(MRS_dat$Dose, levels = c(0, 2, 5)) # Treating Dose as a factor so r treats it as a categorical variable
MRS_dat$Group = factor(MRS_dat$Group, levels = c(0, 1)) # ...again

MRS_low$Dose = factor(MRS_low$Dose, levels = c(0, 2)) # Treating Dose as a factor so r treats it as a categorical variable
MRS_high$Dose = factor(MRS_high$Dose, levels = c(0, 5)) # ...again

# Plotting a raincloud plot to display data from the low condition with GABAplus
raincloud_low_GABA = ggplot(MRS_low, aes(x = Dose, y = GABAplus, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 2)) + # Note: this needs to be scale_x_continous if using a continous variable!
  ## Add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## Custom bandwidth
    adjust = .5, 
    ## Adjust height
    width = .6, 
    ## Move geom to the right
    justification = -.2, 
    ## Remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    ## Remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("GABA+") +
  ylim(0, 4.5) # Add this line to set the y-limit to 4.5

# Plotting a spaghetti plot to display the participant trajectories of GABAplus between placebo and low dose 
spaghetti_low_GABA = ggplot(MRS_low, aes(x = Dose, y = GABAplus, group = ID, color = factor(ID))) +  # Grouping participants by their ID number
  geom_line() + # We want to see lines and points 
  geom_point() +
  labs(x = "Dose (mg)", y = "GABA+", color = "Participant ID") + 
  ylim(0, 4.5) # Setting y limit between 0 and 4.5 as no GABA+ values exceed these limits (it just looks neater)

# Combine the two plots
combined_low_GABA = raincloud_low_GABA + spaghetti_low_GABA

# Display the combined plot
combined_low_GABA

#########

# Plotting a second raincloud plot to display data from the high condition with GABAplus
raincloud_high_GABA = ggplot(MRS_high, aes(x = Dose, y = GABAplus, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 5)) + # Note: this needs to be scale_x_continous if using a continous variable!
  # Add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # Custom bandwidth
    adjust = .5, 
    # Adjust height
    width = .6, 
    # Move geom to the right
    justification = -.2, 
    # Remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    # Remove outliers
    outlier.color = NA # `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("GABA+") +
  ylim(0, 4.5) # Add this line to set the y-limit to 4.5

# Plotting a second spaghetti plot to display the participant trajectories of GABAplus between placebo and high dose 
spaghetti_high_GABA = ggplot(MRS_high, aes(x = Dose, y = GABAplus, group = ID, color = factor(ID))) +  # Grouping participants by their ID number
  geom_line() + # We want to add lines and points 
  geom_point() +
  labs(x = "Dose (mg)", y = "GABA+", color = "Participant ID") + 
  ylim(0, 4.5) # Setting y limit between 0 and 4.5 as no GABA+ values exceed these limits 

# Combine the two plots
combined_high_GABA = raincloud_high_GABA + spaghetti_high_GABA

# Display the combined plot
combined_high_GABA

#########

# Plotting a third raincloud plot to display data from the low condition with Glx
raincloud_low_Glx = ggplot(MRS_low, aes(x = Dose, y = Glx, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 2)) + # Note: this needs to be scale_x_continous if using a continous variable!
  # Add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # Custom bandwidth
    adjust = .5, 
    # Adjust height
    width = .6, 
    # Move geom to the right
    justification = -.2, 
    # Remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    # Remove outliers
    outlier.color = NA # `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("Glx") +
  ylim(6, 12) # Add this line to set the y-limit to 4.5

# Plotting a third spaghetti plot to display the participant trajectories of Glx between placebo and low dose
spaghetti_low_Glx = ggplot(MRS_low, aes(x = Dose, y = Glx, group = ID, color = factor(ID))) +  # Grouping participants by their ID number
  geom_line() + # We want to add lines and points 
  geom_point() + 
  labs(x = "Dose (mg)", y = "Glx", color = "Participant ID") + 
  ylim(6, 12) # Setting y limit between 6 and 12 as no Glx values exceed these limits

# Combine the two plots
combined_low_Glx = raincloud_low_Glx + spaghetti_low_Glx

# Display the combined plot
combined_low_Glx

#########

# Plotting the final raincloud plot to display data from the high condition with Glx
raincloud_high_Glx = ggplot(MRS_high, aes(x = Dose, y = Glx, color = factor(Dose))) +
  scale_x_discrete(breaks = c(0, 5)) + # Note: this needs to be scale_x_continous if using a continous variable!
  # Add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    # Custom bandwidth
    adjust = .5, 
    # Adjust height
    width = .6, 
    # Move geom to the right
    justification = -.2, 
    # Remove slab interval
    .width = 0, 
    point_colour = NA,
    aes(fill = factor(Dose))
  ) + 
  geom_boxplot(
    width = .12, 
    # Remove outliers
    outlier.color = NA # `outlier.shape = NA` works as well
  ) +
  xlab("Dose (mg)") +
  ylab("Glx") +
  ylim(6, 12) # Add this line to set the y-limit to 4.5

# Plotting a final spaghetti plot to display the participant trajectories of Glx between placebo and high dose
spaghetti_high_Glx = ggplot(MRS_high, aes(x = Dose, y = Glx, group = ID, color = factor(ID))) +  # Grouping participants by their ID number
  geom_line() + # We want to add lines and points 
  geom_point() +
  labs(x = "Dose (mg)", y = "Glx", color = "Participant ID") + 
  ylim(6, 12) # Setting y limit between 6 and 12 as no Glx values exceed these limits

# Combine the two plots
combined_high_Glx = raincloud_high_Glx + spaghetti_high_Glx

# Display the combined plot
combined_high_Glx

