# Loading the relevant package herein Tidyverse 
library(tidyverse)

# Setting the relevant options -------------------
# The code benith sets global options in R for various packages and functions
options(pillar.sigfig = 4) # ensure tibble include 4 digits

options(tibble.width = Inf) # This sets the width of output from the tibble 
                            # package to infinity, meaning that the output will 
                            # not be truncated or wrapped.

options(dplyr.print_min = 310) # This sets the minimum number of rows that will 
                               # be printed when using the dplyr package to 310

options(scipen = 10) # This sets the scientific notation precision in R to 10, 
                     # meaning that numbers will be displayed with 10 digits of precision

# Making a data frame with the information from Fisher (1996) Table 4 p. 1248. ------
fisher_1996 <- tibble(
# First i am making the relevant variables indicating the status of each group and subgroup
# and the relevant outcomes. 
  setting = rep(c("inpatient", "outpatient"), each = 3,4),
  subscales = rep(c("alcohol_use", "drug_use", "social/family_realtions", "psychological_functioning"), each = 6),
  treatment = rep(c("disease_recovery", "cognitive_behaviorial", "TAU"), 8),

# Now I will put in the different values of the mean and standard deviation
# for each treatment-group before(pre) and after treatment (post) and the difference

 N = 6,
 m_pre = c(
  0.469, 0.441, 0.349, # alcohol_use - inpatient
  0.725, 0.598, 0.682, # alcohol_use - outpatient
  
  0.107, 0.116, 0.117, # drug_use - inpatient
  0.219, 0.200, 0.322, # drug_use - outpatient

  0.342, 0.419, 0.450, # social relations - inpatient 
  0.683, 0.584, 0.571, # social relations - outpatient
  
  0.393, 0.498, 0.605, # psychological functioning - inpatient
  0.464, 0.476, 0.487  # psychological functioning - outpatient
  ),

sd_pre = c(
  0.12, 0.13, 0.22, # alcohol_use - inpatient
  0.11, 0.16, 0.17, # alcohol_use - outpatient
  
  0.09, 0.10, 0.12, # drug_use - inpatient
  0.11, 0.12, 0.05, # drug_use - outpatient
  
  0.21, 0.12, 0.24, # social relations - inpatient
  0.04, 0.05, 0.09, # social relations - outpatient
  
  0.17, 0.13, 0.15, # psychological functioning - inpatient
  0.14, 0.12, 0.16  # psychological functioning - outpatient
),

m_post = c(
 0.070, 0.018, 0.141, # alcohol use - inpatient 
 0.521, 0.152, 0.492, # alcohol use - outpatient

 0.001, 0.008, 0.087, # drug use - inpatient
 0.167, 0.044, 0.216, # drug use - outpatient 

 0.083, 0.103, 0.498, # social relations - inpatient
 0.641, 0.233, 0.514, # social relations - outpatient

 0.319, 0.442, 0.601, # psychological functioning - inpatient
 0.432, 0.232, 0.472  # psychological functioning - outpatient 
),

sd_post = c(
  0.11, 0.05, 0.21, # alcohol use - inpatient 
  0.11, 0.15, 0.27, # alcohol use - outpatient 
  
  0.01, 0.02, 0.12, # drug use - inpatient
  0.10, 0.05, 0.15, # drug use - outpatient 
  
  0.10, 0.10, 0.18, # social relations - inpatient 
  0.06, 0.16, 0.12, # social relations - outpatient 
  
  0.14, 0.16, 0.18, # psychological functioning - inpatient
  0.18, 0.07, 0.18  # psychological functioning - outpatient 
  ),

m_diff = c(
  -0.399, -0.423, -0.208, # alcohol use - inpatient
  -0.204, -0.446, -0.190, # alcohol use - outpatient 
  
  -0.106, -0.108, -0.030, # drug use - inpatient 
  -0.052, -0.196, -0.106, # drug use - outpatient 
  
  -0.259, -0.316, 0.039, # social relations - inpatient 
  -0.042, -0.351, -0.057, # social relations - outpatient
  
  -0.074, -0.056, -0.004, # psychological functioning - inpatient
  -0.032, -0.244, -0.015  # psychological functioning - outpatient
),

sd_diff = c(
  0.02, 0.09, 0.04, # alcohol use - inpatient
  0.01, 0.02, 0.11, # alcohol use - outpatient 
  
  0.09, 0.09, 0.02, # drug use - inpatient
  0.02, 0.08, 0.11, # drug use - outpatient
  
  0.13, 0.03, 0.07, # social relations - inpatient
  0.03, 0.12, 0.04, # social relations - outpatient
  
  0.04, 0.04, 0.04, # psychological functioning - inpatient
  0.05, 0.06, 0.03  # psychological functioning - outpatient
  
),

#Taking from the Cochrane handbook (Higgins & Thomas, 2019, p. 166)
r = (sd_pre^2 + sd_post^2-sd_diff^2)/(2 * sd_pre * sd_post ),

# Obtained from Wilson (2016). Converting sd_diff to raw sd
sdg = sd_diff/sqrt(2*(1-r)),

z = 0.5 * log( (1+r)/(1-r) ),
v = 1/(N-3)

)


# To see the data frame in console
fisher_1996