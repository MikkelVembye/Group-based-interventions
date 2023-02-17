

install.packages("devtools")
devtools::install_github("MikkelVembye/VIVECampbell")
library(VIVECampbell)
library(dplyr)
?df_h_1armcluster

install.packages("tidyverse")
install.packages("VIVECampbell")
library(devtools)
library(tidyverse)

Fischer1996 <- tibble::tibble(
  Outcome = rep(c("Alcohol use", "Drug use", "Social"), each = 6),
                
  Setting = rep(c("Inpatient", " Outpatient"), each = 3, 3),
  
  Treatment = rep(rep(c("Disease", "Cognitive", "TAU", 2), 3),
                
                  
                  
N = 9, 
## All values are from pretest
m_pre = c(
  .469, .441, .349, # Alcohol use - inpatient setting
  .725, .598, .682, # Alcohol use - outpatient setting

  .107, .116, .117, # Drug use - Inpatient setting
  .219, .200, .322, # Drug use - Outpatient setting
  
  .342, .419, .450, # Social and family relations - inpatient setting
  .683, .584, .571 # Social and family relations - Outpatient setting
  
  ),

sd_pre = c(
  .12, .13, .22, #Alcohol use - in
  .11, .16, .17, #Alcohol use - out
  
  .09, .10, .12, # Drug use - in
  .11, .12, .05, #Drug use - out 
  
  .21, .12, .24, #Social and family - in
  .04, .05, .09 # Social and family -out 
),
  
  m_post = c(
    .070, .018, .141, #Alcohol use - in
    .7521, .152, .492, #Alcohol use - out
    
    .001, .008, .087, #Drug use - in
    .167, .044, .216, #Drug use - out
    
    .083, .103, .489, #social and family - in
    .641, .233, .514 # Social and family - out
    
  ),
  
  sd_post = c(
    .11, .05, .21, # Alcohol use - in
    .11, .15, .27, # Alcohol use - out
    
    .01, .02, .12, # Drug use - in
    .10, .05, .15, # Drug use - out
    
    .10, .10, .18, #social and family
    .06, .16, .12 #S&F - out
  ),

#Difference - both inpatient and outpatient setting

m_diff = c(
  .399, .423, .208,# Alcohol use - inp
  .204, .446, .190,# Alcohol use - out
  
  .106, .108, .030, # Drug use - inp
  .052, .196, .106, # Drug use - outp
  
  .259, .316, -.039, # S&F - inp
  .042, .351, .057 # S&F - outp
),


sd_diff = c(
  .02, .09, .04, #Alcohol use - inp
  .01, .02, .11, #Alcohol use - out
  
  .09, .09, .02, #Drug use - inp
  .02, .08, .11, #Drug use - outp
  
  .13, .03, .07, #S&F - inp
  .03, .12, .04 # S&F - outp
  
),


# We are testing weather the data on difference in meanvalues are reported right
# install.packages("mean_diff_test"),

# mean_diff_test = m_pre - m_post,### HVORFOR VIRKER DET IKKE!

#From the Cochrane handbook (Higgins & Thomas, 2019, p. 166)
r = (sd_pre^2 + sd_post^2-sd_diff^2)/(2 * sd_pre * sd_post ),

# (sd_pre^2 + sd_post^2-sd_diff^2)/(2 * sd_pre * sd_post)

# r = (sd_post^2)

# Obtained from Wilson (2016). Converting sd_diff to raw sd
sdg = sd_diff/sqrt(2*(1-r)),

z = 0.5 * log( (1+r)/(1-r) ),
v = 1/(N-3)
)

