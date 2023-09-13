library(tibble)
library(VIVECampbell)
library(purrr)
library(dplyr)
library(tidyr)
library(gt)
library(ggplot2)
library(tidyverse)


#Data extraction from Gonzalez and Prihoda (2007)
#Draft by Jasmin


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

  
Gonzalez2007 <- tibble(
    measure = c("Clinician status", "Depression symptoms", "Manic symptoms", "GAF past month", "CGI score"),
    Nt = rep(8, 5), # Gentag 8 fem gange
    Nc = rep(9, 5), # Gentag 9 fem gange
    F_Val = c(6.13, 5.03, 0.02, 0.22, 2.34),
    P_val = c(.03, .05, .88, .65, .15)
  )
  
Gonzalez2007
view(Gonzalez2007)


Gonzalez2007_est <- 
  Gonzalez2007 |> 
  mutate(
    N_tot = Nt + Nc,
    df_ind = N_tot,
    vd = (d^2/F_val + d^2/(2*N_tot) ),
    
    J = 1 - 3/(4*df_ind-1),
    g = J * d,
    vg = vd
    
  ) |> 
  relocate(vd, .after = d); Gonzalez2007_est
