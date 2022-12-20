library(tidyverse)

Bond_1991 <- tibble(
  trt = c("ACT", "RG", "Control"),
  n = c(28, 23, 40), 
  tval_paried = c(3, 2, 2.6)
)

Bond_1991_es_T <- 
  Bond_1991 |> 
  mutate(
    r = 0.5,
    d_pair = tval_paried * sqrt( (2*(1-r))/n  )
  ) |> 
  summarise(
    
    trt = c("ACT", "RG"),
    
    outcome = "Hospital days",
    timing = "6 months",
    
    cor = 0.5,
    
    Nt = c(28, 23),
    Nc = 40,
    N_tot = Nt + Nc,
    
    d_pp_trt = c(d_pair[1], d_pair[2]),
    d_pp_ctr = d_pair[3]
    
    
  ) |> 
  mutate(
    
    d = d_pp_trt - d_pp_ctr,
    vd = (1/Nt + 1/Nc) * 2*(1-cor) + d^2/(2*(Nt+Nc-2)),
    
    # Gleser & Olkin 2009 eq 19.19
    cov_ds = 1/Nc + (prod(d)/ 2*(N_tot - 2 - 1)),
    
    J = 1 - 3/(4*N_tot-1),
    
    g = J * d,
    vg = J^2 * vd
   
    #Make cluster bias correction below
     
  ); Bond_1991_es_T
