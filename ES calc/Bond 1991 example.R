

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
    
    cor = 0.5,
    
    Nt = c(28, 23),
    Nc = 40,
    
    d_pp_trt = c(d_pair[1], d_pair[2]),
    d_pp_ctr = d_pair[3]
    
    
  ) |> 
  mutate(
    
    d = d_pp_trt - d_pp_ctr,
    vd = (1/Nt + 1/Nc) * 2*(1-cor) + d^2/(2*(Nt+Nc-2)),
    
  
    
  ); Bond_1991_es_T
