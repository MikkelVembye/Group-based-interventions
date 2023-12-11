library(tibble)

cano_vindel2021_new <- tibble(
  sample = rep(c("ITT", "Per-protocol"), each = 80),
  group = rep(c("TAU", "TAU-GCBT"), each = 1,80),
  
  outcome = rep(c("GAD_7", "PHQ_9", "PHQ15", "working_life_Function",
                  "Socal_life_Function", "Family_life_Function", "Physical_QoL",
                  "Psychological_QoL", "Social_QoL", "Environment_QoL"), 
                each = 8,2),
  
  timing = rep(c("Post", "3m", "6m", "12m"), each = 2,20),
  
  N = c(rep(c(534, 527), 40), # ITT
        rep(c(316, 315, # per-protocol post-treatment
              238, 273,# per-protocol 3 months
              204, 229,# per-protocol 6 months
              180, 208), 1,80) # per-protocol 12 months
  ),
  
  m_pre = c(
    rep(c(12.1, 12.5), each = 1,4),  # ITT GAD-7 Anxiety Baseline)
    rep(c(13.5, 13.7), each = 1,4),  # ITT PHQ9
    rep(c(14, 14.3), each = 1,4), # ITT PHQ15
    rep(c(3.5, 3.6), each = 1,4), # ITT working life
    rep(c(4.6, 4.7), each = 1,4), # ITT social life
    rep(c(4.6, 4.8), each = 1,4), # ITT family life
    rep(c(22.4, 22.1), each = 1,4), # ITT Physical
    rep(c(16.9, 16.9), each = 1,4), # ITT Psychological
    rep(c(9.1, 9.1), each = 1,4), # ITT Social
    rep(c(25.3, 25.7), each = 1,4),  # ITT Environment 
      
    rep(c(12.1, 12.5), each = 1,4), # per-protocol GAD-7 Anxiety Baseline)
    rep(c(13.5, 13.7), each = 1,4), # per-protocol GAD-7 PHQ9
    rep(c(14, 14.3), each = 1,4),   # per-protocol GAD-7 PHQ15
    rep(c(3.5, 3.6), each = 1,4),   # per-protocol GAD-7 working life
    rep(c(4.6, 4.7), each = 1,4),   # per-protocol GAD-7 social life
    rep(c(4.6, 4.8), each = 1,4),   # per-protocol GAD-7 family life
    rep(c(22.4, 22.1), each = 1,4), # per-protocol GAD-7 Physical
    rep(c(16.9, 16.9), each = 1,4), # per-protocol GAD-7 Psychological
    rep(c(9.1, 9.1), each = 1,4),   # per-protocol GAD-7 Social
    rep(c(25.3, 25.7), each = 1,4)  # per-protocol GAD-7 Environment 
  ), 
  
  sd_pre = c(
    rep(c(4.7, 4.6), each = 1,4),  # ITT GAD-7 Standard deviation
    rep(c(5.4, 5.3), each = 1,4), # ITT PHQ9
    rep(c(4.8, 4.9),each = 1,4), # ITT PHQ15
    rep(c(3.1, 3.2), each = 1,4), # ITT working life
    rep(c(3, 3), each = 1,4), # ITT social life
    rep(c(3.1, 3), each = 1,4), # ITT family life
    rep(c(4.3, 4.3), each = 1,4), # ITT Physical
    rep(c(3.8, 3.8), each = 1,4), # ITT Psychological
    rep(c(2.4, 2.4), each = 1,4), # ITT Social
    rep(c(4.5, 4.6), each = 1,4),  # ITT Environment 
    
    rep(c(4.7, 4.6), each = 1,4),  # per-protocol GAD-7 Standard deviation
    rep(c(5.4, 5.3), each = 1,4), # per-protocol PHQ9
    rep(c(4.8, 4.9),each = 1,4), # per-protocol PHQ15
    rep(c(3.1, 3.2), each = 1,4), # per-protocol working life
    rep(c(3, 3), each = 1,4), # per-protocol social life
    rep(c(3.1, 3), each = 1,4), # per-protocol family life
    rep(c(4.3, 4.3), each = 1,4), # per-protocol Physical
    rep(c(3.8, 3.8), each = 1,4), # per-protocol Psychological
    rep(c(2.4, 2.4), each = 1,4), # per-protocol Social
    rep(c(4.5, 4.6), each = 1,4)  # per-protocol Environment 
      
    
  ), 
  m_post = c(
    9.5, 6.8, # ITT GAD-7 Anxiety post-treatment
    8.7, 7.3, # ITT GAD-7 Anxiety 3 months
    8.6, 6.9, # ITT GAD-7 Anxiety 6 months
    8.3, 6.6,  # ITT GAD-7 Anxiety 12 months
    
    10.8, 8.0, # ITT PHQ9 Depression post treatment
    10.2, 8.4, # ITT PHQ9 Depression 3 months
    9.8, 7.9, # ITT PHQ9 Depression 6 months
    9.4, 7.8, # ITT PHQ9 Depression 12 months
    
    11.7, 9.9, # ITT PHQ-15 post-treatment
    11.4, 10.1,# ITT PHQ-15 3 months
    11.1, 9.8, # ITT PHQ-15 6 months
    10.7, 9.4, # ITT PHQ-15 12 months
    
    3.0, 2.6, # ITT working life post-treatment
    2.7, 2.4, # ITT Working life 3 months 
    2.7, 2.1, # ITT working life 6 months
    3.1, 2.4, # ITT working life 12 months
    
    
    4.1, 3.2, # ITT social life post-treatment
    3.5, 3.2, # ITT social life 3 months 
    3.4, 2.7, # ITT social life 6 months
    3.8, 2.9, # ITT social life 12 months
    
    3.9, 3.1, # ITT Family life post-treatment
    3.5, 3.1, # ITT Family life 3 months 
    3.6, 2.7, # ITT Family life 6 months
    3.8, 2.8, # ITT Family life  12 months
    
    23.2, 24.7, # ITT Physical post-treatment
    23.5, 24.2, # ITT Physical 3 months
    23.6, 24.3, # ITT Physical 6 months
    24.2, 26.4, # ITT Physical12 months
    
    17.7, 19.2, # ITT Psychological post-treatment
    18.1, 18.9, # ITT Psychological 3 months
    18.5, 18.9, # ITT Psychological 6 months
    18.7, 19.9, # ITT Psychological 12 months
    
    9.4, 9.8, # ITT Social post-treatment
    9.5, 9.7, # ITT Social 3 months
    9.5, 9.7, # ITT Social 6 months
    9.7, 11.1, # ITT Social 12 months
    
    25.7, 27.2, # ITT Environment post-treatment 
    26.1, 26.9, # ITT Environment 3 months
    26.5, 27.1, # ITT Environment 6 months
    27.5, 31.2,  # ITT Environment 12 months
    
    10.2, 6.0, # Per-protocol GAD-7 Anxiety post treatment
    8.9, 6.7, # Per-protocol GAD-7 Anxiety 3 months
    8.8, 6.2, # Per-protocol GAD-7 Anxiety  6 months
    8.7, 5.8, # Per-protocol GAD-7 Anxiety 12 months 
    
    11.7, 9.9,  # Per-protocol PHQ9 Depression post-treatment
    10.3, 7.8, # Per protocol PHQ9 Depression 3 months
    10.0, 7.3,  # Per-protocol PHQ9 Depression 6 months
    10.7, 9.4, # Per-protocol PHQ9 Depression 12 months
    
    12.1, 9.1, # Per-protocol PHQ-15 post treatment
    11.7, 9.5, # Per protocol PHQ-15 3 months
    11.5, 9.2, # Per-protocol PHQ-15 6 months
    11.7, 8.8, # Per-protocol PHQ-15 12 months
    
    3.1, 2.4, # Per-protocol working life post treatment
    2.6, 2.5, # Per protocol Working life 3 months
    2.8, 1.9, # Per-protocol working life 6 months
    3.3, 2.0, # Per-protocol working life 12 months
    
    
    4.1, 2.9, # Per-protocol social life post treatment
    3.4, 3.1, # Per protocol social life 3 months
    3.6, 2.6, # Per-protocol social life 6 months
    4.0, 2.6, # Per-protocol social life 12 months
    
    4.0, 2.8, # Per-protocol Family life post treatment
    3.5, 3.0, # Per protocol Family life 3 months
    3.6, 2.6, # Per-protocol Family life 6 months
    3.9, 2.5, # Per-protocol Family life 12 months
    
    
    22.7, 25.1,# Per-protocol Physical post treatment
    23.2, 24.4, # Per protocol Physical 3 months
    23.1, 24.7, # Per-protocol Physical 6 months 
    22.7, 25.6, # Per-protocol Physical 12 months
    
    17.4, 19.9, # Per-protocol Psychological post treatment
    18.0, 19.3, # Per protocol Psychological 3 months
    18.3, 19.3, # Per-protocol Psychological 6 months
    18.3, 20.2, # Per-protocol Psychological 12 months
    
    9.2, 10.0, # Per-protocol Social post treatment
    9.3, 9.8, # Per protocol Social 3 months
    9.6, 9.8, # Per-protocol Social 6 months
    9.3, 10.0, # Per-protocol Social 12 months
    
    25.5, 27.8, # Per-protocol Environment post treatment
    26.1, 27.5, # Per protocol Environment 3 months
    26.4, 27.7, # Per-protocol Environment 6 months
    26.6, 28.3) # Per-protocol Environment 12 months
    ,
  
  sd_post = c(
    5.4, 4.7, # ITT GAD-7 Anxiety post-treatment
    5.3, 5.0, # ITT GAD-7 Standard deviation 3 months
    5.4, 5.1, # ITT GAD-7 Standard deviation 6 months
    5.7, 5.4,  # ITT GAD-7 Standard deviation 12 months
    
    6.4, 5.7, # ITT PHQ9 Depression standard deviation post-treatment 
    6.4, 6.0, # ITT PHQ9 Depression standard deviation 3 months
    6.4, 6.1, # ITT PHQ9 Depression standard deviation 6 months
    6.3, 5.9, # ITT PHQ9 Depression standard deviation 12 months
    
    5.2, 5.4, # ITT PHQ-15 standard deviation post-treatment
    5.1, 5.3, # ITT PHQ-15 standard deviation 3 months
    5.3, 5.6, # ITT PHQ-15 standard deviation 6 months
    5.6, 5.6, # ITT PHQ-15 standard deviation 12 months
    
    3.1, 3.2, # ITT working life standard deviation post-treatment
    3.0, 3.0, # ITT working life standard deviation 3 months
    3.0, 2.9, # ITT working life standard deviation 6 months
    3.3, 3.2, # ITT working life standard deviation 12 months
    
    3.1, 3.0, # ITT social life standard deviation post-treatment
    3.1, 2.9, # ITT social life standard deviation 3 months
    3.2, 3.1, # ITT social life standard deviation 6 months
    3.4, 3.4, # ITT social life standard deviation 12 months
    
    3.1, 2.9, # ITT Family life standard deviation post-treatment
    3.1, 3.1, # ITT Family life standard deviation 3 months
    3.2, 3.1, # ITT Family life standard deviation 6 months
    3.3, 3.2, # ITT Family life standard deviation 12 months
    
    4.5, 4.6, # ITT Physical standard deviation post-treatment
    4.6, 4.8, # ITT Physical standard deviation 3 months
    4.5, 4.4, # ITT Physical standard deviation 6 months
    5.1, 5.3, # ITT Physical standard deviation 12 months
    
    3.9, 4.9, # ITT Psychological standard deviation post-treatment
    3.9, 4.2, # ITT Psychological standard deviation 3 months
    3.8, 3.9, # ITT Psychological standard deviation 6 months
    4.4, 4.6, # ITT Psychological standard deviation 12 months
    
    3.1, 3.1, # ITT Social standard deviation post-treatment
    2.3, 2.3, # ITT Social standard deviation 3 months
    2.5, 2.2, # ITT Social standard deviation 6 months
    2.2, 2.6, # ITT Social standard deviation 12 months
    
    5.3, 5.6, # ITT Environment standard deviation post-treatment
    4.9, 5.1, # ITT Environment standard deviation 3 months
    4.8, 4.8, # ITT Environment standard deviation 6 months
    5.0, 5.3, # ITT Environment standard deviation 12 months
    
    5.5, 4.3, # Per-protocol GAD-7 Anxiety standard deviation post-treatment
    5.4, 4.9, # Per-protocol GAD-7 Anxiety standard deviation 3 months
    5.7, 4.9, # Per-protocol GAD-7 Anxiety standard deviation 6 months
    5.8, 5.3, # Per-protocol GAD-7 Anxiety standard deviation 12 months
    
    6.6, 5.2, # Per-protocol PHQ9 Depression standard deviation post-treatment
    6.5, 6.0, # Per-protocol PHQ9 Depression standard deviation 3 months
    6.6, 6.1, # Per-protocol PHQ9 Depression standard deviation 6 months
    6.5, 6.2, # Per-protocol PHQ9 Depression standard deviation 12 months
    
    5.2, 5.3, # Per-protocol PHQ-15 standard deviation post-treatment
    5.0, 5.4, # Per-protocol PHQ-15 standard deviation 3 months
    5.3, 5.7, # Per-protocol PHQ-15 standard deviation 6 months
    5.6, 5.7, # Per-protocol PHQ-15 standard deviation 12 months
    
    3.1, 2.9, # Per-protocol working life standard deviation post-treatment
    3.0, 2.9, # Per-protocol working life standard deviation 3 months
    3.0, 2.7, # Per-protocol working life standard deviation 6 months
    3.3, 2.7, # Per-protocol working life standard deviation 12 months
    
    3.1, 2.8, # Per-protocol social life standard deviation post-treatment
    3.2, 2.9, # Per-protocol social life standard deviation 3 months
    3.2, 2.8, # Per-protocol social life standard deviation 6 months
    3.3, 3.1, # Per-protocol social life standard deviation 12 months
    
    3.1, 3.1, # Per-protocol Family life standard deviation post-treatment
    3.1, 3.0, # Per-protocol Family life standard deviation 3 months
    3.1, 2.7, # Per-protocol Family life standard deviation 6 months
    .3, 2.8,  # Per-protocol Family life standard deviation 12 months
    
    4.6, 4.7, # Per-protocol Physical standard deviation post-treatment
    4.8, 4.9, # Per-protocol Physical standard deviation 3 months
    4.8, 4.9, # Per-protocol Physical standard deviation 6 months
    5.1, 5.3, # Per-protocol Physical standard deviation 12 months
    
    4.2, 4.0, # Per-protocol Psychological standard deviation post-treatment
    4.0, 4.2, # Per-protocol Psychological standard deviation 3 months
    4.2, 4.2, # Per-protocol Psychological standard deviation 6 months
    4.4, 4.6, # Per-protocol Psychological standard deviation 12 months
    
    2.4, 2.7, # Per-protocol Social standard deviation post-treatment
    2.2, 2.4, # Per-protocol Social standard deviation 3 months
    2.5, 2.2, # Per-protocol Social standard deviation 6 months
    2.2, 2.6, # Per-protocol Social standard deviation 12 months
    
    4.7, 4.8, # Per-protocol Environment standard deviation post-treatment
    4.8, 5.2, # Per-protocol Environment standard deviation 3 months
    4.8, 5.0, # Per-protocol Environment standard deviation 6 months
    5.0, 4.6) # Per-protocol Environment standard deviation 12 months
  ,
  
  q = 3
  
); cano_vindel2021_new 

