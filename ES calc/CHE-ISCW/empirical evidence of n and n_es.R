library(dplyr)
library(ggplot2)
library(gridExtra)

#--------------------------------------------------
#
#   Empirical distribution of N_j and n_j
#
#--------------------------------------------------

# graphs for the dist of N_j and n_j

sample_sizes <- readRDS("simulations/empirical-sample-sizes.rds")
View(sample_sizes)

# sample size per group
n_draw <- 
  sample_sizes |> 
  dplyr::select(n) |> 
  mutate(n = n / 2) |> # per group sample size
  mutate(study = "Our paper")

n_draw_summary <- 
  n_draw |> 
  summarise(
    mean = mean(n),
    min = min(n),
    max = max(n),
    median = median(n)
  )
n_draw_summary

# Carter: simulate truncated Inv gamma (truncated at n=5 and n=1905)
n_Carter <- invgamma::rinvgamma(n = 100000, shape = 1.153, scale = 0.045)
n_Carter <- 
  as.data.frame(n_Carter) |> 
  rename(n = n_Carter) |> 
  filter(n > 5, n < 1905) |> 
  mutate(study = "Carter et al.")

# McShane
n_McShane <- runif(n = 100000, min = 25, max = 100)
n_McShane <- 
  as.data.frame(n_McShane) |> 
  rename(n = n_McShane) |> 
  mutate(study = "McShane et al.")

# Moreno
n_Moreno <- rlnorm(n = 100000, meanlog = 6, sdlog = 0.6)
n_Moreno <- 
  as.data.frame(n_Moreno) |> 
  rename(n = n_Moreno) |> 
  mutate(study = "Moreno et al.",
         n = n / 2)

# put all sample sizes together
Ns <- bind_rows(n_draw, n_Carter, n_McShane, n_Moreno)

graph_Ns <- 
  Ns |>
  filter(n < 300) |> 
  ggplot(aes(x = n, fill = study, color = study)) +
  geom_density(alpha = 0.2) + 
  theme_minimal() +
  labs(x = "Sample size per group", y = "Dernsity", color = "", fill = "") +
  theme(legend.position = c(0.5, 0.8))

# percentile of sample sizes per study
Ns_total <- Ns |> mutate(N = n*2)
quantile(Ns_total$N, c(.05, .25, .8)) 

# Number of effect sizes per study
n_es_summary <- 
  sample_sizes |> 
  summarise(
    mean = mean(n_ES),
    min = min(n_ES),
    max = max(n_ES),
    median = median(n_ES)
  )
n_es_summary

N_dist <- rnbinom(10000, size = 4, prob = 0.5)
as.data.frame(N_dist) %>% 
  ggplot(aes(x = N_dist)) +
  geom_density(fill = "purple", color = "purple", alpha = .2) +
  stat_function(fun = dgamma,
                args = list(shape = 2, scale = 2.2))

graph_ns <- 
  sample_sizes |> 
  ggplot(aes(x = n_ES)) +
  geom_density(fill = "purple", color = "purple", alpha = .2) +
  # stat_function(fun = dnbinom,
  #               args = list(prob = 0.45, size = 3.4)) +
  # stat_function(fun = dgamma,
  #               args = list(shape = 2, scale = 2.2)) +
  labs(y = "Density") +
  theme_minimal() +
  labs(x = "Number of effects per study", y = "Dernsity", color = "", fill = "")

es_dist <- arrangeGrob(graph_Ns, graph_ns, ncol = 2)
ggsave("simulations/graphs/es-dist.png", es_dist, 
       device = "png", dpi = 500, height = 4, width = 6)




# simulate negative binomial???

n_ES_draw <- 
  sample_sizes |> 
  dplyr::select(n_ES) |> 
  mutate(study = "Our paper")

n_Nbinom <- rnbinom(100000, mu = 4, size = 2)
n_Nbinom <- 
  as.data.frame(n_Nbinom) |> 
  rename(n_ES = n_Nbinom) |> 
  mutate(study = "NegativeBinomial")

bind_rows(n_ES_draw, n_Nbinom) |> 
  filter(n_ES < 20) |> 
  ggplot(aes(x = n_ES, fill = study, color = study)) +
  geom_density(alpha = 0.2) +
  theme_minimal() +
  labs(x = "Number of effects per study", y = "Dernsity", color = "", fill = "") +
  theme(legend.position = c(0.5, 0.8))

# Mikkel et al
Dietrichson_sample_sizes <- readRDS("simulations/chtc/Dietrichson-sample-sizes.rds")
names(Dietrichson_sample_sizes) <- c("n_ES", "N")
Dietrichson_sample_sizes$study <- "Dietrichson"

Burnette_sample_sizes <- readRDS("simulations/Burnette-sample-sizes.rds")
Burnette_sample_sizes$study <- "Burnette"

dat <- rbind(Dietrichson_sample_sizes, Burnette_sample_sizes)

# number of effect sizes per study: gamma distribution
dat |> 
  ggplot(aes(x = n_ES)) +
  geom_density(aes(fill = study, color = study), alpha = .2) +
  labs(y = "Density") +
  # stat_function(fun = dnbinom, args = list(mu = 4, size = 2))
  stat_function(fun = dgamma, # negative binomial
                args = list(shape = 2, scale = 2.1))

# negative binomial: plot(density(rnbinom(1000, mu = 4, size = 2)))

# sample size per study
dat |>
  ggplot(aes(x = N)) +
  geom_density(aes(fill = study, color = study), alpha = .2) +
  labs(y = "Dernsity")

# per-group sample size in the primary study
## Carter et al: truncated inverse gamma (n = 5, 1905)
library(invgamma)
dat |>
  dplyr::mutate(N_g = N / 2) |>
  ggplot(aes(x = N_g)) +
  geom_density(aes(fill = study, color = study), alpha = .4) +
  stat_function(fun = invgamma::dinvgamma, 
                args = list(shape = 1.153, scale = 0.045))

dat |>
  dplyr::mutate(N_g = N / 2) |>
  ggplot(aes(x = N_g)) +
  geom_density(aes(fill = study, color = study), alpha = .4) +
  stat_function(fun = invgamma::dinvgamma, 
                args = list(shape = 0.4, scale = 0.05))

# simulate truncated inverse gamma (truncated at n=5 and n=1905 in Carter et al.)
n <- invgamma::rinvgamma(n = 100000, shape = 1.153, scale = 0.045)
n |>
  as.data.frame() |>
  dplyr::filter(n > 5, n < 1905) |>
  ggplot(aes(x = n)) +
  geom_density() +
  scale_x_continuous(limits = c(5, 250))
  # geom_histogram(binwidth = 50)

# save both datasets for drawing n and n_es
names(dat) <- c("n_ES", "n", "studyid")
write_rds(dat, file = "simulations/empirical-sample-sizes.rds")



#-------------------------------------  
#           Discarded
#-------------------------------------  
# Dai et al. 2023
Dai_dat <- read.csv("resources/Dai_data.csv")
Dai_sample_sizes <- 
  Dai_dat %>% 
  group_by(uniquestudy) %>% 
  summarise(n_es = n(),
            n = mean(n)) %>% 
  filter(n < 1000)
hist(Dai_sample_sizes$n_es)
hist(Dai_sample_sizes$n)

# Burnette et al. 2023
# drop the national growth mindset study (giantic)
# I like this one, six outcome categories (in separate data files), collect pre-registration info.
Burnette_academic <- 
  read.csv("resources/Burnette data/academic.csv") %>% mutate(outcome = "academic") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)
Burnette_behavior <- 
  read.csv("resources/Burnette data/behavior.csv") %>% mutate(outcome = "behavior") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)
Burnette_expectations <- 
  read.csv("resources/Burnette data/expectations.csv") %>% mutate(outcome = "expectations") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)
Burnette_health <- 
  read.csv("resources/Burnette data/health.csv") %>% mutate(outcome = "health") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)
Burnette_mindset <- 
  read.csv("resources/Burnette data/mindset.csv") %>% mutate(outcome = "mindset") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)
Burnette_social <- 
  read.csv("resources/Burnette data/social.csv") %>% mutate(outcome = "social") %>% 
  select(Effect.ID, Manuscript.ID, Study.Number, Sample.ID, Within.Sample.Effect.Number, 
         Unique.Study.ID, Treatment.N, Control.N, outcome)

Burnette_dat <- 
  rbind(Burnette_academic, Burnette_behavior, Burnette_expectations, 
        Burnette_health, Burnette_mindset, Burnette_social) %>% 
  mutate(N = Treatment.N + Control.N) %>% 
  filter(!is.na(N), N < 500)

Burnette_n_es <-   
  Burnette_dat %>% 
  group_by(Unique.Study.ID) %>% 
  count()

Burnette_N <- 
  Burnette_dat %>% 
  group_by(Unique.Study.ID) %>% 
  summarise(N = round(mean(N), 0))

Burnette_sample_sizes <- 
  Burnette_N %>% 
  inner_join(Burnette_n_es, by = "Unique.Study.ID") %>% 
  rename(n_ES = n) %>% 
  select(N, n_ES)
# subsample of 40 studies with 189 unique effect sizes
# sample sizes of no more than 500, and no more than 20 effect sizes per study
# sample sizes ranged from 17 to 352, with a mean of 147, with a median of 104, sd of 100
# number of effect sizes: ranged from 1 to 19, with a median of 3.5 and a mean of 4.7, sd of 3.6

write_rds(Burnette_sample_sizes, file = "simulations/Burnette-sample-sizes.rds")


# Kalen et al. 2021, 142 studies, 1227 es, 8860 participants, sports
Kalen_dat <- read.csv("resources/Kalen_data.csv")
Kalen_sample_sizes <- 
  Kalen_dat %>% 
  group_by(study_id) %>% 
  summarise(n_es = n(),
            n = mean(sample_size)) %>% 
  filter(n_es < 40, n < 300)
hist(Kalen_sample_sizes$n_es)
hist(Kalen_sample_sizes$n)
