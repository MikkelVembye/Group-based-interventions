library(tidyverse)
library(readxl)

gb_dat <- 
  readRDS("Data/gb_dat.rds") |> 
  mutate(
    trt_type = if_else(trt_type == "Group psychoeducation including CBT", "Group Psychoeducation", trt_type)
  )

intervention_meta <- 
  gb_dat |> 
  summarise(
    in_meta = "Yes",
    .by = c(trt_type, study)
  ) |> 
  summarise(
    n = n(),
    .by = trt_type
  )

trt_type_in_meta <- 
  gb_dat |>
  summarise(
  in_meta = "Yes",
  .by = c(trt_type, study)
) |> 
  relocate(study)

trt_type_not_meta <- 
  read_xlsx("Data/trt_type_not_meta.xlsx") |> 
  mutate(
    in_meta = "No"

  )

bind_rows(trt_type_in_meta, trt_type_not_meta) |> 
  summarise(
    n = n(),
    .by = trt_type
  ) |> View()
  
