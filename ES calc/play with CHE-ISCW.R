
dat <- 
  reintergation_dat |> 
  mutate(Va = (1/N_t + 1/N_c))


rho <- 0.8
# CHE pub bias
V_mat <- vcalc(Va, cluster = author_year, obs = esid, data = dat, rho = rho)

W <- solve(V_mat)

optimizers <- c("nlminb","nloptr","Rvmmin","BFGS")
mod <- "Non-converged"
i <- 1L

while (!inherits(mod, "rma.mv") & i <= 4L) {
    mod <- tryCatch(
      rma.mv(
        yi = gt,
        V = V_mat,
        W = W,
        mods = ~ 1,
        random = ~ 1 | author_year / esid,
        data = dat,
        sparse = TRUE,
        control = list(optimizer=optimizers[i])
      ),
      error = function(e) "Non-converged"
    )
    i <- i + 1L
}

mod_robu <- robust(mod, cluster = author_year, clubSandwich = TRUE) 
mod_robu



