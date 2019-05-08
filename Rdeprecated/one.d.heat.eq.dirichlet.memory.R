one.d.heat.eq.dirichlet.memory <- function(n, init.dat, alpha, dt, eps, dx) {
  du <- rep(0, length(init.dat))
  lambda <- dt*alpha/(2*dx)
  init <- init.dat
  for (i in seq_len(n)[-1]) {
    du[1] <- 2*init[1]-init[2]
    for (j in seq_along(du)[-c(1, length(du))]) {
      du[j] <- 2*init[j]-init[j-1]-init[j+1]
    }
    du[length(du)] <- 2*init[length(du)]-init[length(du)-1]
    init <- init - lambda*du
    init[init < eps] <- 0
  }
  init
}
