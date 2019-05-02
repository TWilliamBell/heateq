c_one.d.heat.eq <- function(n = 1000, init.dat = dgamma(seq(0, 5, 0.01), 
                                                        shape = 3), boundary = "neumann", 
                            alpha = 1, dt = 0.1, eps = 1e-5, 
                            save.intermediates = F) {
  boundary <- stringr::str_to_lower(boundary)
  if (isTRUE(save.intermediates)) {
    c_heat(n, init = init.dat, boundary, alpha, dt, eps)
  } else {
    c_heat_memory(n, init = init.dat, boundary, alpha, dt, eps)
  }
}