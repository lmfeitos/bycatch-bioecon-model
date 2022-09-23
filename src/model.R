


n_strong <- function(K_s, K_w, r_s, r_w, q_s, q_w, p_s, p_w, c){
  N_s <- ( K_s * (c*q_s*r_w + K_w * p_w * q_w * (q_w * r_s - q_s * r_w)  ) ) / (K_w * p_w * q_w^2 * r_s + K_s * p_s * q_s^2 * r_w )
  return(N_s)
}

n_weak <- function(K_s, K_w, r_s, r_w, q_s, q_w, p_s, p_w, c){
  N_s <- ( K_w * (c*q_w*r_s + K_s * p_s * q_s * (-q_w * r_s + q_s * r_w)  ) ) / (K_w * p_w * q_w^2 * r_s + K_s * p_s * q_s^2 * r_w )
  return(N_s)
}

e <- function(K_s, K_w, r_s, r_w, q_s, q_w, p_s, p_w, c){
  e <- ( (-c + K_s * p_s * q_s + K_w * p_w * q_w) * r_s * r_w ) / (K_w * p_w * q_w^2 * r_s + K_s * p_s * q_s^2 * r_w)
  return(e)
}

r_s <- 1
r_w <- 1
K_s <- 1
K_w <- 1
q_s <- 0.1
q_w <- 0.15
p_s <- 1
c <- 0.03333333


p_w_vec <- seq(0, 10, 0.1)
n_weak_vec <- n_weak(K_s, K_w, r_s, r_w, q_s, q_w, p_s, p_w=p_w_vec, c)
n_weak_vec_adj <- 0.0025 / (0.01 + 0.0225 * p_w_vec)
plot(n_weak_vec_adj ~ p_w_vec)
