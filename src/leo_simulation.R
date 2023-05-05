
# Setup simulation
#############################################################

# Packages
library(tidyverse)

# Simulation parameters
nyr <- 40

# Cross stock parameters
cost <- 0.1
alpha <- 1

# Weak stock parameters
r_w <- 0.2
k_w <- 10
q_w <- 0.05
p_w <- 1
beta_w <- 1
f_w <- 0
A_w <- 1

# Strong stock parameters
r_s <- 0.3
k_s <- 12
q_s <- 0.05
p_s <- 1.3
beta_s <- 1
f_s <- 0
A_s <- 1

# Setup container
# N=biomass; E=effort; U=fishing mortality; C=catch; R=revenues
data <- tibble(year = 1:nyr,
               N_w = NA,
               E_w = NA,
               U_w = NA,
               C_w = NA,
               R_w = NA,
               p_w_f = NA,
               N_s = NA,
               E_s = NA,
               U_s = NA,
               C_s = NA,
               R_s = NA,
               p_s_f = NA)

# Record first year
data$N_w[1] <- k_w
data$N_s[1] <- k_s
data$E_w[1] <- data$E_s[1] <- 0
data$C_w[1] <- data$C_s[1] <- 0
data$U_w[1] <- data$U_s[1] <- 0
data$R_w[1] <- data$R_s[1] <- 0
data$p_s_f[1] <- A_s * (q_s * k_s^beta_s-1 * 0)^-f_s
data$p_w_f[1] <- A_w * (q_w * k_w^beta_w-1 * 0)^-f_w

# Run simulation for constant prices
#############################################################

# Loop through years
i <- 2
for(i in 2:nyr){

  # Get previous biomass
  N_w_prev <- data$N_w[i-1]
  N_s_prev <- data$N_s[i-1]

  # Calculate effort
  E_now <- (p_s*q_s*N_s_prev + p_w*q_w*N_w_prev) - cost*E_now

  # Calculate catch
  C_w_now <- q_w * E_now * N_w_prev
  C_s_now <- q_s * E_now * N_s_prev

  # Calculate fishing mortality (as an exploitation rate)
  U_w_now <- C_w_now / N_w_prev
  U_s_now <- C_s_now / N_s_prev

  # Calculate revenues
  R_w_now <-  C_w_now * p_w
  R_s_now <-  C_s_now * p_s

  # Calculate surplus production
  SP_w_now <- r_w*N_w_prev*(1- N_w_prev/k_w)
  SP_s_now <- r_s*N_s_prev*(1- N_s_prev/k_s)

  # Calculate biomass
  N_w_now <- N_w_prev + SP_w_now - C_w_now
  N_s_now <- N_s_prev + SP_s_now - C_s_now

  # Record results
  data$N_w[i] <- N_w_now
  data$N_s[i] <- N_s_now
  data$E_w[i] <- E_now
  data$E_s[i] <- E_now
  data$U_w[i] <- U_w_now
  data$U_s[i] <- U_s_now
  data$C_w[i] <- C_w_now
  data$C_s[i] <- C_s_now
  data$R_w[i] <- R_w_now
  data$R_s[i] <- R_s_now

}

# Wrangle data
#############################################################

# Build stock labels
stock_label_key <- tibble(stock=c("Strong", "Weak"),
                          r=c(r_s, r_w),
                          k=c(k_s, k_w),
                          q=c(q_s, q_w),
                          p=c(p_s, p_w)) %>%
  mutate(stock_label=paste0(stock, " (r=", r, ", k=", k, ", q=", q, ", p=", p, ")"))

# Format data
data1 <- data %>%
  # Gather
  gather(key="variable", value="value", 2:ncol(.)) %>%
  # Split variable and stock
  separate(col="variable", into=c("variable", "stock"), sep="_", remove=T) %>%
  # Format stock
  mutate(stock=recode(stock,
                      "s"="Strong",
                      "w"="Weak")) %>% 
  # Format variable
  mutate(variable=recode(variable,
                         "E"="Effort",
                         "C"="Catch",
                         "N"="Abundance",
                         "U"="Fishing mortality rate",
                         "R"="Revenues")) %>% 
  # Add stock label
  left_join(stock_label_key %>% select(stock, stock_label), by='stock') %>%
  # Arrange
  select(year, stock, stock_label, variable, value, everything())


# Plot simulation
#############################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data1, aes(x=year, y=value, color=stock_label)) +
  facet_wrap(~variable, nrow=1, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="Year", y="") +
  lims(y=c(0, NA)) +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g


# Run simulation for prices as functions of catch
#############################################################

# Setup container
# N=biomass; E=effort; U=fishing mortality; C=catch; R=revenues
data_f <- tibble(year = 1:nyr,
               N_w = NA,
               E_w = NA,
               U_w = NA,
               C_w = NA,
               R_w = NA,
               p_w_f = NA,
               cpue_w = NA,
               cost_w = NA,
               pi_w = NA,
               N_s = NA,
               E_s = NA,
               U_s = NA,
               C_s = NA,
               R_s = NA,
               p_s_f = NA,
               cpue_s = NA,
               cost_s = NA,
               pi_s = NA)

# Record first year
data_f$N_w[1] <- k_w
data_f$N_s[1] <- k_s
data_f$E_w[1] <- data_f$E_s[1] <- 0
data_f$C_w[1] <- data_f$C_s[1] <- 0
data_f$U_w[1] <- data_f$U_s[1] <- 0
data_f$R_w[1] <- data_f$R_s[1] <- 0
data_f$cpue_w[1] <- data_f$cpue_s[1] <- 0
data_f$cost_w[1] <- data_f$cost_s[1] <- 0
data_f$pi_w[1] <- data_f$pi_s[1] <- 0
data_f$p_s_f[1] <- A_s * (q_s * k_s^beta_s-1 * 0)^-f_s
data_f$p_w_f[1] <- A_w * (q_w * k_w^beta_w-1 * 0)^-f_w


# Loop through years
i <- 2
for(i in 2:nyr){
  
  # Get previous biomass
  N_w_prev <- data_f$N_w[i-1]
  N_s_prev <- data_f$N_s[i-1]
  
  # Calculate effort
  E_now <- alpha * E_now * (A_s * (q_s * N_s_prev^beta_s[i-1] * E_now)^-f_s * q_s * N_s_prev^beta_s[i-1] + 
    A_w * (q_w * N_w_prev^beta_w[i-1] * E_now)^-f_w * q_w * N_w_prev^beta_w[i-1]) - (cost * E_now)
  
  # Calculate prices
  p_s_f <- A_s * (q_s * N_s_prev^beta_s[i- 1] * E_now)^-f_s * q_s * N_s_prev^beta_s[i-1]
  p_w_f <- A_w * (q_w * N_w_prev^beta_w[i- 1] * E_now)^-f_w * q_w * N_w_prev^beta_w[i-1]
  
  # Calculate catch
  C_w_now <- q_w * E_now * N_w_prev^beta_w[i-1]
  C_s_now <- q_s * E_now * N_s_prev^beta_s[i-1]
  
  # Calculate CPUE
  CPUE_s_now <- C_s_now / E_now
  CPUE_w_now <- C_w_now / E_now
  
  # Calculate costs
  cost_s_now <- cost * E_now / C_s_now
  cost_w_now <- cost * E_now / C_w_now
  
  # Calculate fishing mortality (as an exploitation rate)
  U_w_now <- C_w_now / N_w_prev^beta_w[i-1]
  U_s_now <- C_s_now / N_s_prev^beta_s[i-1]
  
  # Calculate revenues
  R_w_now <-  C_w_now * p_w_f
  R_s_now <-  C_s_now * p_s_f
  
  # Calculate profits
  pi_w_now <- R_w_now - cost_w_now
  pi_s_now <- R_s_now - cost_s_now
  
  # Calculate surplus production
  SP_w_now <- r_w*N_w_prev^beta_w[i-1]*(1- N_w_prev^beta_w[i-1]/k_w)
  SP_s_now <- r_s*N_s_prev^beta_s[i-1]*(1- N_s_prev^beta_s[i-1]/k_s)
  
  # Calculate biomass
  N_w_now <- N_w_prev + SP_w_now - C_w_now
  N_s_now <- N_s_prev + SP_s_now - C_s_now
  
  # Record results
  data_f$N_w[i] <- N_w_now
  data_f$N_s[i] <- N_s_now
  data_f$E_w[i] <- E_now
  data_f$E_s[i] <- E_now
  data_f$U_w[i] <- U_w_now
  data_f$U_s[i] <- U_s_now
  data_f$C_w[i] <- C_w_now
  data_f$C_s[i] <- C_s_now
  data_f$R_w[i] <- R_w_now
  data_f$R_s[i] <- R_s_now
  data_f$p_s_f[i] <- p_s_f
  data_f$p_w_f[i] <- p_w_f
  data_f$cpue_s[i] <- CPUE_s_now
  data_f$cpue_w[i] <- CPUE_w_now
  data_f$cost_s[i] <- cost_s_now
  data_f$cost_w[i] <- cost_w_now
  data_f$pi_s[i] <- pi_s_now
  data_f$pi_w[i] <- pi_w_now
  
}





