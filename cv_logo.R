library(iprior)
library(tidyverse)
library(gganimate)

# Generate data and fit I-prior model
dat <- gen_smooth(seed = 123)
mod <- iprior(y ~ X, dat, kernel = "fbm", est.hurst = TRUE, 
              control = list(silent = TRUE))
x.new <- seq(min(dat$X), max(dat$X), length = 1000)
f.hat <- predict(mod, data.frame(X = x.new))$y

# Obtain the I-prior covariance kernel
psi <- get_psi(mod)
lambda <- 2  # get_lambda(mod) 
gamma <- get_hurst(mod)
Vf.pri <- psi * tcrossprod(lambda * kern_fbm(dat$X, x.new, gamma = gamma))

# Draw from the I-prior and prepare the data set
no.of.draws <- 130
draw.pri <-
  mvtnorm::rmvnorm(no.of.draws, mean = rep(get_intercept(mod), nrow(Vf.pri)),
                   sigma = Vf.pri) %>% t()
melted.pri <- reshape2::melt(data.frame(f = draw.pri, x = x.new), id.vars = "x")

# https://stackoverflow.com/questions/58271332/gganimate-plot-where-points-stay-and-line-fades

# Plot and animate
melted.pri %>%
  as_tibble() %>%
  rename(y = value) %>%
  mutate(time = as.numeric(factor(variable))) %>%
  uncount(length(unique(time)), .id = "frame") %>%
  filter(time <= frame) %>%
  arrange(frame, time) %>%
  group_by(frame) %>%
  mutate(tail = last(time) - time,
         line_alpha = pmax(0, (20 - tail) / 20)) %>%
  ungroup() %>% 
  subset(frame > 30) %>%
  ggplot() +
  scale_x_continuous(
    limits = c(min(x.new), max(x.new)),
    breaks = NULL, name = expression(italic(x))
  ) +
  scale_y_continuous(
    limits = c(min(dat$y) - 5, max(dat$y) + 5),
    breaks = NULL, name = expression(italic(y))
  ) +
  geom_line(aes(x, y, group = variable, alpha = line_alpha), 
            col = "steelblue3", size = 0.3) +
  geom_point(data = dat, aes(x = X, y = y), col = "grey60") +
  theme_void() +
  theme(legend.position = "none") +
  transition_manual(frame) -> p.anim

melted.pri %>%
  as_tibble() %>%
  rename(y = value) %>%
  mutate(time = as.numeric(factor(variable))) %>%
  # subset(time <= 100) %>%
  ggplot() +
  scale_x_continuous(
    limits = c(min(x.new), max(x.new)),
    breaks = NULL, name = expression(italic(x))
  ) +
  scale_y_continuous(
    limits = c(min(dat$y) - 5, max(dat$y) + 5),
    breaks = NULL, name = expression(italic(y))
  ) +
  geom_line(aes(x, y, group = variable), alpha = 0.3, 
            col = "steelblue3", size = 0.25) +
  geom_point(data = dat, aes(x = X, y = y), col = "grey60") +
  geom_line(data = data.frame(x = x.new, y = f.hat), aes(x, y),
            col = "red3") +
  theme_void() -> p.static
