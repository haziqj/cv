library(iprior)
library(tidyverse)
library(gganimate)

# Generate data and fit I-prior model
n <- 150
dat <- 
  gen_smooth(n = n, seed = 123) %>%
  rename(x = X)
mod <- iprior(y ~ x, dat, kernel = "se", est.lengthscale = TRUE, 
              control = list(silent = TRUE))

# New data points
x.new <- seq(min(dat$x), max(dat$x), length = 50)
f.hat <- predict(mod, data.frame(x = x.new))$y

# Obtain the I-prior covariance kernel
psi <- get_psi(mod)
lambda <- get_lambda(mod) 
gamma <- get_lengthscale(mod) 
Hlam.star <- lambda * kern_se(dat$x, x.new, l = gamma)
Vf.pri <- psi * tcrossprod(Hlam.star)
Vy <- psi * tcrossprod(kern_se(dat$x, l = gamma)) + diag(1 / psi, n)
Vf.pos <- Hlam.star %*% solve(Vy, t(Hlam.star))

# Draw from the I-prior and prepare the data set
no.of.draws <- 100
draw.pri <-
  mvtnorm::rmvnorm(no.of.draws, mean = rep(get_intercept(mod), nrow(Vf.pri)),
                   sigma = Vf.pri) %>% t()
draw.pos <-
  mvtnorm::rmvnorm(no.of.draws, mean = f.hat, sigma = Vf.pos) %>% t()
melted.pri <- reshape2::melt(data.frame(f = draw.pri, x = x.new), id.vars = "x", 
                             value.name = "y")
melted.pos <- reshape2::melt(data.frame(f = draw.pos, x = x.new), id.vars = "x", 
                             value.name = "y")

# Draw the fitted line as the model learns it
set.seed(456)
the.samp <- sample(nrow(dat))  
res <- NULL
for (k in 1:150) {
  mod <- iprior(y ~ x, dat[the.samp[1:k], ], kernel = paste0("se,", gamma), 
                lambda = lambda, psi = psi, fixed.hyp = TRUE, est.psi = FALSE, 
                method = "em", control = list(silent = TRUE))
  f.hat <- predict(mod, data.frame(x = x.new))$y
  res <- bind_rows(res, tibble(x = x.new, y = f.hat, k = k))
}


# This data frame is for the appearing points
the.index <- rep(the.samp, n)[c(t(lower.tri(diag(n), diag = TRUE)))]
point.df <- bind_cols(dat[the.index, ], k = rep(1:n, 1:n))

# Animated plot
ggplot(res, aes(x, y)) +
  geom_line(data = melted.pos, aes(group = variable), alpha = 0.3,
            col = "steelblue3", size = 0.25) +
  geom_point(data = dat, col = "grey60", shape = 1, size = 1.2) +
  geom_point(data = point.df, size = 1.2) +
  geom_point(data = bind_cols(dat[the.samp, ], k = 1:n),
             size = 8, col = "red3", alpha = 0.4) +
  geom_line(col = "red3", size = 1) +
  # labs(title = "State: {closest_state}", subtitle = "Frame {frame} of {nframes}") +
  transition_states(k, transition_length = 4, wrap = FALSE) +
  ease_aes("cubic-in-out") +
  theme_void() -> p.anim

# Static plot
ggplot(subset(res, k <= n), aes(x, y)) + 
  geom_line(data = melted.pos, aes(group = variable), alpha = 0.3, 
            col = "steelblue3", size = 0.25) +
  geom_point(data = dat, col = "grey30", size = 1.2) +
  geom_line(data = tibble(x = x.new, y = f.hat), col = "red3", size = 1) +
  theme_void() -> p.static

# Old version ------------------------------------------------------------------
# 
# # https://stackoverflow.com/questions/58271332/gganimate-plot-where-points-stay-and-line-fades
# 
# # Plot and animate
# melted.pri %>%
#   as_tibble() %>%
#   mutate(time = as.numeric(factor(variable))) %>%
#   uncount(length(unique(time)), .id = "frame") %>%
#   filter(time <= frame) %>%
#   arrange(frame, time) %>%
#   group_by(frame) %>%
#   mutate(tail = last(time) - time,
#          line_alpha = pmax(0, (20 - tail) / 20)) %>%
#   ungroup() %>% 
#   subset(frame > 30) %>%
#   ggplot() +
#   scale_x_continuous(
#     limits = c(min(x.new), max(x.new)),
#     breaks = NULL, name = expression(italic(x))
#   ) +
#   scale_y_continuous(
#     limits = c(min(dat$y) - 5, max(dat$y) + 5),
#     breaks = NULL, name = expression(italic(y))
#   ) +
#   geom_line(aes(x, y, group = variable, alpha = line_alpha), 
#             col = "steelblue3", size = 0.3) +
#   geom_point(data = dat, aes(x = X, y = y), col = "grey60") +
#   theme_void() +
#   theme(legend.position = "none") +
#   transition_manual(frame) -> p.anim
# 
# melted.pri %>%
#   as_tibble() %>%
#   rename(y = value) %>%
#   mutate(time = as.numeric(factor(variable))) %>%
#   # subset(time <= 100) %>%
#   ggplot() +
#   scale_x_continuous(
#     limits = c(min(x.new), max(x.new)),
#     breaks = NULL, name = expression(italic(x))
#   ) +
#   scale_y_continuous(
#     limits = c(min(dat$y) - 5, max(dat$y) + 5),
#     breaks = NULL, name = expression(italic(y))
#   ) +
#   geom_line(aes(x, y, group = variable), alpha = 0.3, 
#             col = "steelblue3", size = 0.25) +
#   geom_point(data = dat, aes(x = X, y = y), col = "grey60") +
#   geom_line(data = data.frame(x = x.new, y = f.hat), aes(x, y),
#             col = "red3") +
#   theme_void() -> p.static
