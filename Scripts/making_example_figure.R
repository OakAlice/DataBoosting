# This is code to make an example figure for the paper --------------------
# I want to visually demonstrate the effect/goal of augmentation
# most of this code was claude generated

set.seed(12)

# Generate a fake example triaxial waveform
n <- 300
t <- seq(0, 3, length.out = n)
original <- tibble(
  time = t,
  X = 0.65 * sin(2 * pi * 1.3 * t) + 0.18 * sin(2 * pi * 3.1 * t),
  Y = 0.48 * sin(2 * pi * 1.3 * t + 0.9) + 0.12 * cos(2 * pi * 2.2 * t),
  Z = 0.35 * cos(2 * pi * 1.3 * t + 0.3) + 0.10 * sin(2 * pi * 2.7 * t)
)

# Augmentation functions
augment <- list(
  "Original" = function(d) d,
  "Amplitude scaling" = function(d) {
    d %>% mutate(across(X:Z, ~ .x * 1.45))
  },
  "Additive noise" = function(d) {
    d %>% mutate(across(X:Z, ~ .x + rnorm(n(), sd = 0.1)))
  },
  "Time warping" = function(d) {
    source <- pmax(1, pmin(n, seq(1, n, length.out = n) +
                             17 * sin(2 * pi * seq(0, 1, length.out = n))))
    d %>% mutate(across(X:Z, ~ approx(seq_len(n), .x,
                                      xout = source)$y))
  },
  "Temporal shift" = function(d) {
    d %>% mutate(across(X:Z, ~ dplyr::lead(.x, 35, default = first(.x))))
  },
  "Axis flip (X)" = function(d) {
    d %>% mutate(X = -X)
  },
  "Axis rotation" = function(d) {
    angle <- 35 * pi / 180
    d %>% mutate(
      X = cos(angle) * X - sin(angle) * Y,
      Y = sin(angle) * X + cos(angle) * Y
    )
  }
)

# Convert each augmented signal to long format
plot_data <- lapply(names(augment), function(method) {
  dat <- augment[[method]](original)
  
  dat %>%
    pivot_longer(X:Z, names_to = "axis", values_to = "acceleration") %>%
    mutate(augmentation = method)
}) %>% bind_rows()

plot_data$augmentation <- factor(
  plot_data$augmentation,
  levels = names(augment)
)

# Plot
ggplot(plot_data, aes(time, acceleration, colour = axis)) +
  geom_line(linewidth = 2) +
  facet_wrap(~ augmentation, ncol = 2) +
  scale_colour_manual(
    values = c(X = "coral", Y = "goldenrod2", Z = "#4a708b"),
    name = "Axis"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    x = "Time (s)",
    y = "Acceleration (g)"
  ) +
  my_theme() + theme(legend.position = "none")
