# theme details
my_theme <- function() {
  theme_minimal(base_size = 10, base_family = "serif") +
    theme(
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10) 
    )
}
fave_colours <- c(
  "coral", "goldenrod2", "khaki2", 
  "aquamarine3", "turquoise2", "powderblue", "deepskyblue3", 
  "plum", "lightpink1", "lavenderblush2")


more_colours <- c(
  "firebrick3", "tomato", "coral", "darkorange2", "sienna3", "tan2",
  "goldenrod2", "lemonchiffon2", "khaki2", "gold1",
  "springgreen3", "darkseagreen4", "seagreen", "olivedrab4", "darkcyan", "aquamarine3", "palegreen3",
  "skyblue4", "deepskyblue3", "cornflowerblue", "powderblue", "royalblue4", "slateblue2", "lightslateblue", "lightcyan4",
  "purple4", "mediumpurple3", "mediumorchid", "plum", "orchid3", "thistle",
  "deeppink3", "hotpink", "lightpink1", "rosybrown1",
  "mistyrose3", "lavenderblush2", "seashell2"
)
