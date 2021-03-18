# ==========================================================================
# Recreate Tidytuesday plots (and maybe add something else)
# ==========================================================================

sessionInfo()

library(dplyr)
library(ggplot2)
library(extrafont)

#
# Load data
# --------------------------------------------------------------------------
path <- "tidytuesday/data/2021/2021-03-16"

gms <- read.csv2(list.files(path, full.names = TRUE, pattern = "games"), sep = ",")

#
# Pic 1
# --------------------------------------------------------------------------

cols <- c("#636EFB", "#F0553A", "#09CD96", "#AB63FA", "#FFA15A", "#1DD4F4",
	"#FF6693", "#B6E880", "#FF97FE", "#FECB51")

df_pic1 <- gms %>%
	group_by(gamename) %>%
	summarise(peak_sum = sum(peak / 1000000)) %>%
	ungroup() %>%
	arrange(desc(peak_sum)) %>%
	slice(1L:10L)

df_pic1 <- df_pic1 %>%
	mutate(gamename = factor(gamename,
		levels = gamename[order(peak_sum, decreasing = TRUE)]))

ggplot(df_pic1, aes(x = gamename, y = peak_sum, fill = gamename)) +
	geom_bar(stat = "identity") +
	labs(title = "Top Games based on total sum of their streams") +
	theme(
		panel.background = element_rect(fill = "#E5EDF7"),
		legend.title.align = -0.1,
		legend.title = element_text(margin = margin(t = 0, r = 0, b = 0, l = 5)),
		legend.justification = c(1, 1.02),
		legend.spacing.x = unit(0.4, "cm"),
		plot.title.position = "plot",
		plot.title = element_text(size = 14, margin = margin(t = 20, r = 20, b = 30, l = 8)),
		aspect.ratio = 1.4/1.5,
		axis.ticks = element_blank(),
		axis.text.x = element_text(angle = -40, hjust = 0),
		axis.text = element_text(colour = "#53647E"),
		axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 15)),
		panel.grid.minor = element_blank(),
		panel.grid.major.x = element_blank(),
		text = element_text(family = "Verdana", size = 12, colour = "#53647E")
		) +
	scale_fill_manual(values = setNames(cols, df_pic1$gamename)) +
	scale_y_continuous(labels = function(x) c("0", paste0(seq(20, 80, 20), "M")),
		limits = c(0, max(df_pic1$peak_sum) + 5),
		expand = c(0, 0))

ggsave(file.path("viz", paste0("viz_", Sys.Date(), ".png")), width = 9,
	height = 7)