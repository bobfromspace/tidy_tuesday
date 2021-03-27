# ==========================================================================
# Recreate Tidytuesday plots (and maybe add something else)
# ==========================================================================

sessionInfo()

library(dplyr)
library(ggplot2)
library(extrafont)
library(grid)
library(gridExtra)
library(magick)

#
# Load data
# --------------------------------------------------------------------------
path <- "tidytuesday/data/2021/2021-03-16"
logo <- "https://static.wikia.nocookie.net/terraria_gamepedia/images/c/c8/Logo-11.png/revision/latest?cb=20170910164428"

gms <- read.csv2(list.files(path, full.names = TRUE, pattern = "games"), sep = ",")
img <- image_read(logo)

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

#
# My viz: Terraria (ine of my favourite game)
# --------------------------------------------------------------------------
levs <- c("January", "February", "March", "April", "May", "June", "July", "August", "September",
	"October", "November", "December")
terraria <- filter(gms, grepl("Terraria", gamename)) %>%
	mutate(avg = as.numeric(avg),
		month = factor(month, levels = rev(levs), ordered = TRUE))

png(file.path("viz", paste0("viz_", Sys.Date(), ".png")))
ggplot(terraria, aes(year, month, fill = avg / 1000)) +
	geom_tile(alpha = 0.8) +
	scale_x_continuous(breaks = unique(terraria$year),
		limits = c(min(terraria$year) - 0.5, max(terraria$year) + 1.7)) +
	scale_fill_gradientn(colours = c("#822523", "#EAEFEC", "#E5CA2C"),
		breaks = seq(25, 125, 25),
		labels = paste0(seq(25, 125, 25), "K")
		) +
	theme(
		legend.position = "bottom",
		legend.background = element_rect(fill = "#3F6961"),
		legend.title = element_blank(),
		legend.box.margin = margin(t = 0.4, r = 0, b = 0, l = 0, "cm"),
		axis.ticks = element_blank(),
		axis.text = element_text(colour = "#BBA334"),
		axis.text.y = element_text(margin = margin(2, -0.5, 2, 0.5, "cm")),
		axis.title = element_blank(),
		panel.background = element_blank(),
		plot.caption = element_text(size = 11),
		plot.title = element_text(family = "Berlin Sans FB",
			size = 20, colour = "#BBA334",
			margin = margin(t = 2, l = 1, r = 1, b = 0, "cm"),
			hjust = 0.5
			),
		plot.subtitle = element_text(family = "Berlin Sans FB", hjust = 0.5,
			margin(t = 0, l = 1, r = 1, b = 1, "cm")),
		text = element_text(family = "Berlin Sans FB",
			colour = "#BBA334", size = 15),
		plot.background = element_rect(fill = "#3F6961"),
		panel.grid = element_blank()
		) +
	guides(fill = guide_colourbar(barwidth = 20, barheight = 0.5,
		ticks.colour = "#3F6961")) +
	labs(title = "average number of players", subtitle = "(in thousands)",
		caption = "Data: Steam | Logo: Official Terraria Wiki | Visualisation: @bob_from_space"
		) +
	geom_curve(x = 2021, y = 7, xend = 2020, yend = 8, arrow = arrow(length = unit(0.2, "cm")),
		lineend = "round", size = 1, colour = "#454240") +
	annotate("text", x = 2021.9, y = 6.5, colour = "#BBA334",
		label = "\nRelease of\nthe Journey's End\nmajor update",
		size = 4.3, family = "Berlin Sans FB")

grid.raster(img, x = unit(0.5, "npc"), y = unit(0.93, "npc"),
	width = 0.3)
dev.off()