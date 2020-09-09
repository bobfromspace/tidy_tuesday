# ==========================================================================
# Recreating Ceros awesome doughnut & bar charts
# https://www.ceros.com/originals/friends-scripts-25th-anniversary-catchphrase-scenes-quotes/
# ==========================================================================

library(data.table)
library(ggplot2)
library(ggimage)
library(ggforce)
library(extrafont)
library(patchwork)

dt <- fread("./tidytuesday/data/2020/2020-09-08/friends.csv")
setDT(dt)

background <- "#FFFAF4"
title <- "#0015FF"

char_colours <- data.table(
	speaker = c("Rachel Green", "Monica Geller", "Chandler Bing",
		"Joey Tribbiani", "Phoebe Buffay", "Ross Geller"),
	numb = c(1, 2, 3, 4, 5, 6),
	colour = c("#FF797A", "#6BC9FF", "#4E0FCA", "#FFA040", "#7FC061",
		"#000000"),
	# after an hour of search in the source, I (luckily) succeeded in finding these pics
	image_links = c("https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/823e97e11be833cb07bb356f5f1ec88a/rachel-copy.png",
		"https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/453d6c2129da2f9b2ce271f6fe91a812/monica-copy.png",
		"https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/54007d55df82f058f3b3632191e272e9/chandler-copy.png",
		"https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/faf03d07c388d9b81cd78e7fd5f2b943/joey-copy.png",
		"https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/c2df59bbe16d9ff188a9569ff2a44ab2/phoebe-copy.png",
		"https://media-s3-us-east-1.ceros.com/editorial-content/images/2019/08/06/c8844c2c04b63d7c14f0d17d54b9625d/ross-copy.png")
	)
char_colours <- char_colours[, tidyr::separate(.SD, speaker, c("character", "second_name"), sep = " ", remove = FALSE)]

divide <- function(x) {
	split <- strsplit(x, " ")
	sapply(split, `[[`, 1)
}

#
# Process the data
# --------------------------------------------------------------------------
prep_dt <- dt[,previous_speaker := dplyr::lag(speaker), by = c("season", "episode", "scene")][
	speaker %chin% char_colours$speaker & previous_speaker %chin% char_colours$speaker,][,
	speaker := divide(speaker)][, previous_speaker := divide(previous_speaker)][
	speaker != previous_speaker,][,
	pair := {
		sapply(1:length(speaker), function(cnt) Reduce(function(x, y) {paste(x, y, sep = " & ")}, sort(c(speaker[cnt], previous_speaker[cnt]))))
		}, by = c("season", "episode", "scene")][,.N,by = "pair"][
		order(-N), ]

dts <- lapply(char_colours$character, function(name) {
	don_dt <- prep_dt[grep(name, pair),][,
			partner := gsub(paste0(c(name, " ", "&"), collapse = "|"), "", pair)][
			order(-N), perc := round(N / sum(N), 2)][, character := name]
	don_dt <- char_colours[character == name, list(character, numb, image_links)][
	don_dt, on = "character"][,
		ymax := cumsum(perc)][, ymin := c(0, head(ymax, n = -1))]
	don_dt <- don_dt[char_colours[, list(character, colour)], on = c(partner = "character"), nomatch = 0]
	stopifnot(nrow(don_dt) == 5)
	return(don_dt)
	})

dts <- rbindlist(dts, fill = TRUE)
dts <- dts[, character := as.factor(character)][,
	character := factor(character,
		levels = char_colours$character)]

#
# Doughnut chart(s)
# --------------------------------------------------------------------------
donut <- ggplot(dts, aes(ymin = ymin, ymax = ymax, xmax = 4, xmin = 3)) +
	geom_rect(fill = dts$colour) +
	coord_polar(theta = "y") +
	xlim(c(-2, 4)) +
	geom_image(aes(x = -2, y = 0,
		image = image_links), size = 0.69) +
	facet_wrap(.~ character, ncol = 2) +
	theme(
		plot.background = element_rect(fill = background),
		panel.background = element_rect(fill = background),
		axis.text = element_blank(),
		axis.title = element_blank(),
		axis.ticks = element_blank(),
		panel.grid = element_line(colour = background),
		strip.background = element_blank(),
		strip.text = element_blank()
			)

ggsave("./viz/friends_viz03_01.png", plot = donut, width = 7, height = 10.5)

#
# Bar chart
# --------------------------------------------------------------------------
bar_prep <- copy(dts)
bar_prep <- prep_dt[, tidyr::separate(.SD, pair, c("character", "partner"), sep = " & ", remove = FALSE)]
# dt for point beginnings
col_dt <- char_colours[, list(character, colour)][
	bar_prep, on = "character"][, N := 0][, partner := NULL]
# dt for point ends
col_dt2 <- char_colours[, list(character, colour)][
	bar_prep, on = c(character = "partner")][, i.character := NULL]
# merge everything together
link_dt <- rbind(col_dt, col_dt2)
link_dt <- link_dt[,pair := reorder(as.factor(pair), N, max)][]
# create named vector for manual fill scale
link_cols <- with(char_colours, setNames(colour, character))

barplot <- ggplot(link_dt, aes(x = N, y = pair, group = pair, colour = character)) +
	geom_link2(size = 8, lineend = "round", n = 500) +
	scale_colour_manual(values = link_cols) +
	geom_text(data = col_dt2, aes(x = N + 300, y = pair, label = N),
		hjust = 0, colour = "black", size = 3.5, family = "Arial",
		fontface = "bold") +
	xlim(c(0, max(link_dt$N) + 400)) +
	ggtitle("Number of lines spoken between the characters:") +
	theme(
		plot.background = element_rect(fill = background),
		panel.background = element_rect(fill = background),
		axis.text.x = element_blank(),
		axis.text.y = element_text(colour = "black", family = "Arial",
			face = "bold", size = 10),
		axis.title = element_blank(),
		axis.ticks = element_blank(),
		panel.grid = element_line(colour = background),
		strip.background = element_blank(),
		strip.text = element_blank(),
		legend.position = "none",
		# hjust isn't stable, so to say - it depends on the width of the saved pic
		plot.title = element_text(hjust = -1, colour = title,
			family = "Arial", face = "bold")
			)

ggsave("./viz/friends_viz03_02.png", plot = barplot, width = 7)

out_plots <- donut + barplot

ggsave("./viz/friends_viz03_03.png", plot = out_plots, width = 12.5)