# ==========================================================================
# make a graph of wheighted average scores per season and the episodes
# ==========================================================================
library(dplyr)
library(ggplot2)
library(extrafont)

#
# Arrangements for the working space
# --------------------------------------------------------------------------

# needs to be ran just once
# font_install('fontcm')

# needs to be run once
# font_import()

path <- "tidytuesday/data/2020/2020-03-17"

office <- file.path(path, "office_ratings.csv") %>%
	read.csv2(sep = ",")
#
# Prepare the data
# --------------------------------------------------------------------------
offc_whghtd <- mutate(office,
	imdb_rating = as.numeric(imdb_rating))

wh_ave_season <- group_by(offc_whghtd, season) %>%
	summarise(mean_rating = weighted.mean(imdb_rating, total_votes, na.rm = TRUE)) %>%
	ungroup()

wh_best_ep <- group_by(offc_whghtd, season) %>%
	filter(imdb_rating == max(imdb_rating)) %>%
	slice(1L) %>%
	select(season, episode, title, imdb_rating) %>%
	transmute(ep_name = paste0("e", episode, ": ", title, " (", imdb_rating, ")")) %>%
	ungroup()

wh_all <- inner_join(wh_ave_season, wh_best_ep, by = "season") %>%
	arrange(desc(mean_rating)) %>%
	mutate(season = paste0("S", season),
		season = as.factor(season),
		season = reorder(season, mean_rating, function(x) sort(x, decreasing = TRUE)),
		# create the coordinates for text annotations
		x_ann = nrow(.):1
		)

#
# Visualise
# --------------------------------------------------------------------------

# palette: #91b6d4, #4e7ab2, #d6d8db, #29292a, #887b71

loadfonts(device="win")
windowsFonts()

ggplot(wh_all, aes(x = season, y = mean_rating,
	label = round(mean_rating, 2))) +
	geom_col(fill = "#4e7ab2") +
	coord_flip() +
	labs(title = "What season of the Office is the best?",
		subtitle = "Weighted average of episode ratings by votes on IMDB\n+ best episode in each season",
		x = "Season",
		caption = "Data: rfordatascience/tidytuesday (GitHub)\nVisualisation: @bob_from_space (Twitter)") +
	geom_text(nudge_y = 0.4, family = "Courier New") +
	annotate("text", x = wh_all$x_ann,
		# the line below is making me sad :C
		y = c(2, 1.7, 2.2, 2.45, 2.1, 1.65, 2.3, 1.9, 1.8),
		label = paste0("bold('", wh_all$ep_name, "')"),
		colour = "#29292a", family = "Courier New", parse = TRUE) +
	theme(
		axis.title.x = element_blank(),
		axis.text.x = element_blank(),
		axis.ticks.x = element_blank(),
		axis.title.y = element_blank(),
		axis.ticks.y = element_blank(),
		panel.background = element_rect(fill = "#d6d8db"),
		plot.background = element_rect(fill = "#d6d8db"),
		panel.grid = element_line(colour = "#d6d8db"),
		plot.title = element_text(face = "bold"),
		axis.text.y = element_text(hjust = -1,
			size = 11,
			family = "Courier New",
			face = "bold",
			margin = margin(2, -0.5, 2, 0.5, "cm")),
		text = element_text(family = "Courier New", colour = "#29292a")
		)

ggsave("./scripts/2020/2020-03-17/tidytuesday_vis.png")