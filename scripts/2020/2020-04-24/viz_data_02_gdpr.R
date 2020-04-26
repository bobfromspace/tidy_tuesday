# ==========================================================================
# Explore violations of GDPR
# ==========================================================================
library(data.table)
library(ggplot2)
library(extrafont)
# check the latest version if you need to load {dplyr} too
library(ggbump)

# I like these colors: #FFE694, #E0A860, #26263B, #D4D1C7

path <- "tidytuesday/data/2020/2020-04-21"

#
# Questions to explore:
#
# => 1) How big are the charges for the violations? Are there any country patterns?
# => 2) Did the charges increase with time (in countries where there are many cases)?
# => 3) Is there a difference between single and multiple charges and the sum of the charges?
# => 4) Most common articles that get violated?
df <- read.delim(file.path(path, "gdpr_violations.tsv"), header = TRUE, stringsAsFactors = FALSE)
setDT(df)

# 4) Articles 5 and 6, + 32, which is less so.
top_art <- df[, list(article = unlist(strsplit(article_violated, "[|]")))][
	, list(article = gsub("Art[.] |\\(\\d\\)| \\S\\)|GDPR|Art[.]|Art|GDRP|of the|,", "", article))][
	, list(article = trimws(article, "both"))][
	, list(article = fifelse(grepl("^\\d", article), article, NA_character_))][
	, list(n_cases = .N), by = article][
	order(n_cases, decreasing = TRUE), head(.SD, 5),][
	, article := reorder(paste("Art.", article), n_cases, median)][
	, p_cases := round(n_cases/ sum(n_cases), 2) * 100]

#
# Plot: Top violated articles
# --------------------------------------------------------------------------
ggplot(top_art, aes(article, p_cases)) +
	geom_point(colour = "#E0A860", size = 6, alpha = 0.9) +
	geom_segment(aes(x = article, xend = article, y = 0, yend = p_cases),
		colour = "#E0A860", size = 1.5, alpha = 0.7) +
	coord_flip() +
	labs(title = "\nMost violated GDPR articles", x = "",
		subtitle = "Percent of all charges",
		caption = "Data: rfordatascience/tidytuesday (GitHub)\nVisualisation: @bob_from_space (Twitter)") +
	geom_text(aes(x = article, y = p_cases + 2.5 , label = paste0(p_cases, "%")),
		colour = "#26263B", family = "Bahnschrift", size = 4) +
	geom_text(aes(x = article, y = sqrt(p_cases) + c(15.3, 16.8, 7.8, rep(0, 2)), 
		label = c("Principles relating to processing of personal data",
			"Lawfulness of processing",
			"Security of processing", rep("", 2))), vjust = -0.7, family = "Bahnschrift",
			size = 3.3) +
	theme(
		# legend
		legend.position = "none",
		# background
		panel.background = element_rect(fill = "#FFFFFF"),
		panel.grid.minor = element_line(colour = "#E0DDD3"),
		plot.background = element_rect(fill = "#FFFFFF"),
		# axis
		axis.title.x = element_blank(),
		axis.ticks = element_blank(),
		axis.text.x = element_blank(),
		# text
		plot.title.position = "plot",
		plot.title = element_text(face = "bold", size = 23, hjust = 0.05),
		plot.subtitle = element_text(hjust = 0.025),
		axis.text.y = element_text(colour = "#26263B", size = 12),
		text = element_text(colour = "#26263B", family = "Bahnschrift")
		)

ggsave(file.path(getwd(), paste0("gdpr_viz02_01.png")), width = 5.3, height = 7, unit = "in")

# 2) and 3) 
arts <- c(5, 6, 32)

price_df <- df[, paste0("prim_a", arts) := lapply(arts, function(x) {grepl(paste0(" ", x, " "), article_violated)})][
	, paste0("prim_a", arts) := lapply(arts, function(x) {fifelse(grepl("R[|]A", article_violated), FALSE, get(paste0("prim_a", x)))})][
	, c("prim_article", "price_cat") := list(fifelse((prim_a5 + prim_a6 + prim_a32) == 1, "cat", "Multiple"),
		cut(price, c(-1, 2500, 10000, 50000, 200000, max(price)), labels = c("0-2.5K", "2.5-10K", "10-50K", "50-200K", "200K+")))][
	grepl(" 5 | 6 | 32 ", article_violated),][
	, prim_article := fifelse(prim_article == "cat" & prim_a5, "Art. 5", fifelse(prim_article == "cat" & prim_a6, "Art. 6", fifelse(prim_article == "cat" & prim_a32, "Art. 32", prim_article)))][
	, .N, by = c("price_cat", "prim_article")]

all_cats <- with(price_df, expand.grid(price_cat = unique(price_cat), prim_article = unique(prim_article)))
setDT(all_cats)[, prim_article := as.character(prim_article)]

price_df <- price_df[all_cats, on = c("price_cat", "prim_article")][
	order(N, decreasing = TRUE), art_order := .N:1 , by = price_cat][
	, prim_article := factor(prim_article, levels = levels(factor(prim_article))[c(2, 3, 1, 4)])][
	, p_cases := round(100 *(N/sum(N, na.rm = TRUE)), 2)][
	, art_order := fifelse(is.na(N), NA_integer_, art_order)]

#
# Plot: Fine amount for most common articles
# --------------------------------------------------------------------------
ggplot(price_df, aes(price_cat, art_order, group = prim_article)) +
	geom_bump(aes(colour = prim_article), size = 1.5, alpha = 0.7) +
	geom_point(aes(size = p_cases, colour = prim_article), shape = 19, alpha = 0.9) +
	geom_text(aes(price_cat, art_order + 0.2, label = prim_article),
		family = "Bahnschrift", colour = "#26263B") +
	labs(title = "\nFine range for top 3 GDPR charges",
		subtitle = "Amounts in thousand Euros by several or single article charges where Art. 5, 6, or 32 are specified\n",
		caption = "Data: rfordatascience/tidytuesday (GitHub)\nVisualisation: @bob_from_space (Twitter)",
		size = "Percent of cases:") +
	scale_x_discrete(position = "top") +
	scale_colour_manual(values = c("#61492A", "#E0A960", "#E6CBA8", "#E07C55")) +
	guides(colour = FALSE,
		size = guide_legend(override.aes = list(colour = "#26263B"))) +
	scale_size_continuous(breaks = c(3, 6, 9), labels = paste0(c(3, 6, 9), "%")) +
	theme(
		# legend
		legend.direction = "horizontal",
		legend.position = c(0.2, -0.02),
		legend.key = element_rect(fill = "#FFFFFF"),
		# text
		plot.title.position = "panel",
		plot.title = element_text(face = "bold", size = 23),
		text = element_text(family = "Bahnschrift", colour = "#26263B"),
		axis.text.y = element_blank(),
		axis.ticks = element_blank(),
		axis.title = element_blank(),
		# panel
		panel.background = element_rect(fill = "#FFFFFF"),
		panel.grid.minor = element_line(colour = "#FFFFFF"),
		plot.background = element_rect(fill = "#FFFFFF")
		)

ggsave(file.path(getwd(), paste0("gdpr_viz02_02.png")), width = 10, height = 7, unit = "in")