#!/usr/bin/RScript
require("ggplot2")
require("reshape2")
rm(list=ls())

df <- read.csv("_r_Firefox Census (Responses) - Form Responses 1.csv",
	header=TRUE)

# You may need to relevel some factors as they are somewhat arbitrarily imported
levels1 <- c(
"",
"I'm not sure what this is OR I don't have an opinion",
"I see nothing positive - I hate it!",
"Some positives, but it's mostly negative",
"Some negatives, but it's mostly positive",
"I see nothing negative - I love it!"
)
levels2 <- c(
"",
"I'm not sure",
"Awful",
"Bad",
"Neutral, neither good nor bad.",
"Good",
"Great"
)
levels3 <- c(
"",
"Dark",
"Light"
)
levels4 <- c(
"",
"No, never",
"No, but do warn them",
"Warn poster, temp-ban after repeated warnings, never permaban",
"Warn poster, ban after repeated warnings",
"Temp-ban on any offense",
"Permaban on any offense"
)
df[[23]] <- factor(df[[23]], levels=levels1)
df[[24]] <- factor(df[[24]], levels=levels1)
df[[14]] <- factor(df[[14]], levels=levels2)
df[[51]] <- factor(df[[51]], levels=levels3)
df[[37]] <- factor(df[[37]], levels=levels4)
df[[20]] <- factor(df[[20]], levels=levels1)
df[[53]] <- factor(df[[53]], levels=levels1)
ff_xtab <- function(df, cols, names, rel=0) {
	col1 <- colnames(df)[(cols[1])]
	col2 <- colnames(df)[(cols[2])]
	formula <- as.formula(paste("~", col1, "+", col2))
	xtab <- xtabs(formula, data=df)
	xtab.rel <- xtab
	plot.title <- "Number of responses"
	if (rel != 0) {
		xtab.rel <- round(prop.table(xtab, rel), digits=3)
		plot.title <- paste("Relative responses to",
			names[rel], sep=" ")
	}
	melted <- melt(xtab.rel)
	plot <- ggplot(melted, aes_string(col1, col2)) +
		geom_tile(aes(fill = value)) +
		geom_text(aes(label = value), color="white") +
		labs(x=names[1], y=names[2], fill="responses", title="Cross Tabs",
			subtitle=plot.title) +
		theme(axis.text.x = element_text(angle=45, hjust=1))
	png(filename=paste("fig/", gsub(" ", "", tolower(paste(names[1],
		names[2], rel, sep="_"))), ".png", sep=""), width=720, height=720)
	print(plot)
	dev.off()
	return(plot)
}

# ff_xtab {}
# 
# Description
# 	ff_xtab returns a ggplot of crosstabs between selected variables
# Arguments
# 	df
# 		data table with imported responses
# 	cols
# 		indices of two columns selected for crosstabbing
# 	names
# 		names to replace default imported column names
# 	rel
# 		0 if the xtab should return absolute number of responses
# 		1 if the xtab should return relative number of responses with respect
# 			to column sums
# 		2 if the xtab should return relative number of responses with respect
# 			to row sums
# Details
# 	The ff_xtab function also prints a png file to a `fig/` subdirectory
# Examples

plot1 <- ff_xtab(df=df, cols=c(3, 8),
	names=c("Desktop OS", "Mobile OS"), rel=0)
plot2 <- ff_xtab(df=df, cols=c(3, 8),
	names=c("Desktop OS", "Mobile OS"), rel=1)
plot3 <- ff_xtab(df=df, cols=c(3, 8),
	names=c("Desktop OS", "Mobile OS"), rel=2)
plot4 <- ff_xtab(df=df, cols=c(11, 8),
	names=c("Linux distro", "Mobile OS"), rel=0)
plot5 <- ff_xtab(df=df, cols=c(3, 14),
	names=c("Desktop OS", "Direction Mozilla is heading"), rel=1)
plot6 <- ff_xtab(df=df, cols=c(23, 24),
	names=c("Firefox for iOS", "Firefox for Android"), rel=0)
plot7 <- ff_xtab(df=df, cols=c(24, 8),
	names=c("Firefox for Android", "Mobile OS"), rel=2)
plot8 <- ff_xtab(df=df, cols=c(23, 8),
	names=c("Firefox for iOS", "Mobile OS"), rel=2)
plot9 <- ff_xtab(df=df, cols=c(37, 51),
	names=c("Color theme", "Mods should ban trolls"), rel=2)
plot10 <- ff_xtab(df=df, cols=c(20, 53),
	names=c("e10s", "New Mozilla logo"), rel=0)
