#!/usr/bin/RScript
require("ggplot2")
require("reshape2")
rm(list=ls())

df <- read.csv("_r_Firefox Census (Responses) - Form Responses 1.csv",
	header=TRUE)
ff_xtab <- function(df, cols, names) {
	col1 <- colnames(df)[(cols[1])]
	col2 <- colnames(df)[(cols[2])]
	formula <- as.formula(paste("~", col1, "+", col2))
	xtab <- xtabs(formula, data=df)
	melted <- melt(xtab)
	plot <- ggplot(melted, aes_string(col1, col2)) +
		geom_tile(aes(fill = value)) +
		geom_text(aes(label = value), color="white") +
		labs(x=names[1], y=names[2], fill="responses")
	return(xtab)
}

ff_relxtab <- function(df, cols, names) {
	col1 <- colnames(df)[(cols[1])]
	col2 <- colnames(df)[(cols[2])]
	formula <- as.formula(paste("~", col1, "+", col2))
	xtab <- xtabs(formula, data=df)
	margin <- margin.table(xtab, 1)
	relxtab <- 
	melted <- melt(relxtab)
	plot <- ggplot(melted, aes_string(col1, col2)) +
		geom_tile(aes(fill = value)) +
		geom_text(aes(label = value), color="white") +
		labs(x=names[1], y=names[2], fill="responses")
	return(xtab)
}

plot1 <- ff_xtab(df=df, cols=c(3, 8), c("Desktop OS", "Mobile OS"))
plot2 <- ff_xtab(df=df, cols=c(3, 4), c("Desktop OS", "Firefox branch"))
