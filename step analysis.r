library(ggplot2)
library(scales)
library(gdata)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(gridExtra)
library(digest)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_latest_step_data()


# TODO: Hist of gap, hist of growth, ggs mid year with 1 STEP per wave expected

cut.ggs <- function(vec, expected.steps=3){
	cut(vec, c(-999, expected.steps, (expected.steps+1), 999),
													labels=c("opened", "none", "closed"), right=FALSE
													)
}

df$ggs <- cut.ggs(df$growth)

ggplot(data=df, aes(x=w5.gap))+
	geom_bar(aes(y = (..count..)/sum(..count..)), colour="black", binwidth=1)+
	scale_y_continuous(labels=percent, limits=c(0,.5), breaks=seq(0,.5,.05))+
	scale_x_continuous(breaks=seq(-10,10,1), limits=c(-10, 10))+
	theme_bw()+
	labs(title="")

df.s <- subset(df, df$grade != -1)

# Growth distributions plot and loop for faceted
growth.hist <- function(vec, title, y.max){
	df.t <- data.frame(prop.table(table(vec)))
	colnames(df.t) <- c("growth", "perc")
	df.t$growth <- as.numeric(as.character(df.t$growth))
	p <- ggplot(data=df.t, aes(x=growth, y=perc))+
		geom_bar(colour="black", stat="identity")+
		scale_y_continuous(labels=percent, limits=c(0,y.max), breaks=seq(0,y.max,.1))+
		scale_x_continuous(breaks=seq(-1,10,1), limits=c(-1, 10))+
		theme_bw()+
		labs(title=title)+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					axis.text.x=element_text(size=7),
					axis.text.y=element_text(size=7),
					title=element_text(size=9)
					)
	return(p)
}
schools <- c("RCAA", "STA", "DTA", "SCH")
grades <- 0:2
plots <- sapply(levels(interaction(grades, schools)), function(x){NULL})
for(s in schools){	
	df.s <- subset(df, school==s & !is.na(w12.growth))
	for(g in grades){
		df.g <- subset(df.s, grade==g)
		p <- growth.hist(df.g$w12.growth, paste(s, g, sep=" "), .75)
		plots[[paste(g, s, sep=".")]] <- p
	}
}
do.call(grid.arrange, c(plots, main="\nSTEP Growth From Wave 1 to Wave 2",
												left="\nPercent of Students",
												sub="STEP Growth From Wave 1 to 2 (≥1 is Expected)\n",
												ncol=3)
)

# STEP Growth by teacher (need to change school manually)
df.s <- drop.levels(subset(df, school=="SCH"))
teachers <- unique(levels(df.s$home.room))
plots <- sapply(teachers, function(x){NULL})
for(t in teachers){	
	df.t <- subset(df.s, home.room==t & !is.na(w12.growth))
	p <- tryCatch({
		growth.hist(df.t$w12.growth, t, 1)
	}, error=function(e){
		return(NULL)
	})
	plots[[t]] <- p
}
do.call(grid.arrange, c(plots, main="\nSCH STEP Growth From Wave 1 to Wave 2",
												left="\nPercent of Students",
												sub="STEP Growth From Wave 1 to 2 (≥1 is Expected)\n",
												ncol=4)
)

df$w12.growth.status <- apply(df, 1, function(r){
	status <- as.character(cut.ggs(as.numeric(r[['w12.growth']]),1))
	tryCatch({
		if(status == 'opened'){
			if(as.numeric(r[['wave.2.gap']]) >= 0){
					return('none')
			}else{
				return(status)
			}
		}else{
			return(status)
		}
		}, error=function(e){
			return(NA)
		}
	)
})

# ggs distributions plot and loop for faceted
ggs.hist <- function(vec, title, y.max){
	df.t <- data.frame(prop.table(table(vec)))
	colnames(df.t) <- c("growth", "perc")
	p <- ggplot(data=df.t, aes(x=growth, y=perc, fill=growth))+
		geom_bar(stat="identity")+
		scale_y_continuous(labels=percent, limits=c(0,y.max), breaks=seq(0,y.max,.1))+
		scale_x_discrete(breaks=c("opened", "none", "closed"))+
		scale_fill_manual(values=c("#198D33", "#E5E167", "#D16262"), guide=F)+
		theme_bw()+
		labs(title=title)+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					axis.text.x=element_text(size=7),
					axis.text.y=element_text(size=7),
					title=element_text(size=9)
					)
	return(p)
}
# By school
schools <- c("RCAA", "STA", "DTA", "SCH")
grades <- 0:2
plots <- sapply(levels(interaction(grades, schools)), function(x){NULL})
for(s in schools){	
	df.s <- subset(df, school==s)
	for(g in grades){
		df.g <- subset(df.s, grade==g & !is.na(w12.growth.status))
		p <- ggs.hist(df.g$w12.growth.status, paste(s, g, sep=" "), 1)
		plots[[paste(g, s, sep=".")]] <- p
	}
}
do.call(grid.arrange, c(plots, main="\nSTEP Gap Growth Status, Wave 1 to 2",
												left="\nPercent of Students",
												sub="STEP Gap Growth Status\n",
												ncol=3)
)
# By teacher (need to change school manually)
df.s <- drop.levels(subset(df, school=="RCAA" & !is.na(w12.growth.status) & grade %in% 0:2))
teachers <- unique(levels(factor(df.s$home.room)))
plots <- sapply(teachers, function(x){NULL})
for(t in teachers){	
	df.t <- subset(df.s, home.room==t)
	p <- tryCatch({
		ggs.hist(df.t$w12.growth.status, t, 1)
	}, error=function(e){
		return(NULL)
	})
	plots[[t]] <- p
}
do.call(grid.arrange, c(plots, main="\nSTEP Gap Growth Status, Wave 1 to 2, RCAA",
												left="\nPercent of Students",
												sub="STEP Gap Growth Status\n",
												ncol=4)
)

# Histograms of Gap to Grade Level
gap.hist <- function(vec, title, y.max){
	df.t <- data.frame(prop.table(table(vec)))
	colnames(df.t) <- c("gap", "perc")
	df.t$gap <- as.numeric(as.character(df.t$gap))
	p <- ggplot(data=df.t, aes(x=gap, y=perc))+
		geom_bar(colour="black", stat="identity")+
		scale_y_continuous(labels=percent, limits=c(0,y.max), breaks=seq(0,y.max,.1))+
		scale_x_continuous(breaks=seq(-8,8,1), limits=c(-8, 8))+
		theme_bw()+
		labs(title=title)+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					axis.text.x=element_text(size=7),
					axis.text.y=element_text(size=7),
					title=element_text(size=9)
					)
	return(p)
}

schools <- c("RCAA", "STA", "DTA", "SCH")
grades <- -1:2
plots <- sapply(levels(interaction(grades, schools)), function(x){NULL})
for(s in schools){	
	df.s <- subset(df, school==s)
	for(g in grades){
		df.g <- subset(df.s, grade==g)
		p <- tryCatch({
			gap.hist(df.g$latest.gap, paste(s, g, sep=" "), 1)
			}, error=function(e){
				return(arrangeGrob(textGrob("missing data")))
			}
		)
		plots[[paste(g, s, sep=".")]] <- p
	}
}
do.call(grid.arrange, c(plots, main="\nSTEP Gap to Grade Level, Wave 2",
												left="\nPercent of Students",
												sub="Gap to Grade Level, in STEPs\n",
												ncol=4)
)

# Need to change schools manually
df.s <- drop.levels(subset(df, school=="SCH"))
teachers <- unique(levels(df.s$home.room))
plots <- sapply(teachers, function(x){NULL})
for(t in teachers){	
	df.t <- subset(df.s, home.room==t)
	p <- tryCatch({
		gap.hist(df.t$latest.gap, t, 1)
	}, error=function(e){
		return(NULL)
	})
	plots[[t]] <- p
}
do.call(grid.arrange, c(plots, main="\nSTEP Gap to Grade Level, Wave 2, SCH",
												left="\nPercent of Students",
												sub="Gap to Grade Level, in STEPs\n",
												ncol=4)
)


# Gap breakdown by wave...
df$growth1.2 <- apply(df, 1, function(r){
	as.numeric(r['w2']) - as.numeric(r["w1"])
})
df$growth2.3 <- apply(df, 1, function(r){
	as.numeric(r['w3']) - as.numeric(r['w2'])
})
df$growth3.4 <- apply(df, 1, function(r){
	as.numeric(r['w4']) - as.numeric(r['w3'])
})
df$growth4.5 <- apply(df, 1, function(r){
	as.numeric(r['w5']) - as.numeric(r['w4'])
})
growth.cut <- function(vec){
	cut(vec, c(-99, 0, 1, 2, 3, 4, 99),
						labels=c("<0", "0", "1", "2", "3", "≥4"), right=FALSE
						)
}
df$growth1.2.cut <- growth.cut(df$growth1.2)
df$growth2.3.cut <- growth.cut(df$growth2.3)
df$growth3.4.cut <- growth.cut(df$growth3.4)
df$growth4.5.cut <- growth.cut(df$growth4.5)

growth.by.wave.plot <- function(d, title){
	df.m <- melt(d, id=c("id", "school", "grade", "home.room"),
								measure.vars=c("growth1.2", "growth2.3", "growth3.4",
									"growth4.5"
								),
								variable.name="wave", value.name="growth"
	)

	props <- prop.table(table(df.m$wave, df.m$growth), 1)
	props.m <- melt(props)
	names(props.m) <- c("period", "growth", "perc")
	ggplot(props.m, aes(x=growth, y=perc))+
		geom_bar(stat="identity")+
		scale_x_continuous(breaks=seq(-3,4,1))+
		scale_y_continuous(labels=percent, breaks=seq(0,1,.1))+
		labs(x="Growth in STEPs",
				y="Percent of Students",
				title=title
		)+
		theme_bw()+
		theme(axis.title.x=element_blank(),
					axis.title.y=element_blank(),
					axis.text.x=element_text(size=7),
					axis.text.y=element_text(size=7),
					title=element_text(size=9)
					)+
		facet_wrap(~period, nrow=1)
}

# ...by school-grade
schools <- c("RCAA", "STA", "DTA")
grades <- -1:2
plots <- sapply(levels(interaction(grades, schools)), function(x){NULL})
for(s in schools){	
	df.s <- subset(df, school==s)
	for(g in grades){
		df.g <- subset(df.s, grade==g)
		p <- growth.by.wave.plot(df.g, paste(s, g, sep=" "))
		plots[[paste(g, s, sep=".")]] <- p
	}
}
do.call(grid.arrange, c(plots, main="\nSTEP Growth by Wave",
												left="\nPercent of Students",
												sub="Growth from Wave to Wave in STEPs\n",
												ncol=4)
)

# Does being wth us matter?
df$gap.1 <- apply(df, 1, function(r){
	if(!is.na(r[5])){
		if(as.numeric(r[5])==0){
			target <- 0
		}else if(as.numeric(r[5])==1){
			target <- 3
		}else if(as.numeric(r[5])==2){
			target <- 6
		}else{
			target <- NA
		}
		return(as.numeric(r[6]) - target)
	}else{
		return(NA)
	}
})
no.na.length <- function(vec){
	vec <- vec[!is.na(vec)]
	return(length(vec))
}
step.comparison <- function(df){
	y <- rep(NA,6)
	vet <- subset(df, y11==1)
	new <- df[is.na(df$y11),]
	y[1] <- nrow(vet)
	y[2] <- nrow(new)
	t.gap1 <- tryCatch({
		t.test(vet$gap.1, new$gap.1)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.gap1)){
		y[3] <- t.gap1$estimate[1]
		y[4] <- t.gap1$estimate[2]
		y[5] <- t.gap1$conf.int[1]
		y[6] <- t.gap1$conf.int[2]
		y[7] <- t.gap1$p.value	
	}
	t.gap4 <- tryCatch({
		t.test(vet$gap.4, new$gap.4)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.gap1)){
		y[8] <- t.gap4$estimate[1]
		y[9] <- t.gap4$estimate[2]
		y[10] <- t.gap4$conf.int[1]
		y[11] <- t.gap4$conf.int[2]
		y[12] <- t.gap4$p.value	
	}
	t.growth <- tryCatch({
		t.test(vet$growth, new$growth)
		}, error=function(err){
			return(NA)
		}
	)
	if(!is.na(t.growth)){
		y[13] <- t.growth$estimate[1]
		y[14] <- t.growth$estimate[2]
		y[15] <- t.growth$conf.int[1]
		y[16] <- t.growth$conf.int[2]
		y[17] <- t.growth$p.value	
	}
	names(y) <- c("n.vet", "n.new",
								"gap.1.vet.mean", "gap.1.new.mean",
								"gap.1.dif.low", "gap.1.dif.high",
								"gap.1.dif.p",
								"gap.4.vet.mean", "gap.4.new.mean",
								"gap.4.dif.low", "gap.4.dif.high",
								"gap.4.dif.p",
								"growth.vet.mean", "growth.new.mean",
								"growth.dif.low", "growth.dif.high",
								"growth.dif.p"
								)
	return(y)
}

df.s <- subset(df, grade %in% 0:2)
df.sum <- ddply(df.s, .(school, grade), step.comparison)
write.csv(df.sum, "step_summary.csv")
df.sum <- ddply(df.s, .(grade), step.comparison)
write.csv(df.sum, "step_summary_grade.csv")
df.sum <- ddply(df.s, .(school), step.comparison)
write.csv(df.sum, "step_summary_school.csv")

# ANOVA of STEP
df$y11.status <- apply(df, 1, function(r){
	if(is.na(r['y11'])){
		return("rookie")
	}else{
		return("vet")
	}
})
df$y11.status <- factor(df$y11.status)
df.s <- subset(df, grade %in% 0:2)
aov(df.s$growth ~ df.s$y11.status)
lm1 <- lm(df.s$growth ~ df.s$y11.status)
lm2 <- lm(df.s$growth ~ df.s$school * df.s$y11.status)



# STEP vs MAP
df.map <- read.csv(file="map data fall.csv", head=TRUE, na.string=c("", " ", "  "))
d.map <- subset(df.map, subject == "reading")
d.map <- d.map[, c("id", "fall.percentile")]
d.step <- df[, c("id", "home.room", "school", "grade", "first", "first.gap")]
d.sm <- merge(d.map, d.step)
d.sm <- subset(d.sm, grade != -1)
ggplot(d.sm, aes(x=first.gap, y=fall.percentile))+
	geom_point(alpha=.3)+
	geom_smooth()+
	scale_y_continuous(limits=c(0, 100))+
	labs(x="First Test (usually EOY previous year) Gap from Grade Level in STEPs",
			y="MAP Reading Fall National Percentile",
			title="MAP Scores by STEP Gap"
	)+
	theme_bw()+
	facet_grid(school ~ grade)
	
# STEP vs LEAP at DTA 4th Grade
al.order <- c("U", "AB", "B", "M", "A")
df <- read.csv(file="step ileap data.csv", head=TRUE, na.string=c("", " ", "  "))
df$ela <- reorder(df$ela, new.order=al.order)
df$math <- reorder(df$math, new.order=al.order)
ggplot(df, aes(x=ela, y=step))+
	scale_y_continuous(breaks=seq(2, 12, 1))+
	geom_boxplot(notch=T)+
	geom_point(alpha=.3, position=position_jitter(width=.25))

ggplot(df, aes(x=step, y=ela))+
	scale_x_continuous(breaks=seq(2, 12, 1))+
	geom_boxplot(notch=T)+
	geom_point(alpha=.3, position=position_jitter(height=.25))

# Make a graph of percent of students at or above each STEP that scored B or above on iLEAP
steps <- unique(df$step)
find.percent.basic <- function(step.cut, data){
	mean(as.numeric(data[data$step >= step.cut, ]$ela) >= 3)
}
d <- data.frame(step=steps, perc.prof=sapply(steps, find.percent.basic, df))
ggplot(d, aes(x=step, y=perc.prof))+
	scale_x_continuous(breaks=seq(2, 12, 1))+
	scale_y_continuous(labels=percent)+
	geom_point()+
	labs(title="2013 3rd ELA iLEAP Scores by 2014 BOY STEP Level at DTA",
				x="STEP",
				y="Percent of Students at or Above that STEP Level Scoring Basic or Above"
	)+
	theme_bw()


