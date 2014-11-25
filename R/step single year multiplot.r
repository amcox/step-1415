library(reshape2)
library(plyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(gdata)

update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

df <- load_data_with_gaps_long()
df <- subset(df, school %in% schools & grade < 3)
df.goals <- load_step_goals_to_wave_and_year_just_ends(w=4, y=c(2015))

p.avg.step <- make_plot_average_step(df, df.goals, title="Average STEP")+
  theme(plot.title=element_text(size=7),
    axis.title.y=element_text(size=7)
  )
p.avg.gap <- make_plot_average_gap(df, title="Average Gap")+
  theme(plot.title=element_text(size=7),
    axis.title.y=element_text(size=7)
  )
p.percent <- make_plot_percent_of_students_on_level(df, "Percent On-Level")+
  theme(plot.title=element_text(size=7),
    axis.title.y=element_text(size=7)
  )

tmp <- ggplot_gtable(ggplot_build(p.percent))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend.grob <- tmp$grobs[[leg]]
# Manually save plot because the stretch is weird
grid.arrange(p.avg.step + theme(legend.position="none"),
						p.avg.gap + theme(legend.position="none"),
						p.percent + theme(legend.position="none"),
						legend.grob,
						ncol=4, widths=c(.3, .3, .3, .1),
						main="\nSTEP Results by Grade"
)