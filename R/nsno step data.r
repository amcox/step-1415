update_functions <- function() {
	old.wd <- getwd()
	setwd("functions")
	sapply(list.files(), source)
	setwd(old.wd)
}
update_functions()

library(digest)

df <- load_latest_step_data()

# # NSNO STEP Reporting for the first round
# d <- df[,c(1, 5:25)]
# d$hash.id <- apply(d, 1, function(r){
#   digest(as.numeric(r[['id']]), algo="sha256")
# })
# d <- d[,c(2:23)]
# write.csv(d, "ReNEW STEP data for NSNO.csv", na="", row.names=F)

d <- df[, names(df) %in% c("id", "w1", "w2", "w3", "w4", "school", "grade")]
d$hash.id <- apply(d, 1, function(r){
  digest(as.numeric(r[['id']]), algo="sha256")
})
d <- d[, !names(d) %in% c("id")]

write.csv(d, "ReNEW STEP data for NSNO, Round 1.csv", na="", row.names=F)