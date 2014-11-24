cut_ggs <- function(vec, expected.steps=3){
	cut(vec, c(-999, expected.steps, (expected.steps+1), 999),
  	labels=c("opened", "none", "closed"), right=FALSE
	)
}