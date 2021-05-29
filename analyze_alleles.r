library(ggplot2);
library(reshape2);
library(cowplot);

analyze_allele_origins <- function(timepoints, focaldeme, origindemes, imagefile="allele_origins.pdf")
{
	q = matrix(nrow=length(timepoints), ncol = length(origindemes)+1); # 1 extra for generation
	for (i in 1:length(timepoints)) {
		t <- read.table (file = paste("deme", focaldeme, "_", timepoints[i], sep = ""), header = T);
		q[i,1] = timepoints[i];
		for (j in 1:length(origindemes)) {
			q[i,1+j] = sum(t[1,] == origindemes[j]);
		}
	}
	n <- c("gen");
	for (i in 1:length(origindemes)) {
		n <- c(n, paste("d",origindemes[i],sep=""));
	}
	q <- as.data.frame(q);
	names(q) <- n;
	mq <- melt(q, id.vars="gen");
	gg <- ggplot(mq, aes(gen)) + geom_point(aes(y=value, colour=variable));
	gg <- gg + geom_line(aes(y=value, colour=variable));
	#gg + scale_colour_manual(values = c("snow2", "snow4", "black"));
	gg;
	ggsave(imagefile);
	return(q);
}
