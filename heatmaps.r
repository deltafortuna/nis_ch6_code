library(ggplot2)
library(cowplot)
library(reshape)

distributional_heatmap <- function(inputfile, stat, discrete = T, interval = 0.5) {
	d <- read.table(file = inputfile, header = T)
	dd <- split(d, d$stat)
	size <- dim(dd[[stat]])
	numcol = size[2] - 2
	mat <- dd[[stat]][,3:size[2]]
	numrow = numeric()
	high = numeric()
	low = numeric()
	brks = vector()
	if (discrete) {
		numrow <- max(mat) + 1
	}	else {
		for (i in 1:numcol) {
			minnie <- min(mat[,i], na.rm=T)
			maxie <- max(mat[,i], na.rm=T)
			if (i==1) {
				low <- minnie
				high <- maxie
			} else {
				if (minnie < low) {low <- minnie}
				if (maxie  > high) {high <- maxie}
			}
		}
		low = floor(low)
		high = ceiling(high)
		brks = seq(low, high, interval)
		numrow = length(brks) - 1
	}
	mat2 <- matrix(0, nrow = numrow, ncol = numcol)
	if (discrete) {
		for (i in 1:size[1]) {
			for (j in 1:numcol) {
					mat2[mat[i,j],j] <- mat2[mat[i,j],j] + 1
			}
		}
	} else {
		for (i in 1:numcol) {
			h = hist(mat[,i], breaks = brks, plot = F)
			mat2[,i] = h$counts
		}
	}
	mat2.melted <- melt(mat2)
	r = 1 # ratio for creating squares in heatmap
	if (!discrete) {
		for (i in 1:length(mat2.melted[,1])) {
			mat2.melted[i,1] = brks[mat2.melted[i,1]]
		}
		r = 1 / interval
	}
	ggplot(mat2.melted, aes(x = X2, y = X1, fill = value)) + geom_tile() + coord_equal(ratio = r) + scale_fill_gradient(low = "white", high = "black")
}

longitudinal_heatmap <- function(inputfile, stat="K", timeflow="down", scalelim=vector(), timelim=vector(), lowcol=1, fix=0) {
	d <- read.table(file = inputfile, header = T)
	dd <- split(d, d$stat)
	size <- dim(dd[[stat]])
	q <- dd[[stat]][,c(1,3:size[2])]
	m <- melt(q, id.vars=c("gen"))

	d<- ggplot(m, aes(x=variable, y=gen, fill=value)) + geom_tile()

	if (length(timelim) != 0) {
		if (timeflow == "down") {
			d <- d + ylim(timelim[2], timelim[1])
		} else {
			d <- d + ylim(timelim[1], timelim[2])
		}
	} else {
		if (timeflow == "down") {
			d <- d + ylim(max(m$gen), min(m$gen))
		}
	}

	if (length(scalelim) != 0) {
		if (lowcol) { # use white as low-value color
			d <- d + scale_fill_gradient(low="white", high="black", limits=scalelim)
		} else {
			d <- d + scale_fill_gradient(low="black", high="white", limits=scalelim)
		}
	} else {
		if (lowcol) { # use white as low-value color
			d <- d + scale_fill_gradient(low="white", high="black")
		} else {
			d <- d + scale_fill_gradient(low="black", high="white")
		}
	}

	if (fix != 0) {
		d <- d + geom_hline(yintercept=fix, linetype=2)
	}

	d <- d + geom_vline(xintercept = (size[2] - 2) / 2, linetype = 2)
	d
}
