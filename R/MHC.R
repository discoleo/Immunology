

# nS = Start position of Epitope;
# nE = End position of Epitope;
# nPP   = (Start, End) in the original PP-sequence;
# nCore = (Start, End) of core-subsequence;
plot.epitopes = function(pp, nS, nE, nCore, nPP = NULL,
		opt, cex = 1, col = "#B088C2CC") {
	if(is.null(nPP)) nPP = range(c(nS, nE));
	# Core:
	nCoreA = nCore - nPP[1] + 1;
	ppCore = substr(pp, nCoreA[1], nCoreA[2]);
	# Plot:
	par.old = par(mar = c(1,1,1,1) + 0.1, cex = cex);
	on.exit(par(par.old));
	with(opt, {
	plot.new();
	plot.window(xlim = c(0, xL), ylim = c(0, xy[2] + 10), asp = 1);
	
	xE = xy[1] + dXE + nchar(ppCore) * xCh;
	rect(xy[1] - dX/2 - 0.25, y0 + 0.75 * dY - 0.25,
		xE - dX/2, y0 + 19 * dY, col = col[1], border = NA);
	
	for(id in seq_along(nS)) {
		preP  = substr(pp, nS[id] - nPP[1] + 1, nCoreA[1] - 1);
		postP = substr(pp, nCoreA[2] + 1, nE[id] - nPP[1] + 1);
		xE = xy[1] + dXE + nchar(ppCore) * xCh;
		y = y0 + dY * (19 - id);
		text(xy[1], y, ppCore, adj = c(0,0));
		text(xy[1] - dX, y, preP, adj = c(1,0));
		text(xE, y, postP, adj = c(0,0));
	}
	});
	invisible();
}


# 1. Lippolis JD, White FM, Marto JA, Luckey CJ, Bullock TN, Shabanowitz J, Hunt DF, Engelhard VH.
#    Analysis of MHC class II antigen processing by quantitation of peptides that constitute nested sets.
#    J Immunol. 2002 Nov 1;169(9):5089-97.
#    doi: 10.4049/jimmunol.169.9.5089. PMID: 12391225.


### Human IgG kappa (constant):
pp = "YPREAKVQWKVDNALQSGNSQES"
ppCore = "WKVDNALQS"
nCore = c(148, 156)
nPP = c(140, 162)

# Core:
nCoreA = nCore - nPP[1] + 1;
substr(pp, nCoreA[1], nCoreA[2])

### Epitope Nested Set
nS = c(145, 145, 140, 144, 140, 145, 141, 144, 145,
	143, 140, 143, 143, 144, 145, 144, 144, 145);
nE = c(159, 158, 158, 158, 159, 160, 158, 159, 161,
	158, 160, 159, 160, 161, 162, 162, 160, 157);

#
ppAll = sapply(seq_along(nS),
	\(id) substr(pp, nS[id] - nPP[1] + 1, nE[id] - nPP[1] + 1));

data.frame(ppAll)

### Graphic

opt = list(
	dY = 14, dX = 5,
	x0 = 100, y0 = 10);
opt = c(opt,
	list(xy = c(opt$x0, 18 * opt$dY + 10),
	#
	xCh = 10,
	dXE = 15, cex = 1.5,
	# dXE =  5, cex = 1.5, # for png
	# dXE = -20, cex = 1,
	xL = nchar(pp) * 13 + opt$y0)
)
cex = opt$cex;

# png(file = "MHC.2.Core.Nested.png")

plot.epitopes(pp, nS, nE, nCore, nPP, opt = opt, cex = cex)

dev.off()

