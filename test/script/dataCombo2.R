fingerprint <- function(z){
	z <- as.ballotTag(z)
	stopifnot(is.ballotTag(z))
	z.combo <- get.combo(z)
	z.ballot <- get.ballot(z)
	elemNames <- rownames(z.ballot)
	z.sbList <- splitBallot(z.ballot)
	z.sbList[[1]] <- sbAbstainers(z.sbList[[1]])
	names(z.sbList)[1] <- 'Abstainers'
	z.sbList <- z.sbList[sapply(z.sbList,sbSum)!=0]
	z.sbList <- head(z.sbList,n=3)
	system('mkdir -p test/pics/dc2')
	z.sbNames <- names(z.sbList)
	lapply(seq_along(z.sbList),
		function(i){
			party <- z.sbNames[i]
			z.chisq <- get.chisq(z,party)
			j <- head(order(z.chisq,decreasing=TRUE),n=z.combo$len%/%20)
			col <- sample_to_color(z.chisq[j])
			testFile <- paste(sep='',
				'test/pics/dc2/',z,'_',
				gsub(' ','.',z.sbNames[i]),'_density.png'
			)
			sb <- z.sbList[[i]]
			combo <- z.combo[j]
			dc <- ultraCombo::dataCombo(combo,sb,sbDensityGen(norm=FALSE))
			dList <- lapply(seq(combo$len),
				function(x){
					dc$dGen(x)
				}
			)
			arg <- densityArgList()
			arg$xlab <- 'Fractional Turnout'
			arg$xlim <- dListXlim(dList)
			arg$ylim <- dListYlim(dList)
			testPng(testFile)
			do.call(plot,arg)
			lapply(seq(dc$len),
				function(x){
					lines(dc$dGen(x),col=col[x])
				}
			)
			dev.off()
			if(buildPackageLoaded)gitAdd(print(testFile))
		}
	)
}

lapply(ls.ballotTag(),fingerprint)
