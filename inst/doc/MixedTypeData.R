## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=F, message=F----------------------------------------------------
library(BCClong)
library(mixAK)
data(PBC910)

## ---- warning=F, message=F----------------------------------------------------
# pre-compiled result
fit.BCC2 <- readRDS("../inst/extdata/PBCseq.rds")

## ---- warning=F, message=F----------------------------------------------------
print(fit.BCC2$summary.stat$PPI)
print(fit.BCC2$summary.stat$ALPHA)
print(fit.BCC2$cluster.global)
print(fit.BCC2$cluster.local[[1]])
print(fit.BCC2$cluster.local[[2]])
print(fit.BCC2$cluster.local[[3]])

## ---- warning=F, message=F, fig.height= 6, fig.width= 12, fig.align='center'----
gp1 <- trajplot(fit=fit.BCC2,feature.ind=1,which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[1] ==.(round(fit.BCC2$alpha[1],2)),")")),
			xlab="months",ylab="lbili",color=c("#00BA38", "#619CFF"))
gp2 <- trajplot(fit=fit.BCC2,feature.ind=2,which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[2] ==.(round(fit.BCC2$alpha[2],2)),")")),
			xlab="months",ylab="platelet",color=c("#00BA38", "#619CFF"))
gp3 <- trajplot(fit=fit.BCC2,feature.ind=3,which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[3] ==.(round(fit.BCC2$alpha[3],2)),")")),
			xlab="months",ylab="spiders",color=c("#00BA38", "#619CFF"))
gp4 <- trajplot(fit=fit.BCC2,feature.ind=1,which.cluster = "global.cluster",
			title="Global Clustering",
			xlab="months",ylab="lbili",color=c("#00BA38", "#619CFF"))
gp5 <- trajplot(fit=fit.BCC2,feature.ind=2,which.cluster = "global.cluster",
			title="Global Clustering",
			xlab="months",ylab="platelet",color=c("#00BA38", "#619CFF"))
gp6 <- trajplot(fit=fit.BCC2,feature.ind=3,which.cluster = "global.cluster",
			title="Global Clustering",
			xlab="months",ylab="spiders",color=c("#00BA38", "#619CFF"))

library(cowplot)
#dev.new(width=180, height=120)
plot_grid(gp1, gp2,gp3,gp4,gp5,gp6, 
          labels=c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)"), ncol = 3,   align = "v" )

## -----------------------------------------------------------------------------
sessionInfo()

