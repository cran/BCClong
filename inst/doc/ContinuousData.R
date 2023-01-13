## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning=F, message=F, fig.height= 5, fig.width= 8, fig.align='center', fig.cap= "Spaghtti plot for each marker"----
library(BCClong)
library(joineRML)
library(ggplot2)
library(cowplot)
# import data from joineRML library	(use ?epileptic.qol to see details)
data(epileptic.qol)	
# convert days to months
epileptic.qol$time_month <- epileptic.qol$time/30.25 		
# Sort by ID and time
epileptic.qol <- epileptic.qol[order(epileptic.qol$id,epileptic.qol$time_month),]  

## Make Spaghetti Plots to Visualize
p1 <- ggplot(data =epileptic.qol, aes(x =time_month, y = anxiety, group = id))+
	 	 geom_point() + geom_line() +
		 geom_smooth(method = "loess", size = 1.5,group =1,se = FALSE, span=2) +
		theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face="bold")) +
		xlab("Time (months)") + ylab("anxiety")
p2 <- ggplot(data =epileptic.qol, aes(x =time_month, y = depress, group = id))+
	 	 geom_point() +
		 geom_line() +
		 geom_smooth(method = "loess", size = 1.5,group =1,se = FALSE, span=2) +
		theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face="bold")) +
		xlab("Time (months)") + ylab("depress")
p3 <- ggplot(data =epileptic.qol, aes(x =time_month, y = aep, group = id))+
	  	geom_point() +
		 geom_line() +
		 geom_smooth(method = "loess", size = 1.5,group =1,se = FALSE, span=2) +
		theme(legend.position = "none",
			plot.title = element_text(size = 20, face = "bold"),
			axis.text=element_text(size=20),
			axis.title=element_text(size=20),
			axis.text.x = element_text(angle = 0 ),
			strip.text.x = element_text(size = 20, angle = 0),
			strip.text.y = element_text(size = 20,face="bold")) +
		xlab("Time (months)") + ylab("aep")

plot_grid(p1,NULL,p2,NULL,p3,NULL,labels=c("(A)","", "(B)","","(C)",""), nrow = 1,
		align = "v", rel_widths = c(1,0.1,1,0.1,1,0.1))

epileptic.qol$anxiety_scale <- scale(epileptic.qol$anxiety)
epileptic.qol$depress_scale <- scale(epileptic.qol$depress)
epileptic.qol$aep_scale <- scale(epileptic.qol$aep)
dat <- epileptic.qol

## ---- warning=F, message=F----------------------------------------------------
fit.BCC2 <- readRDS(file = "../inst/extdata/epil1.rds")
fit.BCC2b <- readRDS(file = "../inst/extdata/epil2.rds")
fit.BCC2c <- readRDS(file = "../inst/extdata/epil3.rds")
fit.BCC2b$cluster.global <- factor(fit.BCC2b$cluster.global,
	labels=c("Cluster 1","Cluster 2"))
table(fit.BCC2$cluster.global, fit.BCC2b$cluster.global)

fit.BCC2c$cluster.global <- factor(fit.BCC2c$cluster.global,
	labels=c("Cluster 1","Cluster 2"))
table(fit.BCC2$cluster.global, fit.BCC2c$cluster.global)

## ---- warning=F, message=F----------------------------------------------------
print(fit.BCC2$N)

print(fit.BCC2$summary.stat$PPI)
print(fit.BCC2$summary.stat$ALPHA)
print(fit.BCC2$summary.stat$GA)
print(fit.BCC2$summary.stat$SIGMA.SQ.U)
print(fit.BCC2$summary.stat$SIGMA.SQ.E)

table(fit.BCC2$cluster.global)
table(fit.BCC2$cluster.local[[1]])
table(fit.BCC2$cluster.local[[2]])
table(fit.BCC2$cluster.local[[3]])

## ---- warning=F, message=F, fig.height=5, fig.width=8, fig.align='center'-----
#=====================================================#
# Trace-plot for key model parameters
#=====================================================#
traceplot(fit=fit.BCC2, parameter="PPI",ylab="pi",xlab="MCMC samples")
traceplot(fit=fit.BCC2, parameter="ALPHA",ylab="alpha",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 1, feature.indx=1,parameter="GA",ylab="GA",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 1, feature.indx=2,parameter="GA",ylab="GA",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 1, feature.indx=3,parameter="GA",ylab="GA",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 2, feature.indx=1,parameter="GA",ylab="GA",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 2, feature.indx=2,parameter="GA",ylab="GA",xlab="MCMC samples")
traceplot(fit=fit.BCC2,cluster.indx = 2, feature.indx=3,parameter="GA",ylab="GA",xlab="MCMC samples")

## ---- warning=F, message=F, fig.width=12, fig.height=6, fig.align='center'----
#=====================================================#
# Trajectory plot for features
#=====================================================#
gp1 <- trajplot(fit=fit.BCC2,feature.ind=1,
			which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[1] ==.(round(fit.BCC2$alpha[1],2)),")")),
			xlab="time (months)",ylab="anxiety",color=c("#00BA38", "#619CFF"))
gp2 <- trajplot(fit=fit.BCC2,feature.ind=2,
			which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[2] ==.(round(fit.BCC2$alpha[2],2)),")")),
			xlab="time (months)",ylab="depress",color=c("#00BA38", "#619CFF"))
gp3 <- trajplot(fit=fit.BCC2,feature.ind=3,
			which.cluster = "local.cluster",
			title= bquote(paste("Local Clustering (",hat(alpha)[3] ==.(round(fit.BCC2$alpha[3],2)),")")),
			xlab="time (months)",ylab="aep",color=c("#00BA38", "#619CFF"))
gp4 <- trajplot(fit=fit.BCC2,feature.ind=1,
			which.cluster = "global.cluster",
			title="Global Clustering",xlab="time (months)",ylab="anxiety",color=c("#00BA38", "#619CFF"))
gp5 <- trajplot(fit=fit.BCC2,feature.ind=2,
			which.cluster = "global.cluster",
			title="Global Clustering",xlab="time (months)",ylab="depress",color=c("#00BA38", "#619CFF"))
gp6 <- trajplot(fit=fit.BCC2,feature.ind=3,
			which.cluster = "global.cluster",
			title="Global Clustering",
			xlab="time (months)",ylab="aep",color=c("#00BA38", "#619CFF"))
library(cowplot)
plot_grid(gp1, gp2,gp3,gp4,gp5,gp6,
          labels=c("(A)", "(B)", "(C)", "(D)", "(E)", "(F)"),
	    ncol = 3,   align = "v" )
plot_grid(gp1,NULL,gp2,NULL,gp3,NULL,
	    gp4,NULL,gp5,NULL,gp6,NULL,
		labels=c("(A)","", "(B)","","(C)","","(D)","","(E)","","(F)",""), nrow = 2,
		align = "v", rel_widths = c(1,0.1,1,0.1,1,0.1))

## ---- message=F, warning=F, fig.height=5, fig.width=7, fig.align='center'-----
#res <- BayesT(fit=fit.BCC2)
res <- readRDS(file = "../inst/extdata/conRes.rds")
plot(log(res$T.obs),log(res$T.rep),xlim=c(8.45,8.7), cex=1.5,
	ylim=c(8.45,8.7),xlab="Observed T statistics (in log scale)", ylab = "Predicted T statistics (in log scale)")
abline(0,1,lwd=2,col=2)

p.value <- sum(res$T.rep > res$T.obs)/length(res$T.rep)
p.value

fit.BCC2$cluster.global <- factor(fit.BCC2$cluster.global,labels=c("Cluster 1","Cluster 2"))
boxplot(fit.BCC2$postprob ~ fit.BCC2$cluster.global,ylim=c(0,1),xlab="",ylab="Posterior Cluster Probability")

## -----------------------------------------------------------------------------
sessionInfo()

