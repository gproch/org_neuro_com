### Prochilo functions for Organizational Neuroscience Commentary

#-------------------------------------------------------------------------------

#Plot the citation data

bean.plot <- function(csv.name, keep.cite, save.plot = TRUE){
  
  data.set <- read.table(file = csv.name, header = TRUE, sep = ",",fill = FALSE)
  
  mu <- mean(data.set$cite)
  stdev <- sd(data.set$cite)
  ran <- sprintf("%.f to %.f", min(data.set$cite), max(data.set$cite))
  quants <- quantile(data.set$cite, probs = seq(0.10,0.90,0.10))
  
  data = keep.cite
  keep = data.set[data.set$paper %in% keep.cite,]
  keep = cbind(keep, LETTERS[length(keep.cite):1])
  colnames(keep) = c("cite","papers","letters")
  other = data.set[!data.set$paper %in% keep.cite,]
  

  beanplot <- 
    ggplot(data = data.set, aes(x = "", y = cite)) +
    geom_violin(aes(y=cite), alpha = .8) +
    geom_segment(aes(x=0.8,xend=1.2,y=mu,yend=mu), size = 1) +
    sapply(X = quants, FUN = function(i){
      geom_segment(aes(x=0.9,xend=1.1,y=i,yend=i), size = 1)
    }) +
    geom_jitter(data = keep,aes(y=cite),width=.1, size = 3) +
    geom_jitter(data = other,aes(y=cite), width=.1, size = 3, shape = 1) +
    
    scale_y_continuous(breaks = pretty_breaks(n=20), name = "Citation Count")+
    scale_x_discrete(breaks = NULL, name = NULL) +
    theme_light() +
    geom_label_repel(data = keep, aes(label = letters), size = 3)+
    rotate()
  
  print(beanplot)
  print(list(mu = mu, stdev = stdev, range = ran, quants = quants))
  
  if(save.plot==TRUE){
    ggsave(filename = "figure1.eps", plot = beanplot, width = 7, height = 3,units = "in", device=cairo_ps)
    ggsave(filename = "figure1.png", plot = beanplot, width = 7, height = 3,units = "in", dpi = 300, type = "cairo")
    print("EPS and PNG files generated.")
  }
  
}

#-------------------------------------------------------------------------------

plot.r <-function(r, n, plot = TRUE, save.plot = TRUE){

plot_r(r = r, n = n, plot = plot)

if(save.plot==TRUE){
setEPS()
postscript("figure2.eps", horizontal = FALSE, onefile = FALSE, paper = "special", 
           height = 10, width=10)
a = plot_r(r = r, n = n, plot = plot)
dev.off()

png(filename = "figure2.png", width = 10, height = 10, units = "in", res = 300)
a = plot_r(r = r, n = n, plot = plot)
dev.off()
print("EPS and PNG files generated.")
}
  
}

#-------------------------------------------------------------------------------

#r.confidence requires: r (correlation) and n (sample size)
#if conducting a one-tailed test, set twotailed = FALSE

r.confidence <- function(r, n, twotailed = TRUE, pr = TRUE) {
  
  df = n-2
  
  #Calculates the Upper and Lower CI using Fishers method
  
  r.lower <- r.con(rho = r, n = n, twotailed = twotailed)[1]
  r.upper <- r.con(rho = r, n = n, twotailed = twotailed)[2]
  
  #Calculates t value for the combination of r and n
  
  t.val <- r2t(rho = r, n = n)
  
  #Calculates p value for t and df. If a two-tailed test, this value is doubled
  
  p.val <- pt(-abs(t.val), df = n-2)
  if (twotailed == TRUE){p.val <- p.val*2}
  
  res = sprintf("r(%.0f) = %.2f [%.2f, %.2f], p = %.3f", df, r, r.lower,r.upper,p.val)
  if(pr==TRUE){print(list(result = res))}
  
  return(list(r = r, 
              n = n,
              df = df,
              ci.lower = r.lower,
              ci.upper = r.upper,
              ci.width = r.upper-r.lower,
              MarginOfError = (r.upper-r.lower)/2,
              t.val = t.val,
              p.val = p.val))
}

#-------------------------------------------------------------------------------

zou <- cocor.dep.groups.overlap(r.jk=.36, 
                         r.jh=.26, 
                         r.kh=.39, 
                         n=50, 
                         alternative="two.sided", 
                         alpha=0.05, 
                         conf.level=0.95, 
                         null.value=0,
                         return.htest = TRUE)$zou2007

#-------------------------------------------------------------------------------

zou.test <-function(r.jk, r.jh, r.kh, n, twotailed = TRUE,alpha = .05,
                    conf.level=0.95, null.value = 0, plot = TRUE, save.plot = TRUE){
  
  res = sapply(X = c(r.jk, r.jh), FUN = function(i){
    r.confidence(r = i, n = n, twotailed = twotailed, pr = FALSE)
  })
  
  zou <- cocor.dep.groups.overlap( r.jk=r.jk, 
                                r.jh=r.jh, 
                                r.kh=r.kh, 
                                n=n, 
                                alternative="two.sided", 
                                alpha=alpha, 
                                conf.level=conf.level, 
                                null.value=null.value,
                                return.htest = TRUE)$zou2007
  
  r.diff <- res[,1]$r-res[,2]$r
  zou.conf <- as.numeric(zou$conf.int)
  zou.conf.lower <- min(zou.conf)
  zou.conf.upper <- max(zou.conf)
  
  label1 = sprintf("%.2f [%.2f, %.2f]", res[,1]$r,res[,1]$ci.lower,  res[,1]$ci.upper)
  label2 = sprintf("%.2f [%.2f, %.2f]", res[,2]$r,res[,2]$ci.lower,  res[,2]$ci.upper)
  label3 = sprintf("%.2f [%.2f, %.2f]", r.diff,zou.conf.lower,zou.conf.upper)
  
  r.plot1 <-
    ggplot() +
    geom_errorbar(data = as.data.frame(res[,1]), aes(x = 1, ymin = ci.lower, ymax = ci.upper), 
                  width = 0.2, size = 0.3)+
    geom_errorbar(data = as.data.frame(res[,2]), aes(x = 2, ymin = ci.lower, ymax = ci.upper), 
                width = 0.2, size = 0.3) +
    scale_x_discrete(breaks = 2, name = NULL) +
    scale_y_continuous(breaks = pretty_breaks(n=10), expand = c(-0.1, .1)) +
    geom_hline(yintercept = 0, 
               color = "black", 
               size = 0.5, 
               linetype = "dashed") +
    geom_segment(data = as.data.frame(res[,1]), aes(x=0.9,xend=1.1,y=r,yend=r), size = 1) +
    geom_segment(data = as.data.frame(res[,2]), aes(x=0.9,xend=1.1,y=r,yend=r), size = 1, position = position_nudge(x = 1)) +
    geom_label(data = as.data.frame(res[,1]), aes(x = 1, y = r), 
               label = label1, vjust = -1, size = 4.4)+
    geom_label(data = as.data.frame(res[,2]), aes(x = 1, y = r), 
               label = label2, vjust = -1, size = 4.4, position = position_nudge(x = 1))+
    rotate() +
    theme_light()
  
  r.plot2 <-
    ggplot() +
    geom_errorbar(aes(x = 1, ymin = zou.conf.lower, ymax = zou.conf.upper), 
                  width = 0.2, size = 0.3)+
    scale_x_discrete(breaks = 2, name = NULL) +
    scale_y_continuous(breaks = pretty_breaks(n=10), expand = c(-0.1, .1)) +
    geom_hline(yintercept = 0, 
               color = "black", 
               size = 0.5, 
               linetype = "dashed") +
    geom_segment(aes(x=0.9,xend=1.1,y=r.diff,yend=r.diff), size = 1) +
    geom_label(aes(x = 1, y = r.diff), label = label3, vjust = -1, size = 4.4)+
    rotate() +
    theme_light()
  
  
  final.plot <- ggarrange(r.plot1, r.plot2,ncol=1,nrow=2,align="hv", heights = c(2,1.5))
  
  if(plot==TRUE){print(final.plot)}
  
  if(save.plot==TRUE){
  ggsave(filename = "figure3.eps", plot = final.plot, width = 7, height = 4,units = "in")
  ggsave(filename = "figure3.png", plot = final.plot, width = 7, height = 4,units = "in")
  print("EPS and PNG files generated.")
  }
  
  return(list(r.jk = label1, r.jh = label2, r.diff = label3))
  
}

#-------------------------------------------------------------------------------

errorPlot <-function(comparisons, 
                     sampleSize, 
                     reps, 
                     seedval = TRUE, 
                     plotname = "figure4",
                     save.plot = TRUE){
  
  n = comparisons
  N=sampleSize
  
  print("Generating samples. Please wait.")
  
  pval=
    pbsapply(1:n, function(i){
      if(seedval==TRUE){set.seed(0)}
      X <- replicate(n=i, expr=rPearson(n=reps,N=N,rho=0))
      r.confidence(r=X,n=N,twotailed = TRUE,pr = FALSE)$p.val
    })
  
  print(paste("Finding mean error rate for comparisons from 1 to", n))
  
  res = 
    pbsapply(1:n, function(i){
      mean(sapply(X=1:reps, FUN=function(j){
        any(pval[[i]][j,] < 0.05)}))
    })
  
  print("Finding mean error rate for comparisons with Bonferroni correction")
  
  res2 = 
    pbsapply(1:n, function(i){
      mean(sapply(X=1:reps, FUN=function(j){
        any(p = pval[[i]][j,] < 0.05/i)}))
    })
  
  resFin  = t(as.data.frame(rbind(res,res2)))
  rownames(resFin) = 1:length(res)
  resFin = melt(resFin,varnames = c("res","res2"))

  
  label1 = sprintf("%s = %.3f", "\u03b1",resFin$value[20])
  label2 = sprintf("%s = %.3f", "\u03b1",resFin$value[70])
  
  p=
    ggplot(data = resFin, aes(x=res, y = value, linetype = res2)) +
    geom_smooth(size = 1, method = "loess", fullrange=TRUE, color = "black", se = FALSE)+
    #geom_line(aes(x=res2$V1,y=res2$res2), size = 1, ) +
    scale_y_continuous(breaks=pretty_breaks(n=20), name = "\u03b1") +
    scale_x_continuous(breaks=pretty_breaks(n=15),limits=c(1,50), name = 
                         "Number of comparisons") +
    geom_vline(xintercept = 20) +
    geom_label(aes(x = 20, y = 0.65), label = label1) +
    geom_label(aes(x = 20, y = 0.05), label = label2) +
    scale_linetype_manual(name = "Legend", 
                          values = c(1,2),
                          breaks = c("res","res2"), 
                          labels = c("Uncorrected","Corrected")) +
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(keywidth = 3, keyheight = 1))+
    theme_light()
  
  print(p)
  
  if(save.plot==TRUE){
  plotname1 = sprintf("%s.eps", plotname)
  ggsave(filename = plotname1, plot = p, width = 8, height = 4, units = "in")
  plotname2 = sprintf("%s.png", plotname)
  ggsave(filename = plotname2, plot = p, width = 8, height = 4, units = "in")
  print("EPS and PNG files generated.")
  }
  
  return(resFin)
}

#-------------------------------------------------------------------------------

errorPlot.lazy <-function(csv.name, plotname = "figure4", save.plot=TRUE){
  
  resFin <- read.table(file = csv.name, header = TRUE,sep = ",",stringsAsFactors = FALSE)
  
  label1 = sprintf("%s = %.3f", "\u03b1",resFin$value[20])
  label2 = sprintf("%s = %.3f", "\u03b1",resFin$value[70])
  
  p=
    ggplot(data = resFin, aes(x=res, y = value, linetype = res2)) +
    geom_smooth(size = 1, method = "loess", fullrange=TRUE, color = "black", se = FALSE)+
    #geom_line(aes(x=res2$V1,y=res2$res2), size = 1, ) +
    scale_y_continuous(breaks=pretty_breaks(n=20), name = "\u03b1") +
    scale_x_continuous(breaks=pretty_breaks(n=15),limits=c(1,50), name = 
                         "Number of comparisons") +
    geom_vline(xintercept = 20) +
    geom_label(aes(x = 20, y = 0.65), label = label1) +
    geom_label(aes(x = 20, y = 0.05), label = label2) +
    scale_linetype_manual(name = "Legend", 
                          values = c(1,2),
                          breaks = c("res","res2"), 
                          labels = c("Uncorrected","Corrected")) +
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(keywidth = 3, keyheight = 1))+
    theme_light()
  
  print(p)
  
  if(save.plot==TRUE){
    plotname1 = sprintf("%s.eps", plotname)
    ggsave(filename = plotname1, plot = p, width = 8, height = 4, units = "in")
    plotname2 = sprintf("%s.png", plotname)
    ggsave(filename = plotname2, plot = p, width = 8, height = 4, units = "in")
    print("EPS and PNG files generated.")
  }
  
  resFin$res2[resFin$res2 == "res"] <- "uncorrected"
  resFin$res2[resFin$res2 == "res2"] <- "corrected"
  colnames(resFin) <- c("comparisons","status","alpha")
  
  return(resFin)
  
}

#-------------------------------------------------------------------------------

critical.r <- function(n, alpha = .05, twotailed = TRUE){
  if(twotailed==TRUE){tail=2}
  if(twotailed!=TRUE){tail=1}
  t.crit <- qt(1-(alpha/tail), n-2)
  r.crit <- t2r(t = t.crit, df = n-2)
  
  return(list(t.crit = t.crit, r.crit = r.crit))
}

#-------------------------------------------------------------------------------
# Functions for plots that didn't make the final cut for the manuscript...

# Generate cor data

gendat <- function(rho, r.range, pr = TRUE){
  set.seed(0)
  
  gen <- function(){
    set.seed(0)
    n = 1000000
    r = rho
    data = mvrnorm(n=n, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
    X = data[,1]
    Y = data[,2]
    res = cbind(X,Y)
    return(res)
  }
  
  a = gen()
  
  for(i in 1:5000){
    samp = a[sample(nrow(a),50, replace = FALSE),]
    res = cor.test(samp[,1],samp[,2])
    final = res$estimate
    if(final >= r.range[1] & final <= r.range[2]){
      res = cor.test(samp[,1],samp[,2])
      pres = sprintf("r = %.3f [%.3f, %.3f], p = %.3f", 
                     as.numeric(res$estimate),
                     as.numeric(res$conf.int)[1],
                     as.numeric(res$conf.int)[2],
                     as.numeric(res$p.value))
      if(pr==TRUE){print(pres)}
      return(as.data.frame(samp))}
  }
}


#-------------------------------------------------------------------------------
# Functions for plots that didn't make the final cut for the manuscript...

cor.plot <- function(show.ci = TRUE, save.plot = TRUE){
  
  if(show.ci==FALSE){plot.name = "figure2A"}
  if(show.ci==TRUE){plot.name = "figure2B"}
  
  a = gendat(rho = 0.1, r.range = c(0.387,0.389))
  a = as.data.frame(a)
  
  res = cor.test(a$X,a$Y)
  r = as.numeric(res$estimate)
  lower = as.numeric(res$conf.int)[1]
  upper = as.numeric(res$conf.int)[2]
  pval = as.numeric(res$p.value)
  pval2 = format(round(pval,3), digits = 3)
  pval2 = substring(pval2,2)
  
  if(show.ci==TRUE){plottext = sprintf("r = %.2f [%.2f, %.2f], p = %s",r,lower,upper,pval2)}
  if(show.ci!=TRUE){plottext = sprintf("r = %.2f, p = %s",r,pval2)}
  
  text <- ggparagraph(plottext, size = 15, lineheight = 1)
  
  corr.plot <- 
    ggplot(a, aes(x = X, y = Y)) +
    geom_point(size = 3) +
    ggtitle(label = plottext) +
    scale_x_continuous(breaks=pretty_breaks(10), name = "x") + 
    scale_y_continuous(breaks=pretty_breaks(10), name = "y") +
    theme_light()
  
  if(show.ci==TRUE){
    c1 = geom_smooth(method = lm, se = TRUE, color = "black", size = 1)
    corr.plot = corr.plot + c1
  }
  
  if(show.ci==FALSE){
    c1 = geom_smooth(method = lm, se = FALSE, color = "black", size = 1)
    corr.plot = corr.plot + c1
  }
  
  print(corr.plot)
  
  if(save.plot==TRUE){ 
    plot.name1 = sprintf("%s.eps", plot.name)
    ggsave(filename = plot.name1, plot = corr.plot, width = 6, height = 6, units = "in",device=cairo_ps)
    plot.name2 = sprintf("%s.png", plot.name)
    ggsave(filename = plot.name2, plot = corr.plot, width = 6, height = 6, units = "in", dpi = 300, type = "cairo")
    print("EPS and PNG files generated.")
  }
}

#-------------------------------------------------------------------------------
# Functions for plots that didn't make the final cut for the manuscript...
# library(SuppDists)

r.sampling <- function(r, n, samples,color = c("pstat","rci"),seedval = NULL, save.plot=TRUE){
  print("Starting. Please wait.")
  
  if(!is.null(seedval)){set.seed(seedval)}
  rdist = rPearson(n = samples, N = n, rho = r)
  
  a = sapply(X = rdist, FUN = function(i){
    res = r.confidence(r = i, n = n, twotailed = TRUE, pr=FALSE)
    lower = res$ci.lower
    upper = res$ci.upper
    rval = res$r
    pval = round(res$p.val,3)
    final = cbind(rval,lower,upper,pval)
    return(final)
  })
  
  a = t(a)
  a = cbind(1:length(a[,1]),a)
  colnames(a) = c("sample","r","lower","upper","pval")
  a = as.data.frame(a)
  
  a.temp1 = cbind(a[!a$pval<.05,], pstat = "N")
  rownames(a.temp1) = NULL
  a.temp2 = cbind(a[a$pval<.05,], pstat = "Y")
  rownames(a.temp2) = NULL
  a = rbind(a.temp1,a.temp2)
  
  # In case all 95%CI contain rho
  temp = a[!(r <= a$upper & r >= a$lower),]
  a.temp1 = NULL
  if(!is.na(temp$sample[1])){
    a.temp1 = cbind(a[!(r <= a$upper & r >= a$lower),],rci = "N")
    rownames(a.temp1) = NULL
  }
  a.temp2 = cbind(a[r <= a$upper & r >= a$lower,], rci = "Y")
  rownames(a.temp2) = NULL  
  
  a = rbind(a.temp1,a.temp2)
  a = a[order(a$sample),]
  
  if(color=="pstat"){
    g = ggplot(data = a, aes(x = sample, y = r, ymin = lower, ymax = upper,color=pstat))
    c = scale_color_manual(breaks = c("N", "Y"),
                           values=c("red", "black"),
                           labels = c("p >= .05","p < .05"))
  }
  if(color=="rci"){
    g = ggplot(data = a, aes(x = sample, y = r, ymin = lower, ymax = upper,color=rci))
    c = scale_color_manual(breaks = c("N", "Y"),
                           values=c("red", "black"),
                           labels = c("95% CI does not contain rho",
                                      "95% CI does contain rho"))
  }
  
  p = g + c +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
    theme_light() +
    geom_hline(yintercept = r) +
    geom_hline(yintercept = 0,linetype = "dashed") +
    scale_x_continuous(trans = "reverse", breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks(n=10), limits = c(-1,1)) +
    geom_text(data = a[a$pval < .05,], aes(label = substring(format(pval,nsmall=3),2),
                                           y = max(a$upper+0.1)),size = 4) +
    rotate()
  
  print(p)
  
  psum <- sum(a$pval < .05)
  rcapt <- a[r <= a$upper & r >= a$lower,]
  rnot <- a[!(r <= a$upper & r >= a$lower),]
  rsum <- sprintf("%s %%", (length(rcapt$sample)/samples)*100)
  p.rcapt.sum <- sum(rcapt$pval<.05)
  p.rnot.sum <- length(rnot$sample)
  
  vals = a
  vals[-c(6,7)] = round(a[-c(6,7)],3)
  colnames(vals) = c("sample","r","CI.lower","CI.upper","p.val","p<.05","rho.in.CI")
  rownames(vals) <- NULL
  
  if(save.plot==TRUE){
    ggsave(filename = "figure2C.eps", plot = p, width = 8, height = 12,units = "in")
    ggsave(filename = "figure2C.png", plot = p, width = 8, height = 12,units = "in") 
    print("EPS and PNG files generated.")
  }
  
  print(list("How many 95%CIs capture the true value of rho?" = rsum))
  return(vals)
  
}