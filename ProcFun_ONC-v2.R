# MIT License
# 
# ProcFun_ONC-v2.R: R functions for organizational neuroscience commentary.
# Copyright (c) 2019 Guy A. Prochilo
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
# 
# Contact
# Name: Guy A. Prochilo
# Email: guy.prochilo@gmail.com
# 
# Last update: April 2019
# 
# Cite as:
# Prochilo, G. A. (2019). ProcFun_ONC-v2.R: R functions for organizational neuroscience commentary. 
# Retrieved from https://github.com/gprochilo/org_neuro_com.

#-------------------------------------------------------------------------------

# Print the above license whenever ProchFun_ONC-v2.R is sourced
# If the license exceeds 32 lines in future, update 32 to a higher number

writeLines(readLines("ProcFun_ONC-v2.R",32))

#-------------------------------------------------------------------------------
# Begin scripts
#-------------------------------------------------------------------------------

cite.plot <- function(csv.name = "citeData.csv", 
                      keep.cite = c(
                        "Waldman et al. (2011)",
                        "Boyatzis et al. (2012)",
                        "Peterson et al. (2008)",
                        "Waldman et al. (2013a; 2015)",
                        "Kim and James (2015)"), 
                      save.plot = TRUE){

################################################################################
# This function reproduces the Figure 1 citations plot.
#
# Example use:
# cite.plot()
################################################################################
  
  # Define a dataframe (df) using the csv file input
  
  df <- read.csv(csv.name)
  
  # Compute descriptive statistics
  
  mu = mean(df$cite)
  stdev = sd(df$cite)
  minmax = sprintf("%.f to %.f", min(df$cite), max(df$cite))
  quants = quantile(df$cite, probs = seq(0.10,0.90,0.10))
  
  # Define citations to be labelled (incl) or not labelled (n.incl) in the plot
  # We will labels citations with letters, from lowest citation count to highest 
  # citation count
  
  data = keep.cite
  incl = df[df$paper %in% data,]
  incl = cbind(incl, LETTERS[length(data):1])
  colnames(incl) = c("cite", "papers", "letters")
  n.incl = df[!df$paper %in% data,]
  
  # Generate a plot of the data using ggplot2
  
  #geom_text_repel() is part of the ggrepel pacakge:
  # Kamil Slowikowski (2018). ggrepel: Automatically Position Non-Overlapping Text
  # Labels with 'ggplot2'. R package version 0.8.0.
  # https://CRAN.R-project.org/package=ggrepel
  
  pl <- 
    ggplot() + 
    geom_violin(data = df, aes(x = "", y = cite), alpha = .5) +
    geom_point(data = incl, aes(x = "", y = cite), size = 2) + 
    geom_point(data = n.incl, aes(x = "", y = cite), shape = 1, size = 2) + 
    geom_segment(aes(x = 0.8, xend = 1.2, y = mu, yend = mu), size = 1) +
    rotate() + 
    geom_text_repel(data = incl, aes(x = "", y = cite, label = letters), size = 5,
      nudge_x = 0.5,
      hjust = 5,
      direction = "x",
      angle = 0,
      vjust = 0,
      segment.size = 0.2) +
    scale_y_continuous(breaks = pretty_breaks(n=20), name = "Citation Count")+
    scale_x_discrete(breaks = NULL, name = NULL) +
    theme_light()
    
    # Print the plot and relevant descriptive statistics
  
    print(pl)
    print(list(mu = mu, stdev = stdev, range = minmax, quants = quants))

    # Save the plot as an EPS and PNG file. We need to account for the alpha
    # transparancy of the violin plot by using device as cairo
    
    if(save.plot==TRUE){
      ggsave(filename = here("plots", "figure1.eps"), plot = pl, width = 7, height = 3,units = "in", device=cairo_ps)
      ggsave(filename = here("plots", "figure1.png"), plot = pl, width = 7, height = 3,units = "in", dpi = 600, type = "cairo")
      print("EPS and PNG files generated.")
    }
    }

#-------------------------------------------------------------------------------

plot.r <-function(r, n, plot = TRUE, save.plot = TRUE){
  
################################################################################
# This function generates scatterplots consistent with a Pearson correlation
# coefficient and sample size to demonstrate the importance of plotting scatter
# plots.
#
# Example use:
# plot.r(r = 0.36, n = 50, plot = TRUE)
################################################################################

  # plot_r() is part of the cannonball package:
  # Jan Vanhove (NA). cannonball: Tools for Teaching Statistics. R package version
  # 0.0.0.9000.
  
plot_r(r = r, n = n, plot = plot)

  # Save the generated plot as an EPS file
  
if(save.plot==TRUE){
setEPS()
postscript(here("plots","figure2.eps"), horizontal = FALSE, onefile = FALSE, paper = "special", 
           height = 10, width=10)
a = plot_r(r = r, n = n, plot = plot)
dev.off()

# Save the generated plot as a PNG file

png(filename = here("plots","figure2.png"), width = 10, height = 10, units = "in", res = 600)
a = plot_r(r = r, n = n, plot = plot)
dev.off()

print("EPS and PNG files generated.")
}
  
}

#-------------------------------------------------------------------------------

r.confidence <- function(r, n, twotailed = TRUE, pr = TRUE) {

################################################################################
# This function computes a 95% CI for a given Pearson correlation coefficient
# (r) and sample sample size (n), and allows for twotailed (TRUE) and onetailed
# (FALSE) CIs. If pr = TRUE it will print the result in a reportable format. 
# 
# Example use:
# r.confidence(0.36,50,twotailed = TRUE)
################################################################################
    
  # Compute degrees of freedom
  
  df = n-2
  
  # Compute the Upper and Lower CI using Fishers method
  
  # r.con() is part of the psych package:
  # Revelle, W. (2018) psych: Procedures for Personality and Psychological
  # Research, Northwestern University, Evanston, Illinois, USA,
  # https://CRAN.R-project.org/package=psych Version = 1.8.12.
  
  r.lower <- r.con(rho = r, n = n, twotailed = twotailed)[1]
  r.upper <- r.con(rho = r, n = n, twotailed = twotailed)[2]
  
  # Compute the t value for the combination of r and n
  
  # r2t() is part of the psych package
  
  t.val <- r2t(rho = r, n = n)
  
  # Compute p value for t and df combination. 
  # If a two-tailed test is specified, this value is doubled
  
  p.val <- pt(-abs(t.val), df = n-2)
  if (twotailed == TRUE){p.val <- p.val*2}
  
  # Print the result in a reportable format
  
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

zou.test <-function(r.jk, 
                    r.jh, 
                    r.kh, 
                    n, 
                    twotailed = TRUE,
                    alpha = .05,
                    conf.level=0.95, 
                    null.value = 0, 
                    plot = TRUE, 
                    save.plot = TRUE){
  
################################################################################
# This function uses Zou's method to compute a confidence interval for the 
# difference between two overlapping dependent Pearson correlation coefficients. 
# r.jk = correlation 1, r.jh = correlation 2, r.kh = overlapping dependent 
# correlation, n = sample size, twotailed (TRUE = 2-tail, FALSE = 1-tail), 
# conf.level = 95% by default, null.value = 0 difference.
#
# Example use:
# zou.test(r.jk=.36, r.jh=.26, r.kh=.39, n=50, twotailed = TRUE, alpha=0.05, 
# conf.level=0.95, null.value=0, plot = FALSE, save.plot = FALSE)
################################################################################  

  # Compute the 95% CI on each individual correlation being compared using the 
  # r.confidence function
   
  res = sapply(X = c(r.jk, r.jh), FUN = function(i){
    r.confidence(r = i, n = n, twotailed = twotailed, pr = FALSE)
  })
  
  # Compute a confidence interval for the difference between two overlapping 
  # dependent Pearson correlation coefficients.
  
  # cocor.dep.groups.overlap is part of the coror package:
  # Diedenhofen, B. & Musch, J. (2015). cocor: A Comprehensive Solution for the
  # Statistical Comparison of Correlations. PLoS ONE, 10(4): e0121945. doi:
  # 10.1371/journal.pone.0121945 Available:
  # http://dx.doi.org/10.1371/journal.pone.0121945
  
  zou <- cocor.dep.groups.overlap( r.jk=r.jk, 
                                r.jh=r.jh, 
                                r.kh=r.kh, 
                                n=n, 
                                alternative="two.sided", 
                                alpha=alpha, 
                                conf.level=conf.level, 
                                null.value=null.value,
                                return.htest = TRUE)$zou2007
  
  # Compute the difference between each correlation that is being compared
  
  r.diff <- res[,1]$r-res[,2]$r
  
  # Save the 95% CI on the differences computed by the cocor function
  # Save the lower and upper limits of this 95% CI
  
  zou.conf <- as.numeric(zou$conf.int)
  zou.conf.lower <- min(zou.conf)
  zou.conf.upper <- max(zou.conf)
  
  # Generate nice labels for the plots
  
  label1 = sprintf("%.2f [%.2f, %.2f]", res[,1]$r,res[,1]$ci.lower,  res[,1]$ci.upper)
  label2 = sprintf("%.2f [%.2f, %.2f]", res[,2]$r,res[,2]$ci.lower,  res[,2]$ci.upper)
  label3 = sprintf("%.2f [%.2f, %.2f]", r.diff,zou.conf.lower,zou.conf.upper)
  
  # Generate a plot of the two correlation coefficients being compared. The 
  # plot includes their 95% CIs, a label specifying their correlation magnitude
  # and 95% CI, and a dashed line indicating the null of zero
  
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
  
  # Generate a plot of the correlation difference and its 95% CI. Also includes 
  # a dashed line indicating a null of zero
  
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
  
  # Combine each of the above plots into a single image
  
  # ggarrange() is part of the ggpubr package:
  # Alboukadel Kassambara (2018). ggpubr: 'ggplot2' Based Publication Ready Plots.
  # R package version 0.2. https://CRAN.R-project.org/package=ggpubr
  
  final.plot <- ggarrange(r.plot1, r.plot2,ncol=1,nrow=2,align="hv", heights = c(2,1.5))
  
  # Print the plot
  
  if(plot==TRUE){print(final.plot)}
  
  # Save the plot as a png and eps file
  
  if(save.plot==TRUE){
  ggsave(filename = here("plots", "figure3.eps"), plot = final.plot, width = 7, height = 4,units = "in")
  ggsave(filename = here("plots","figure3.png"), plot = final.plot, width = 7, height = 4,units = "in")
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
                     save.plot = TRUE,
                     n.comparison.label = 20){

################################################################################
# This function reproduces Figure 4. It simulates the multiple comparisons 
# problem across studies that perform different numbers of multiple comparisons
# in the same study. 
# comparisons = number of comparisons to perform in each study, from 1 to comparisons
# sampleSize = sample size for each study
# reps = number of replicates of each study
# seedval = TRUE if you want the exact result each time
# plotname = name for the plot that is generated
# save.plot = saves the plot as eps and png file
# n.comparison.label = the Type I error rate you want to highlight for a specific
# number of comparisons (defaults to 20 for the corresponding publication)
#
# Example use:
# errorPlot(comparisons = 50,sampleSize = 17,reps = 10000,plotname = "errorPlot", n.comparison.label = 20)
################################################################################
  
  # Define comparisons and sample size using the following notation
    
  n = comparisons
  N = sampleSize
  
  print("Generating samples. Please wait.")
  
  # Draw n=reps number of Pearson correlation coefficients from a bivariate
  # normal distribution (rPearson), with rho = 0 and sample size = N. E.g., if 
  # reps = 1000, it will draw 1000 correlation coefficients. 
  
  # Replicate this process i number of times, where i = number of comparisons 
  # in a single study.
  
  # Compute a p-value for each correlation coeffient using r.confidence
  
  # sapply will run through i = 1 to i = max number of comparisons requested by 
  # the user
  
  # The resulting pval variable is a list of matrices. The first matrix is 
  # has 1 column because only 1 comparison was performed, and n = reps number
  # of p values. The second matrix has 2 columns because 2 comparisons were
  # performed, and n = reps number of p values in each column. And so on until
  # reaching a matrix with n=comparisons number of columns, with n=reps number
  # of p values in each column
  
  pval=
    pbsapply(1:n, function(i){
      if(seedval==TRUE){set.seed(0)}
      X <- replicate(n=i, expr=rPearson(n=reps,N=N,rho=0))
      r.confidence(r=X,n=N,twotailed = TRUE,pr = FALSE)$p.val
    })
  
  print(paste("Finding mean error rate for comparisons from 1 to", n))
  
  # pval[[1]][1,] will return the p value for the first replicate of a study
  # with n = 1 comparisons. pval[[1]][2,] will return the p value for the second 
  # replicate of a study with n = 1 comparisons, and so on. 
  
  # pval[[2]][1,] will return two p values for the first replicate of a study 
  # with n = 2 comparisons. pval[[2]][2,] will return two p values for the 
  # second replicate of a study with n = 2 comparisons, and so on. 
  
  # any(pval[[i]][j,] < 0.05) checks whether *any* of these replicates report a 
  # false positive. If there is more than 1 comparison occuring in a study, it 
  # will check if *any* of these multiple comparisons report p < .05. If there 
  # are *any* p < .05, the function will return TRUE. 
  
  # sapply(X=1:reps, FUN=function(j){any(pval[[i]][j,] < 0.05)} goes through 
  # every replicate of a study. It feeds the 'j' in 'any(pval[[i]][j,] < 0.05)'. 
  # It will return TRUE if *any* p < .05, and FALSE if p !< .05. Taking the mean 
  # of these values will coerce TRUE = 1 and FALSE = 0. This means we compute the 
  # mean probability of a Type 1 error for that specific number of comparisons. 
  
  # pbsapply(1:n, function(i)) repeats the above for every number of comparisons 
  # performed. It feeds the 'i' in 'any(pval[[i]][j,] < 0.05)'.
  
  # The resulting res variable is a vector of Type 1 error probabilities for each
  # number of comparisons performed, from 1:comparisons.

  res = 
    pbsapply(1:n, function(i){
      mean(sapply(X=1:reps, FUN=function(j){
        any(pval[[i]][j,] < 0.05)}))
    })
  
  print("Finding mean error rate for comparisons with Bonferroni correction")
  
  # The above function is repeated but with a Bonferroni correction (0.05/comparisons)
  
  res2 = 
    pbsapply(1:n, function(i){
      mean(sapply(X=1:reps, FUN=function(j){
        any(p = pval[[i]][j,] < 0.05/i)}))
    })
  
  # Combine the non-corrected and corrected results into a dataframe
  
  resFin  = t(as.data.frame(rbind(res,res2)))
  
  # Give the rownames numbers that correspond to the number of comparisons performed
  
  rownames(resFin) = 1:length(res)
  
  # Convert the data to long format so it can be used easily in ggplot
  
  resFin = melt(resFin,varnames = c("res","res2"))

  # Label the number of comparsions Type 1 error you want to highlight on the plot
  
  label1 = sprintf("%s = %.3f", "\u03b1",resFin$value[n.comparison.label])
  label2 = sprintf("%s = %.3f", "\u03b1",resFin$value[n+n.comparison.label])
  
  # Generate a plot of corrected and non-corrected Type I error probabilities
  
  p=
    ggplot(data = resFin, aes(x=res, y = value, linetype = res2)) +
    geom_smooth(size = 1, method = "loess", fullrange=TRUE, color = "black", se = FALSE)+
    #geom_line(aes(x=res2$V1,y=res2$res2), size = 1, ) +
    scale_y_continuous(breaks=pretty_breaks(n=20), name = "\u03b1") +
    scale_x_continuous(breaks=pretty_breaks(n=15),limits=c(1,50), name = 
                         "Number of comparisons") +
    geom_vline(xintercept = n.comparison.label) +
    geom_label(aes(x = n.comparison.label, y = resFin$value[n.comparison.label]), label = label1) +
    geom_label(aes(x = n.comparison.label, y = resFin$value[n+n.comparison.label]), label = label2) +
    scale_linetype_manual(name = "Legend", 
                          values = c(1,2),
                          breaks = c("res","res2"), 
                          labels = c("Uncorrected","Corrected")) +
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(keywidth = 3, keyheight = 1))+
    theme_light()
  
  print(p)
  
  # Save plots as png and eps files
  
  if(save.plot==TRUE){
  plotname1 = sprintf("%s.eps", plotname)
  ggsave(filename = here("plots", plotname1), plot = p, width = 8, height = 4, units = "in")
  plotname2 = sprintf("%s.png", plotname)
  ggsave(filename = here("plots", plotname2), plot = p, width = 8, height = 4, units = "in")
  print("EPS and PNG files generated.")
  }
  
  return(resFin)
}

#-------------------------------------------------------------------------------

errorPlot.lazy <-function(csv.name, plotname = "figure4", save.plot=TRUE, n.comparison.label = 20){
  
################################################################################
# This function reproduces Figure 4 without the need to re-run the entire 
# simulation. The csv input assumes that you are merely using the csv file 
# that has been generated using the errorPlot() function. 
# You can choose which number of comparisons you want highlighted in the label
# of the plot by specifying n.comparison.label = n. Default is 20 to correspond
# to the test performed in the publication. 
#
# Example use:
# errorPlot.lazy("figure_4_data.csv", n.comparison.label = 20)
################################################################################
  
  # Import the csv file data and safe as resFin
  
  resFin <- read.table(file = csv.name, header = TRUE,sep = ",",stringsAsFactors = FALSE)
  
  # Count how many replications have been performed in this dataset to define n
  
  temp = resFin[resFin$res2 == "res",]
  n = length(temp$res)
  
  # Label the number of comparsions Type 1 error you want to highlight on the plot
  
  label1 = sprintf("%s = %.3f", "\u03b1",resFin$value[n.comparison.label])
  label2 = sprintf("%s = %.3f", "\u03b1",resFin$value[n+n.comparison.label])
  
  # Generate a plot of corrected and non-corrected Type I error probabilities
  
  p=
    ggplot(data = resFin, aes(x=res, y = value, linetype = res2)) +
    geom_smooth(size = 1, method = "loess", fullrange=TRUE, color = "black", se = FALSE)+
    #geom_line(aes(x=res2$V1,y=res2$res2), size = 1, ) +
    scale_y_continuous(breaks=pretty_breaks(n=20), name = "\u03b1") +
    scale_x_continuous(breaks=pretty_breaks(n=15),limits=c(1,50), name = 
                         "Number of comparisons") +
    geom_vline(xintercept = n.comparison.label) +
    geom_label(aes(x = n.comparison.label, y = resFin$value[n.comparison.label]), label = label1) +
    geom_label(aes(x = n.comparison.label, y = resFin$value[n+n.comparison.label]), label = label2) +
    scale_linetype_manual(name = "Legend", 
                          values = c(1,2),
                          breaks = c("res","res2"), 
                          labels = c("Uncorrected","Corrected")) +
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
           linetype=guide_legend(keywidth = 3, keyheight = 1))+
    theme_light()
  
  print(p)
  
  # Save plots as png and eps files
  
  if(save.plot==TRUE){
    plotname1 = sprintf("%s.eps", plotname)
    ggsave(filename = here("plots",plotname1), plot = p, width = 8, height = 4, units = "in")
    plotname2 = sprintf("%s.png", plotname)
    ggsave(filename = here("plots",plotname2), plot = p, width = 8, height = 4, units = "in")
    print("EPS and PNG files generated.")
  }
  
  # Rename res and res2 to more easily understandable variable names, and rename columns
  
  resFin$res2[resFin$res2 == "res"] <- "uncorrected"
  resFin$res2[resFin$res2 == "res2"] <- "corrected"
  colnames(resFin) <- c("comparisons","status","alpha")
  
  return(resFin)
  
}

#-------------------------------------------------------------------------------

critical.r <- function(n, alpha = .05, twotailed = TRUE){
  
################################################################################
# This function finds the critical t and r value for a given bivariate sample
# size, alpha, and tails of a test. 
#
# Example use:
# critical.r(n = 17, alpha = .05, twotailed = TRUE)
################################################################################
  
  # t2r() is part of the psych package:
  # Revelle, W. (2018) psych: Procedures for Personality and Psychological
  # Research, Northwestern University, Evanston, Illinois, USA,
  # https://CRAN.R-project.org/package=psych Version = 1.8.12.
  
  if(twotailed==TRUE){tail=2}
  if(twotailed!=TRUE){tail=1}
  t.crit <- qt(1-(alpha/tail), n-2)
  r.crit <- t2r(t = t.crit, df = n-2)
  
  return(list(t.crit = t.crit, r.crit = r.crit))
}

#-------------------------------------------------------------------------------
# End script
#-------------------------------------------------------------------------------
