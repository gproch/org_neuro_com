# An Extended Commentary on Post-Publication Peer Review in Organizational 
# Neuroscience
#
# These commands can reproduce the analysis without the markdown file
# Before running any of the functions ensure you source the following:

source("libraries.R")
source("ProcFun_ONC-v2.R")

#-------------------------------------------------------------------------------

# Read the license file for these scripts

writeLines(readLines("ProcFun_ONC-v2.R",32))

#-------------------------------------------------------------------------------

# Reproduce Figures

# Figure 1

cite.plot()

# Figure 2

plot.r(r = 0.36, n = 50, plot = TRUE)

# Figure 3

zou.test(
  r.jk=.36, 
  r.jh=.26, 
  r.kh=.39, 
  n=50, 
  twotailed = TRUE, 
  alpha=0.05, 
  conf.level=0.95, 
  null.value=0,
  plot = TRUE
)

# Figure 4 (script simulates data for plot ~ approx 2-3 min to run)

errorPlot(comparisons = 50,sampleSize = 17,reps = 10000,plotname = "errorPlot", n.comparison.label = 20)

# Figure 4 (script uses saved simulation data)

errorPlot.lazy("figure_4_data.csv", n.comparison.label = 20)


#-------------------------------------------------------------------------------

# Waldman et al. (2011a)

# Confidence interval computations [r(48) = 0.36, p < .05)]

r.confidence(0.36,50,twotailed = TRUE)

# Confidence interval computations [r(48) = 0.26, p < .10]

r.confidence(0.26,50,twotailed = TRUE)

# Zou's test

zou.test(
  r.jk=.36, 
  r.jh=.26, 
  r.kh=.39, 
  n=50, 
  twotailed = TRUE, 
  alpha=0.05, 
  conf.level=0.95, 
  null.value=0,
  plot = FALSE,
  save.plot = FALSE
)

# Precision for planning [r = 0.36, MoE = 0.18]

pwr.confIntR(r = 0.36, w = 0.18, conf.level = 0.95)

# Precision for planning [r = 0.09, MoE = 0.045]

pwr.confIntR(r = 0.09, w = 0.045, conf.level = 0.95)

#-------------------------------------------------------------------------------

# Waldman et al. (2013a)

# Confidence interval computations [r(24) = 0.32, p < .05 (Two-tailed test)]

r.confidence(0.32,26,twotailed = TRUE)

# Confidence interval computations [r(24) = 0.32, p < .05 (Two-tailed test)]

r.confidence(0.32,26,twotailed = FALSE)

#-------------------------------------------------------------------------------

# Kim and James (2015)

# Confidence interval computations [r(48) = 0.49, p < .05]

r.confidence(0.49,17,twotailed = TRUE)

# Critical r computations [for a study with N = 17]

critical.r(n = 17, alpha = .05, twotailed = TRUE)

#-------------------------------------------------------------------------------