# TABLE OF CONTENTS
# 0. Prepare environment
# 1. Set up the study
#    1.1 Load the covariance matrices
#    1.2 Set up group names
# 2. Run the analysis on the indicator level
# 3. Run the analysis on the scale level
# 4. Run subgroup analysis
# 5. Save plots
#

# 0. Prepare environment ----
list.of.packages <- c(
  "qgraph",
  "data.table",
  "RColorBrewer",
  "SID",
  "xtable",
  "dplyr",
  "bootnet",
  "rstudioapi"
)
new.packages <-
  list.of.packages[!(list.of.packages %in%
    installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages,
    repos = "http://cran.us.r-project.org"
  )
}


# load packages
require(qgraph)
require(data.table)
require(RColorBrewer)
require(SID)
require(xtable)
require(dplyr)
require(bootnet)
require(rstudioapi)

# get the path to active Rstudio project
current_path <- getActiveDocumentContext()$path
# set wd to active path
setwd(dirname(current_path))
# clean up
rm(list = ls())

# sample size
# hardcode these variables as the data is not open
N <- 568
N_member <- 303
N_nonmember <- 265
# END SECTION




##################### Relationship between items################################

load(file = "./correlationMatrices/ItemCorrelationMatrixFIML.Rdata") # load corItems

# groups items by colour (construct)
groups_items <- list(
  `Altruistic values [1-4]` = c(1:4),
  `Biospheric values [5-8]` = c(5:8),
  `Egoistic values [9-13]` = c(9:13),
  `Hedonic values [14-16]` = c(14:16),
  `Environmental self-identity [17-19]` = c(17:19),
  `Personal importance of sustainable energy behaviour [20-22]` = c(20:22),
  `Need to belong [23]` = c(23),
  `Need to be unique [24]` = c(24),
  `Neighbourhood entitativity [25]` = c(25),
  `Neighbourhood homogeneity [26-27]` = c(26:27),
  `Neighbourhood interaction [28-29]` = c(28:29),
  `Interaction with neighbours [30-31]` = c(30:31),
  `Neighbourhood identification [32-35]` = c(32:35),
  `Environmental neighbourhood identity [36-38]` = c(36:38),
  `Neighbourhood importance of sustainable energy behaviour [39-41]` = c(39:41),
  `Group-based anger [42-43]` = c(42:43),
  `Group-based distrust [44-45]` = c(44:45),
  `Membership [46]` = c(46),
  `Overall energy savings [47]` = c(47),
  `Thermostat temperature (°C) [48]` = c(48),
  `Shower time (min) [49]` = c(49),
  `Energy-efficient appliances [50]` = c(50),
  `Energy-saving measures [51]` = c(51),
  `Household sustainable energy intentions [52-56]` = c(52:56),
  `Communal sustainable energy intentions [57-58]` = c(57:58),
  `Initiative involvement intentions [59]` = c(59),
  `Other pro-environmental intentions [60-62]` = c(60:62),
  `Other commmunal intentions [63-64]` = c(63:64),
  `Demographical variables [65-68]` = c(65:68)
)


# set up color scales
col_item <- rep(brewer.pal(length(groups_items), name = "Set3"), 3)

# display and save indicator graph
# sample size N = 568

glasso_item <-
  qgraph(
    corFIML,
    layout = "spring",
    graph = "glasso",
    labels = TRUE,
    sampleSize = N,
    tuning = 0.5,
    minimum = 0.1,
    groups = groups_items,
    legend.cex = 0.35,
    layoutOffset = c(-.2, 0),
    vsize = 3.0,
    color = col_item,
    filename = "Images/itemGGM",
    filetype = "tiff"
  )

##################### Relationship between scales################################

load(file = "./correlationMatrices/ScaleCorrelationMatrixFIML.Rdata") # load corScales

# groups scales by category
scales_groups <- list(
  `Personal factors` = c(1:8),
  `Factors related to the social context` = c(9:15),
  `Evaluations of energy companies and the government` = c(16:17),
  `Sustainable energy intentions and behaviours` = c(18:27),
  `Socio-demographics` = c(28:31),
  `Membership` <- c(32)
)

col_scales <- brewer.pal(length(scales_groups), name = "Accent")

# sample size = 568
coreConstructsGGM <-
  qgraph(
    corScalesFIML,
    layout = "spring",
    graph = "glasso",
    minimum = 0.1,
    labels = TRUE,
    color = col_scales,
    nodeNames = colnames(corScalesFIML),
    groups = scales_groups,
    legend.mode = "style1",
    tuning = 0.5,
    legend.cex = 0.32,
    vsize = 3.8,
    layoutOffset = c(-.2, 0),
    sampleSize = N,
    filename = "Images/scaleGGM",
    filetype = "tiff"
  )
# dev.off()

#####################   Subgroup analysis  ################################

load(file = "./correlationMatrices/members_ScaleCorrelationMatrixFIML.Rdata") # member
load(file = "./correlationMatrices/non_members_ScaleCorrelationMatrixFIML.Rdata") # non-members

# colors for graphs
subgroup_groups <- list(
  `Personal factors` = c(1:8),
  `Factors related to the social context` = c(9:15),
  `Evaluations of energy companies and the government` = c(16:17),
  `Sustainable energy intentions and behaviours` = c(18:27),
  `Socio-demographics` = c(28:31)
)

# display and save graphs
memberGGM <-
  qgraph(
    cor_memFIML,
    layout = "spring",
    graph = "glasso",
    minimum = 0.1,
    palette = "ggplot2",
    labels = TRUE,
    nodeNames = colnames(cor_memFIML),
    groups = subgroup_groups,
    legend.mode = "style1",
    tuning = 0.5,
    legend.cex = 0.32,
    vsize = 3.8,
    layoutOffset = c(-.2, 0),
    sampleSize = N_member,
    filename = "Images/Scalesmember",
    filetype = "tiff"
  )

Nonmember_GGM <-
  qgraph(
    cor_nonmemFIML,
    layout = memberGGM$layout,
    graph = "glasso",
    minimum = 0.1,
    labels = TRUE,
    nodeNames = colnames(cor_nonmemFIML),
    groups = subgroup_groups,
    legend = FALSE,
    legend.mode = "style1",
    palette = "ggplot2",
    tuning = 0.5,
    legend.cex = 0.32,
    vsize = 3.8,
    sampleSize = N_nonmember,
    filename = "Images/ScalesNonMember",
    filetype = "tiff"
  )


###################     Computer Structural Hamming Distance  #################
# Obtain adjacency matrices for SHD
Wm <- getWmat(memberGGM)
Xm <- 1L * abs(Wm > 0.1)

Wnm <- getWmat(Nonmember_GGM)
Xnm <- 1L * abs(Wnm > 0.1)

# Structural Hamming distance (SHD)
hammDist <- hammingDist(Xm, Xnm)

# print SHD
hammDist

####################    Stability Analysis ####################################
# non-parametric due to ordinal variables
# note that bootnet requires the original dataset
# dataset available on request

boot <- bootnet(
  Scales,
  default = "EBICglasso",
  statistics = c("edge", "strength"),
  computeCentrality = FALSE,
  nCores = 4
)

# print summary
summary(boot)


# Plot bootstrapped edge CIs:
pdf("Images/EdgeBootstrap.pdf",
  width = 11.2,
  height = 8
)
plot(boot, plot = "interval", labels = FALSE)
dev.off()
