# ---------------------------------------------------------------------------- #
# Run Exploratory Factor Analysis
# Original code by: Jeremy W. Eberle
# Minor modifications by: Dandan Tang
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("~/code/1a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

pkgs <- c("psych", "lavaan")
groundhog.library(pkgs, groundhog_day)

# Set seed

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Define functions used in script ----
# ---------------------------------------------------------------------------- #

# Define function to export basic EFA results and details to TXT and loadings to CSV

export_efa_res <- function(fit, path, filename_stem) {
  sink(paste0(path, paste0(filename_stem, ".txt")))
  print(summary(fit))
  sink()
  
  sink(paste0(path, paste0(filename_stem, "_detail.txt")))
  print(summary(fit, se = TRUE, zstat = TRUE, pvalue = TRUE))
  sink()
  
  sink(paste0(path, paste0(filename_stem, ".csv")))
  print(fit$loadings)
  sink()
}

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("~/data/further_clean/mdib_hd_dat2.RData")
load("~/data/helper/mdib_dat_items.RData")
load("~/data/helper/mdib_item_map.RData")

# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

# Restrict to baseline and MDIB columns

mdib_items <- c(mdib_dat_items$mdib_ben, mdib_dat_items$mdib_neg)

mdib_bl <- mdib_hd_dat2[mdib_hd_dat2$redcap_event_name == "baseline_arm_1", mdib_items]

# Order columns by meaning and then domain

mdib_item_map <- mdib_item_map[order(mdib_item_map$meaning, mdib_item_map$domain), ]

mdib_bl <- mdib_bl[match(mdib_item_map$items_rename, names(mdib_bl))]

# ---------------------------------------------------------------------------- #
# Inspect item distributions ----
# ---------------------------------------------------------------------------- #

# Define function to plot histograms for six items at a time

plot_item_hists <- function(df, cols) {
  par(mfrow = c(3, 2))
  
  for (i in cols) {
    col_name <- names(df[i])
    hist(df[, i], main = col_name, xlab = "")
  }
}

# Run function and export plots to PDF

efa_path   <- "~/results/efa/"

hists_path <- paste0(efa_path, "hists/")

dir.create(hists_path, recursive = TRUE)

pdf(paste0(hists_path, "mdib_bl_hists.pdf"), height = 6, width = 6)
plot_item_hists(mdib_bl, 1:6)
plot_item_hists(mdib_bl, 7:12)
plot_item_hists(mdib_bl, 13:18)
plot_item_hists(mdib_bl, 19:24)
plot_item_hists(mdib_bl, 25:30)
plot_item_hists(mdib_bl, 31:36)
dev.off()

# Note: Some item distributions are heavily skewed, especially for negative items.
# Thus, use WLSMV estimation in addition to ML estimation with Sattora-Bentler
# scaling (see Finney & DiStefano, 2013, p. 476; Rosellini & Brown, 2021, p. 64).

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on all items ----
# ---------------------------------------------------------------------------- #

# impuate by mean
df <- mdib_bl
num_cols <- sapply(df, is.numeric)
for (nm in names(df)[num_cols]) {
  m <- mean(df[[nm]], na.rm = TRUE)
  if (is.nan(m)) next  # 或者 m <- 0
  df[[nm]][is.na(df[[nm]])] <- m
}


# Obtain eigenvalues of correlation matrix

#eigen(cor(mdib_bl))$values
eigen(cor(df))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

all_items_path <- paste0(efa_path,       "all_items/")
scree_path     <- paste0(all_items_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl, factors = FALSE, pc = TRUE)
dev.off()
# three to four

# Given this, also consider parallel analysis, which suggests an upper bound (see 
# Montoya & Edwards, 2021, p. 416) of 4 factors (based on principal axis factoring;
# "PA-PAF-m" in Lim & Jahng, 2019) or 4 components (based on principal component 
# analysis; "PA-PCA-m"). Lim and Jahng (2019) found that PA-PCA-m is better across 
# a wide variety of situations (inc. ordinal data) and recommend that. (Note that 
# "ncomp" suggests 2 components, whereas the eigenvalues of 4 components actually 
# exceed the mean. William Revelle confirmed via email on 7/13/2023 that "fa.parallel"
# defaults to a "quant" argument of .95, meaning the 95% CI is used as the threshold 
# rather than the mean, in contrast to the documentation, which needs to be fixed.)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
#(result_ml <- fa.parallel(mdib_bl, fm = "ml", n.iter = 100))
(result_ml <- fa.parallel(df, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 2

sum(result_ml$pc.values > result_ml$pc.sim)  == 2
sum(result_ml$pc.values > result_ml$pc.simr) == 2

# Also do parallel analysis of polychoric correlations (given some heavily skewed 
# items--see above). Per personal correspondence on 9/17/2023, William Revelle 
# suggested setting "correct = 0" to avoid error/warnings when setting 'cor = "poly"'
# (though some warnings remain). Unclear what estimation method to use for polychoric 
# correlations, so try both "ML" (as above, but with only 10 iterations as it takes
# long to run) and "minres" (default, incl. for "fa.parallel.poly()"). Results same.

# Two kinds of warnings for "ML":
# 1: In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
# 2: In polychoric(sampledata, correct = correct) :
#    The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
#(result_poly_ml <- fa.parallel(mdib_bl, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
(result_poly_ml <- fa.parallel(df, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 3

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 3
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 2
# 2 

# Note for "minres":
# In smc, smcs < 0 were set to .0

# Three kinds of warnings for "minres":
# 1: In cor.smooth(mat) : Matrix was not positive definite, smoothing was done
# 2: In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#    The estimated weights for the factor scores are probably incorrect. Try a different 
#    factor score estimation method.
# 3: In polychoric(sampledata, correct = correct) :
#    The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
#(result_poly_minres <- fa.parallel(mdib_bl, correct = 0, cor = "poly"))
(result_poly_minres <- fa.parallel(df, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 3

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 3
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 4

# ---------------------------------------------------------------------------- #
# Run EFA based on all items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 2 to 9 factors, but parallel analysis 
# suggests a smaller number (up to 4, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 2, 3, 4, or 5.


# Note: Random seed must be set for each analysis for reproducible results


# No Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = df, nfactors = 2:5, rotation = "oblimin", 
                       estimator = "MLM", check.vcov = FALSE)
set.seed(1234)
fit_geomin_mlm  <- efa(data = df, nfactors = 2:5, rotation = "geomin",  
                       estimator = "MLM", check.vcov = FALSE)
set.seed(1234)
fit_promax_mlm  <- efa(data = df, nfactors = 2:5, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, all_items_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  all_items_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  all_items_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on all items using WLSMV estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

df_ord <- as.data.frame(lapply(df, factor, levels = 0:4, ordered = TRUE))

# For "mdib_ben_int_email_6a", which no participant rated 0, remove the level 0

# compute the max and min values for each column
num_cols <- sapply(df, is.numeric)
res <- sapply(df[ , num_cols, drop = FALSE], range, na.rm = TRUE)
res
# mdib_ben_int_remember_1c (1,4); mdib_neg_ext_server_2a (0,3)

df_ord$mdib_ben_int_remember_1c <- droplevels(df_ord$mdib_ben_int_remember_1c)
levels(df_ord$mdib_ben_int_remember_1c)[levels(df_ord$mdib_ben_int_remember_1c) == 1] <- "0 or 1"

df_ord$mdib_neg_ext_server_2a <- droplevels(df_ord$mdib_neg_ext_server_2a)
levels(df_ord$mdib_neg_ext_server_2a)[levels(df_ord$mdib_neg_ext_server_2a) == 3] <- "3 or 4"


#mdib_bl_ord$mdib_ben_int_email_6a <- droplevels(mdib_bl_ord$mdib_ben_int_email_6a)


#levels(mdib_bl_ord$mdib_ben_int_email_6a)[levels(mdib_bl_ord$mdib_ben_int_email_6a) == 1] <- "0 or 1"

# Consider 2, 3, 4, or 5 factors, as determined above

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = df_ord, nfactors = 2:5, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = df_ord, nfactors = 2:5, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = df_ord, nfactors = 2:5, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, all_items_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  all_items_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  all_items_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on only theorized negative bias items ----
# ---------------------------------------------------------------------------- #

# Restrict to 12 theorized negative bias items

#mdib_bl_neg <- mdib_bl[, names(mdib_bl[grepl("mdib_neg", names(mdib_bl))])]

mdib_bl_neg <- df[, names(df[grepl("mdib_neg", names(mdib_bl))])]

length(mdib_bl_neg) == 12

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_path <- paste0(efa_path,       "neg_items/")
scree_path     <- paste0(neg_items_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# Same two kinds of warnings as above for "ML"

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Same note and three kinds of warnings as above for "minres". And a fourth kind of warning:
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on only negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
#Warning message:
#lavaan->lav_model_vcov():  
#  The variance-covariance matrix of the estimated parameters (vcov) does not appear to be positive definite! The smallest eigenvalue (= 
#                                                                                                                                        -5.507238e-32) is smaller than zero. This may be a symptom that the model is not identified. 

set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
#Warning message:
#lavaan->lav_model_vcov():  
#  The variance-covariance matrix of the estimated parameters (vcov) does not appear to be positive definite! The smallest eigenvalue (= 
 #                                                                                                                                       -9.212384e-32) is smaller than zero. This may be a symptom that the model is not identified. 

set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on only negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_ord <- as.data.frame(lapply(mdib_bl_neg, factor, levels = 0:4, ordered = TRUE))

mdib_bl_neg_ord$mdib_neg_ext_server_2a <- droplevels(mdib_bl_neg_ord$mdib_neg_ext_server_2a)
levels(mdib_bl_neg_ord$mdib_neg_ext_server_2a)[levels(mdib_bl_neg_ord$mdib_neg_ext_server_2a) == 3] <- "3 or 4"

# the same reason

# Consider 1 or 2 factors, as determined above

# Note: No Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 11 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# Dandan: From 12 theorized negative bias items, consider excluding two items.
# "mdib_neg_int_remember_1b" that are theorized to be internal items but that loaded with external items.
# "mdib_neg_ext_server_2a" is theorized to be external items but that loaded with internal items. Start by
# excluding "mdib_neg_ext_server_2a", which loaded more strongly than "mdib_neg_int_remember_1b"

# From 12 theorized negative bias items, consider excluding two items that are
# theorized to be external items but that loaded with internal items. Start by
# excluding "mdib_neg_ext_walk_9c", which loaded more strongly with internal
# items than "mdib_neg_ext_server_2a"

mdib_bl_neg_11 <- mdib_bl_neg[, names(mdib_bl_neg)[names(mdib_bl_neg) != 
                                                     "mdib_neg_ext_server_2a"]]

length(mdib_bl_neg_11) == 11

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_11))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_11_path <- paste0(efa_path, "neg_items_11/")
scree_path        <- paste0(neg_items_11_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_11, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_11, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_11, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_11, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 11 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_11, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_11_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_11_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_11_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 11 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_11_ord <- as.data.frame(lapply(mdib_bl_neg_11, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV", check.vcov = FALSE)
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_11_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_11_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_11_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_11_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 10 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# Now exclude "mdib_neg_int_remember_1b"

mdib_bl_neg_10 <- mdib_bl_neg_11[, names(mdib_bl_neg_11)[names(mdib_bl_neg_11) !=
                                                           "mdib_neg_int_remember_1b"]]

length(mdib_bl_neg_10) == 10

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_10))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_10_path <- paste0(efa_path,       "neg_items_10/")
scree_path        <- paste0(neg_items_10_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_10, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 1 
# factor or 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_10, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning as above for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_10, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_10, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1
sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 10 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_10, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_10_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_10_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_10_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 10 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_10_ord <- as.data.frame(lapply(mdib_bl_neg_10, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV")
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV")
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_10_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_10_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_10_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_10_path, "promax_wlsmv")

# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 9 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# From 10 theorized negative bias items, consider excluding item theorized to be 
# an internal item ("mdib_neg_int_email_6b") but that has moderate cross-loading
# with external items-the same thing happened.

mdib_bl_neg_9 <- mdib_bl_neg_10[, names(mdib_bl_neg_10)[names(mdib_bl_neg_10) !=
                                                          "mdib_neg_int_email_6b"]]
describe(mdib_bl_neg_9[,1:6])
mdib_bl_neg_9[,1:6]

# mean and sd for external 


length(mdib_bl_neg_9) == 9

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_9))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_9_path <- paste0(efa_path,         "neg_items_09/")
scree_path       <- paste0(neg_items_9_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_9, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 
# 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_9, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning as above for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_9, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_9, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 9 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_9, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_9_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_9_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_9_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 9 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_9_ord <- as.data.frame(lapply(mdib_bl_neg_9, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV")
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV")
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_9_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_9_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_9_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_9_path, "promax_wlsmv")


# ---------------------------------------------------------------------------- #
# Inspect scree plot based on 8 reduced negative bias items ----
# ---------------------------------------------------------------------------- #

# From 9 theorized negative bias items, consider excluding item theorized to be 
# an external item ("mdib_neg_ext_exercise_7a") but that has moderate cross-loading
# with internal items-the same thing happened.

mdib_bl_neg_8 <- mdib_bl_neg_9[, names(mdib_bl_neg_9)[names(mdib_bl_neg_9) !=
                                                          "mdib_neg_ext_exercise_7a"]]

length(mdib_bl_neg_8) == 8

# Obtain eigenvalues of correlation matrix

eigen(cor(mdib_bl_neg_8))$values

# Plot eigenvalues as scree plot to help decide how many factors to retain, which
# shows an unclear break point between cliff and scree

neg_items_8_path <- paste0(efa_path,         "neg_items_08/")
scree_path       <- paste0(neg_items_8_path, "scree/")

dir.create(scree_path, recursive = TRUE)

pdf(paste0(scree_path, "mdib_bl_scree.pdf"), height = 6, width = 6)
scree(mdib_bl_neg_8, factors = FALSE, pc = TRUE)
dev.off()

# Given this, also consider parallel analysis, which suggests an upper bound of 
# 1 component (see above)

pdf(paste0(scree_path, "mdib_bl_scree_pa_ml.pdf"), height = 6, width = 6)
(result_ml <- fa.parallel(mdib_bl_neg_8, fm = "ml", n.iter = 100))
dev.off()

result_ml$ncomp == 1

sum(result_ml$pc.values > result_ml$pc.sim) == 1
sum(result_ml$pc.values > result_ml$pc.simr) == 1

# Also do parallel analysis of polychoric correlations (see above)

# One kind of warning as above for "ML":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_ml.pdf"), height = 6, width = 6)
(result_poly_ml <- fa.parallel(mdib_bl_neg_8, fm = "ml", n.iter = 10, correct = 0, cor = "poly"))
dev.off()

result_poly_ml$ncomp == 1

sum(result_poly_ml$pc.values > result_poly_ml$pc.sim)  == 1
sum(result_poly_ml$pc.values > result_poly_ml$pc.simr) == 1

# Similar warnings (three kinds) as above for "minres":
# In polychoric(sampledata, correct = correct) :
#   The items do not have an equal number of response alternatives, global set to FALSE.
# In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  ... :
#   The estimated weights for the factor scores are probably incorrect. Try a different
#   factor score estimation method.
# In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  ... :
#   An ultra-Heywood case was detected.  Examine the results carefully

pdf(paste0(scree_path, "mdib_bl_scree_pa_poly_minres.pdf"), height = 6, width = 6)
(result_poly_minres <- fa.parallel(mdib_bl_neg_8, correct = 0, cor = "poly"))
dev.off()

result_poly_minres$ncomp == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.sim)  == 1

sum(result_poly_minres$pc.values > result_poly_minres$pc.simr) == 1

# ---------------------------------------------------------------------------- #
# Run EFA based on 8 reduced negative bias items using "MLM" estimator ----
# ---------------------------------------------------------------------------- #

# Based on scree plot, considered retaining 1 to 3 factors, but parallel analysis 
# suggests a smaller number (up to 1, though looking at +/- 1 is recommended; Lim 
# & Jahng, 2019). Thus, consider 1 or 2.

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_mlm <- efa(data = mdib_bl_neg_8, nfactors = 1:2, rotation = "oblimin", 
                       estimator = "MLM")
set.seed(1234)
fit_geomin_mlm  <- efa(data = mdib_bl_neg_8, nfactors = 1:2, rotation = "geomin",  
                       estimator = "MLM")
set.seed(1234)
fit_promax_mlm  <- efa(data = mdib_bl_neg_8, nfactors = 1:2, rotation = "promax",  
                       estimator = "MLM")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_mlm, neg_items_8_path, "oblimin_mlm")
export_efa_res(fit_geomin_mlm,  neg_items_8_path, "geomin_mlm")
export_efa_res(fit_promax_mlm,  neg_items_8_path, "promax_mlm")

# ---------------------------------------------------------------------------- #
# Run EFA based on 8 reduced negative bias items using "WLSMV" estimator ----
# ---------------------------------------------------------------------------- #

# Convert columns to ordered factors

mdib_bl_neg_8_ord <- as.data.frame(lapply(mdib_bl_neg_8, factor, levels = 0:4, ordered = TRUE))

# Consider 1 or 2 factors, as determined above

# Note: No warnings or Heywood cases

set.seed(1234)
fit_oblimin_wlsmv <- efa(data = mdib_bl_neg_8_ord, nfactors = 1:2, rotation = "oblimin", 
                         estimator = "WLSMV")
set.seed(1234)
fit_geomin_wlsmv  <- efa(data = mdib_bl_neg_8_ord, nfactors = 1:2, rotation = "geomin",  
                         estimator = "WLSMV")
set.seed(1234)
fit_promax_wlsmv  <- efa(data = mdib_bl_neg_8_ord, nfactors = 1:2, rotation = "promax",  
                         estimator = "WLSMV")

# Export basic results and details to TXT and loadings to CSV

export_efa_res(fit_oblimin_wlsmv, neg_items_8_path, "oblimin_wlsmv")
export_efa_res(fit_geomin_wlsmv,  neg_items_8_path, "geomin_wlsmv")
export_efa_res(fit_promax_wlsmv,  neg_items_8_path, "promax_wlsmv")


# ---------------------------------------------------------------------------- #
# Export 8 reduced negative bias items for internal consistency analyses ----
# ---------------------------------------------------------------------------- #

save(mdib_bl_neg_8,     file = "~/data/further_clean/mdib_bl_neg_8.Rdata")
save(mdib_bl_neg_8_ord, file = "~/data/further_clean/mdib_bl_neg_8_ord.Rdata")
#8-item scale are the best 