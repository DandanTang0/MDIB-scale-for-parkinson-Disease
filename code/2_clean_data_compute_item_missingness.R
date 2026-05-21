# ---------------------------------------------------------------------------- #
# Clean Data
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

source("./1a_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

# Import HD MDIB data from RedCap

mdib_hd_dat <- read.csv("final PD Aim 1 data_deid_2022-12-08_OSF.csv")

# ---------------------------------------------------------------------------- #
# Define time point objects used throughout script ----
# ---------------------------------------------------------------------------- #

bl   <- "baseline_arm_1"
fu   <- "followup_arm_1"
both <- c(bl, fu)

# ---------------------------------------------------------------------------- #
# Remove blank rows ----
# ---------------------------------------------------------------------------- #

# For rows where "record_id" is NA, all columns are NA or "". Remove such rows.
# Note: These no longer appear; perhaps due to update to R (vs. R 4.2.2 used in 
# prior script at https://github.com/jwe4ec/ncats-r03)

sum(is.na(mdib_hd_dat$record_id)) == 0

sum(!(is.na(mdib_hd_dat[is.na(mdib_hd_dat$record_id), ]) |
        mdib_hd_dat[is.na(mdib_hd_dat$record_id), ] == "")) == 0

mdib_hd_dat <- mdib_hd_dat[!is.na(mdib_hd_dat$record_id), ]

# no "record_id" is NA

# ---------------------------------------------------------------------------- #
# Remove participants ----
# ---------------------------------------------------------------------------- #

# 7 participants consented to participate but never started 
# the surveys. These participants have values of NA, 0, or "" for all columns at 
# baseline. Remove these participants.

target_cols <- names(mdib_hd_dat)[!(names(mdib_hd_dat) %in% c("record_id", "redcap_event_name"))]

row_never_started <- vector(length = nrow(mdib_hd_dat))

for (i in 1:nrow(mdib_hd_dat)) {
  row_never_started[[i]] <- mdib_hd_dat$redcap_event_name[[i]] == bl &
    all(apply(mdib_hd_dat[i, target_cols], 2, function(x) is.na(x) | x %in% c(0, "")))
}

nrow(mdib_hd_dat[row_never_started, ]) == 7

mdib_hd_dat <- mdib_hd_dat[!row_never_started, ]


length(unique(mdib_hd_dat$record_id)) == 88

# ---------------------------------------------------------------------------- #
# Identify items for relevant scales ----
# ---------------------------------------------------------------------------- #

# Identify items for negative bias (MDIB, BBSIQ; and benign bias for each just for
# reference), anxiety sensitivity (ASI), fear of negative evaluation (BFNE-II), 
# anxiety symptoms (Neuro-QoL Anxiety), social avoidance and distress (full SADS 
# at baseline and reduced SADS at follow-up), and alcohol use (AUDIT-C)

mdib_neg_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("neg", names(mdib_hd_dat))]
mdib_ben_items <- 
  names(mdib_hd_dat)[grepl("md_bbsiq", names(mdib_hd_dat)) & grepl("benign", names(mdib_hd_dat))]

  # Note: The object for BBSIQ items is appended with "mdib" because prior analyses
  # (https://github.com/jwe4ec/pa-20-206) found that BBSIQ item names differ between
  # the MDIB and MindTrails-HD Data Server datasets. Although the MT-HD dataset is
  # not relevant to the present analyses, we retain the label for clarity.

bbsiq_neg_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("neg", names(mdib_hd_dat))]
bbsiq_ben_items_mdib <- 
  names(mdib_hd_dat)[grepl("bbsiq", names(mdib_hd_dat)) & !grepl("md_bbsiq", names(mdib_hd_dat)) &
                       grepl("benign", names(mdib_hd_dat))]

asi_items <- names(mdib_hd_dat)[grepl("asi_", names(mdib_hd_dat))]

bfne2_items <- names(mdib_hd_dat)[grepl("bfne_", names(mdib_hd_dat))]

neuroqol_anx_items <-
  names(mdib_hd_dat)[grepl("neuroqol", names(mdib_hd_dat)) & !(grepl("complete", names(mdib_hd_dat)))]

sads_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & !(grepl("_v2", names(mdib_hd_dat)))]

sads_red_items <- names(mdib_hd_dat)[grepl("sad_", names(mdib_hd_dat)) & grepl("_v2", names(mdib_hd_dat))]

auditc_items <- names(mdib_hd_dat)[grepl("alcohol_audit_c", names(mdib_hd_dat))]

all(mdib_neg_items == c("md_bbsiq_1b_neg", "md_bbsiq_2a_neg", "md_bbsiq_3c_neg", 
                        "md_bbsiq_4c_neg", "md_bbsiq_5a_neg", "md_bbsiq_6b_neg", 
                        "md_bbsiq_7a_neg", "md_bbsiq_8b_neg", "md_bbsiq_9c_neg", 
                        "md_bbsiq_10a_neg", "md_bbsiq_11c_neg", "md_bbsiq_12b_neg"))

all(mdib_ben_items == c("md_bbsiq_1a_benign", "md_bbsiq_1c_benign", "md_bbsiq_2b_benign", 
                        "md_bbsiq_2c_benign", "md_bbsiq_3a_benign", "md_bbsiq_3b_benign", 
                        "md_bbsiq_4a_benign", "md_bbsiq_4b_benign", "md_bbsiq_5b_benign", 
                        "md_bbsiq_5c_benign", "md_bbsiq_6a_benign", "md_bbsiq_6c_benign", 
                        "md_bbsiq_7b_benign", "md_bbsiq_7c_benign", "md_bbsiq_8a_benign", 
                        "md_bbsiq_8c_benign", "md_bbsiq_9a_benign", "md_bbsiq_9b_benign", 
                        "md_bbsiq_10b_benign", "md_bbsiq_10c_benign", "md_bbsiq_11a_benign", 
                        "md_bbsiq_11b_benign", "md_bbsiq_12a_benign", "md_bbsiq_12c_benign"))

all(bbsiq_neg_items_mdib == c("bbsiq_1c_neg", "bbsiq_2b_neg", "bbsiq_3c_neg", 
                              "bbsiq_4c_neg", "bbsiq_5a_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_8c_neg", "bbsiq_9b_neg", 
                              "bbsiq_10b_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_13c_neg", "bbsiq_14c_neg"))

all(bbsiq_ben_items_mdib == c("bbsiq_1a_benign", "bbsiq_1b_benign", "bbsiq_2a_benign", 
                              "bbsiq_2c_benign", "bbsiq_3a_benign", "bbsiq_3b_benign", 
                              "bbsiq_4a_benign", "bbsiq_4b_benign", "bbsiq_5b_benign", 
                              "bbsiq_5c_benign", "bbsiq_6b_benign", "bbsiq_6c_benign", 
                              "bbsiq_7a_benign", "bbsiq_7c_benign", "bbsiq_8a_benign", 
                              "bbsiq_8b_benign", "bbsiq_9a_benign", "bbsiq_9c_benign", 
                              "bbsiq_10a_benign", "bbsiq_10c_benign", "bbsiq_11a_benign", 
                              "bbsiq_11c_benign", "bbsiq_12b_benign", "bbsiq_12c_benign", 
                              "bbsiq_13a_benign", "bbsiq_13b_benign", "bbsiq_14a_benign", 
                              "bbsiq_14b_benign"))

all(asi_items == c("asi_1", "asi_2", "asi_3", "asi_4", "asi_5", "asi_6", "asi_7", 
                   "asi_8", "asi_9", "asi_10", "asi_11", "asi_12", "asi_13", "asi_14", 
                   "asi_15", "asi_16"))

all(bfne2_items == c("bfne_1", "bfne_2", "bfne_3", "bfne_4", "bfne_5", "bfne_6", 
                     "bfne_7", "bfne_8", "bfne_9", "bfne_10", "bfne_11", "bfne_12"))

all(neuroqol_anx_items == c("neuroqol_edanx53", "neuroqol_edanx46", "neuroqol_edanx48", 
                            "neuroqol_edanx41", "neuroqol_edanx54", "neuroqol_edanx55", 
                            "neuroqol_edanx18", "neuroqol_nqanx07"))

all(sads_items == c("sad_1", "sad_2", "sad_3", "sad_4", "sad_5", "sad_6", "sad_7", 
                    "sad_8", "sad_9", "sad_10", "sad_11", "sad_12", "sad_13", "sad_14", 
                    "sad_15", "sad_16", "sad_17", "sad_18", "sad_19", "sad_20", "sad_21", 
                    "sad_22", "sad_23", "sad_24", "sad_25", "sad_26", "sad_27", "sad_28"))

all(sads_red_items == c("sad_20_v2", "sad_27_v2", "sad_13_v2", "sad_12_v2", 
                        "sad_24_v2", "sad_15_v2", "sad_4_v2", "sad_16_v2"))

all(auditc_items == c("alcohol_audit_c_1", "alcohol_audit_c_2", "alcohol_audit_c_3"))

length(mdib_neg_items) == 12
length(mdib_ben_items) == 24
length(bbsiq_neg_items_mdib) == 14
length(bbsiq_ben_items_mdib) == 28
length(asi_items) == 16
length(bfne2_items) == 12
length(neuroqol_anx_items) == 8
length(sads_items) == 28
length(sads_red_items) == 8
length(auditc_items) == 3

# ---------------------------------------------------------------------------- #
# Rename MDIB items ----
# ---------------------------------------------------------------------------- #

# Define names to reflect benign/negative, internal/external threat, scenario, and item number

mdib_items <- c(mdib_ben_items, mdib_neg_items)

mdib_items_rename <-
  c("mdib_ben_int_remember_1a", "mdib_ben_int_remember_1c", "mdib_ben_ext_server_2b",     "mdib_ben_ext_server_2c",
    "mdib_ben_ext_reminder_3a", "mdib_ben_ext_reminder_3b", "mdib_ben_int_cleaning_4a",   "mdib_ben_int_cleaning_4b",
    "mdib_ben_ext_neighbor_5b", "mdib_ben_ext_neighbor_5c", "mdib_ben_int_email_6a",      "mdib_ben_int_email_6c",
    "mdib_ben_ext_exercise_7b", "mdib_ben_ext_exercise_7c", "mdib_ben_int_medication_8a", "mdib_ben_int_medication_8c",
    "mdib_ben_ext_walk_9a",     "mdib_ben_ext_walk_9b",     "mdib_ben_ext_job_10b",       "mdib_ben_ext_job_10c",
    "mdib_ben_ext_stumble_11a", "mdib_ben_ext_stumble_11b", "mdib_ben_int_cough_12a",     "mdib_ben_int_cough_12c",
    "mdib_neg_int_remember_1b", "mdib_neg_ext_server_2a",   "mdib_neg_ext_reminder_3c",   "mdib_neg_int_cleaning_4c",
    "mdib_neg_ext_neighbor_5a", "mdib_neg_int_email_6b",    "mdib_neg_ext_exercise_7a",   "mdib_neg_int_medication_8b",
    "mdib_neg_ext_walk_9c",     "mdib_neg_ext_job_10a",     "mdib_neg_ext_stumble_11c",   "mdib_neg_int_cough_12b")

item_number <- sub(".*_(\\d+[a-z])$", "\\1", mdib_items_rename) # Get item number
item_number <- sub("^([0-9])([a-z])$", "0\\1\\2", item_number)  # Pad 1-digit numbers with leading 0

meaning <-
  c("ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "ben", "ben", "ben", "ben",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg",
    "neg", "neg", "neg", "neg")

domain <-
  c("int", "int", "ext", "ext",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "int", "int",
    "ext", "ext", "ext", "ext",
    "ext", "ext", "int", "int",
    "int", "ext", "ext", "int",
    "ext", "int", "ext", "int",
    "ext", "ext", "ext", "int")

mdib_item_map <- data.frame(items        = mdib_items,
                            items_rename = mdib_items_rename,
                            item_number  = item_number,
                            meaning      = meaning,
                            domain       = domain)

# Rename items

names(mdib_hd_dat)[names(mdib_hd_dat) %in% mdib_items] <- 
  mdib_items_rename[match(names(mdib_hd_dat)[names(mdib_hd_dat) %in% mdib_items], mdib_items)]

mdib_neg_items <- mdib_item_map$items_rename[mdib_item_map$meaning == "neg"]
mdib_ben_items <- mdib_item_map$items_rename[mdib_item_map$meaning == "ben"]

length(mdib_neg_items) == 12
length(mdib_ben_items) == 24

# ---------------------------------------------------------------------------- #
# Define scale items ----
# ---------------------------------------------------------------------------- #

# Define items for purported MDIB scales (internal threats = catastrophizing 
# about disease progression, external threats = negative social evaluation)

mdib_neg_int_items <- mdib_item_map$items_rename[mdib_item_map$meaning == "neg" &
                                                   mdib_item_map$domain == "int"]
mdib_neg_ext_items <- mdib_item_map$items_rename[mdib_item_map$meaning == "neg" &
                                                   mdib_item_map$domain == "ext"]

all(mdib_neg_int_items == c("mdib_neg_int_remember_1b", "mdib_neg_int_cleaning_4c", 
                            "mdib_neg_int_email_6b", "mdib_neg_int_medication_8b", 
                            "mdib_neg_int_cough_12b"))
all(mdib_neg_ext_items == c("mdib_neg_ext_server_2a", "mdib_neg_ext_reminder_3c", 
                            "mdib_neg_ext_neighbor_5a", "mdib_neg_ext_exercise_7a", 
                            "mdib_neg_ext_walk_9c", "mdib_neg_ext_job_10a", 
                            "mdib_neg_ext_stumble_11c"))

length(mdib_neg_int_items) == 5
length(mdib_neg_ext_items) == 7

# Define items for 9 reduced negative bias items (internal threats, external threats)
# retained after EFA (see "run_efa.R")

mdib_neg_9_int_items <- c("mdib_neg_int_cleaning_4c", 
                          "mdib_neg_int_medication_8b", "mdib_neg_int_cough_12b")
#mdib_neg_9_int_items <- c("mdib_neg_int_remember_1b", "mdib_neg_int_cleaning_4c", 
#                          "mdib_neg_int_medication_8b", "mdib_neg_int_cough_12b")


mdib_neg_9_ext_items <- c("mdib_neg_ext_reminder_3c", "mdib_neg_ext_neighbor_5a", 
                          "mdib_neg_ext_exercise_7a", "mdib_neg_ext_walk_9c", "mdib_neg_ext_job_10a", 
                          "mdib_neg_ext_stumble_11c")

length(mdib_neg_9_int_items) == 3
length(mdib_neg_9_ext_items) == 6

# Define items for purported BBSIQ scales (internal threats, external threats)

bbsiq_neg_int_items_mdib <- c("bbsiq_2b_neg", "bbsiq_3c_neg", "bbsiq_5a_neg", 
                              "bbsiq_8c_neg", "bbsiq_11b_neg", "bbsiq_12a_neg", 
                              "bbsiq_14c_neg")
bbsiq_neg_ext_items_mdib <- c("bbsiq_1c_neg", "bbsiq_4c_neg", "bbsiq_6a_neg", 
                              "bbsiq_7b_neg", "bbsiq_9b_neg", "bbsiq_10b_neg", 
                              "bbsiq_13c_neg")

length(bbsiq_neg_int_items_mdib) == 7
length(bbsiq_neg_ext_items_mdib) == 7

# Define items for ASI subscales (physical, cognitive, and social concerns) based
# on three-factor solutions described in Taylor et al. (1996), Taylor (1996), and
# Taylor et al. (2007). Given unstable factor structure of original ASI, which was
# not designed to be multidimensional, analyze ASI in two ways: (a) using all items
# and (b) using reduced set of two items per scale, considering those that have the 
# clearest face validity with physical, cognitive, and social concerns; have clear 
# factor loadings in Taylor et al. (1996) and Taylor (1996); and are retained in 
# Taylor et al. (2007)'s ASI-3 (see "asi_scoring_decisions.xlsx" for details).

asi_red_phy_items <- c("asi_11", "asi_9")
asi_red_cog_items <- c("asi_12", "asi_2")
asi_red_soc_items <- c("asi_1", "asi_13")

length(asi_red_phy_items) == 2
length(asi_red_cog_items) == 2
length(asi_red_soc_items) == 2

# Define items for 8-item BFNE-II, which is preferred (see Carleton et al., 2007; 
# https://doi.org/bgn7v6)

bfne2_8_items <- bfne2_items[!(bfne2_items %in% c("bfne_2", "bfne_4", 
                                                  "bfne_7", "bfne_11"))]

all(bfne2_8_items == c("bfne_1", "bfne_3", "bfne_5", "bfne_6", "bfne_8", "bfne_9", 
                       "bfne_10", "bfne_12"))

length(bfne2_8_items) == 8

# Store items in list

mdib_dat_items <- list(mdib_neg       = mdib_neg_items,
                       mdib_neg_int   = mdib_neg_int_items,
                       mdib_neg_ext   = mdib_neg_ext_items,
                       mdib_neg_9_int = mdib_neg_9_int_items,
                       mdib_neg_9_ext = mdib_neg_9_ext_items,
                       mdib_ben       = mdib_ben_items,
                       bbsiq_neg      = bbsiq_neg_items_mdib,
                       bbsiq_neg_int  = bbsiq_neg_int_items_mdib,
                       bbsiq_neg_ext  = bbsiq_neg_ext_items_mdib,
                       bbsiq_ben      = bbsiq_ben_items_mdib,
                       asi            = asi_items,
                       asi_red_phy    = asi_red_phy_items,
                       asi_red_cog    = asi_red_cog_items,
                       asi_red_soc    = asi_red_soc_items,
                       bfne2          = bfne2_items,
                       bfne2_8        = bfne2_8_items,
                       neuroqol_anx   = neuroqol_anx_items,
                       sads           = sads_items,
                       sads_red       = sads_red_items,
                       auditc         = auditc_items)

# ---------------------------------------------------------------------------- #
# Inspect NA values ----
# ---------------------------------------------------------------------------- #

# No MDIB, BBSIQ, or Neuro-QoL Anxiety items are already NA at baseline or follow-up

sum(is.na(mdib_hd_dat[, c(mdib_dat_items$mdib_neg, mdib_dat_items$mdib_ben,
                          mdib_dat_items$bbsiq_neg, mdib_dat_items$bbsiq_ben,
                          mdib_dat_items$neuroqol_anx)])) == 0

# No ASI, BFNE-2, or SADS items are already NA at baseline

sum(is.na(mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl, 
                      c(mdib_dat_items$asi,
                        mdib_dat_items$bfne2,
                        mdib_dat_items$sads)])) == 0

# Some AUDIT-C items are already NA at baseline (reasons below)

auditc_bl <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl,
                         c("record_id", "alcohol_ever", mdib_dat_items$auditc)]

n_obs_na_auditc <- sum(is.na(auditc_bl[, mdib_dat_items$auditc]))
#n_obs_na_auditc == 60
n_obs_na_auditc == 85

  # 1. REDCap skipped all 3 items (recode NA as 0 later in script) if "alcohol_ever" 
  # was 0 (not had alcohol in life)

 #sum(is.na(auditc_bl[auditc_bl$alcohol_ever == 0, mdib_dat_items$auditc])) == 36
 # 17 participates never 
sum(is.na(auditc_bl[auditc_bl$alcohol_ever == 0, mdib_dat_items$auditc])) == 51

  # 2. REDCap skipped Items 2-3 (recode NA as 0 later in script) when Item 1 was 0 ("never")

 # sum(is.na(auditc_bl[!is.na(auditc_bl$alcohol_audit_c_1) & auditc_bl$alcohol_audit_c_1 == 0,
 # c("alcohol_audit_c_2", "alcohol_audit_c_3")])) == 24
sum(is.na(auditc_bl[!is.na(auditc_bl$alcohol_audit_c_1) & auditc_bl$alcohol_audit_c_1 == 0,
                    c("alcohol_audit_c_2", "alcohol_audit_c_3")])) == 34

# Some reduced SADS items are already NA at follow-up (reason below)

sads_red_fu <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == fu, 
                           c("record_id", mdib_dat_items$sads_red)]

n_obs_na_sads_red <- sum(is.na(sads_red_fu[, mdib_dat_items$sads_red]))
n_obs_na_sads_red == 40

  # Was not administered to 5 participants

rows_all_items_na_sads_red <- rowSums(!is.na(sads_red_fu[, mdib_dat_items$sads_red])) == 0
n_rows_all_items_na_sads_red <- sum(rows_all_items_na_sads_red)
n_rows_all_items_na_sads_red == 5

n_rows_all_items_na_sads_red * length(mdib_dat_items$sads_red) == n_obs_na_sads_red

# ---------------------------------------------------------------------------- #
# Recode AUDIT-C items ----
# ---------------------------------------------------------------------------- #

# Given that REDCap skipped Items 2-3 (not applicable) when Item 1 was 0 ("never";
# see above), recode Items 2-3 as 0 when Item 1 is 0

mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl &
              !is.na(mdib_hd_dat$alcohol_audit_c_1) & mdib_hd_dat$alcohol_audit_c_1 == 0,
            c("alcohol_audit_c_2", "alcohol_audit_c_3")] <- 0

# Given that REDCap skipped all 3 items (not applicable) if "alcohol_ever" was 0 
# (not had alcohol in life), recode Items 1-3 as 0 when "alcohol_ever" is 0

mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl &
              !is.na(mdib_hd_dat$alcohol_ever) & mdib_hd_dat$alcohol_ever == 0,
            c("alcohol_audit_c_1", "alcohol_audit_c_2", "alcohol_audit_c_3")] <- 0

# ---------------------------------------------------------------------------- #
# Identify participants with incomplete MDIB data at baseline   ----
# ---------------------------------------------------------------------------- #

# Identify such participants (because no MDIB items are already NA [see above], 
# identify those that are PNA [coded as 99]) but do not remove them from overall 
# dataset until end of this script

mdib_items <- c(mdib_dat_items$mdib_neg, mdib_dat_items$mdib_ben)

incompl_mdib_bl_data_ids <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl &
                                          rowSums(mdib_hd_dat[, mdib_items] == 99) > 0, "record_id"]

unique_ids <- unique(na.omit(incompl_mdib_bl_data_ids))
length(unique_ids) ==3


#length(incompl_mdib_bl_data_ids) == 6

# ---------------------------------------------------------------------------- #
# Compute scale-level missing data rates due to "prefer not to answer" for all items ----
# ---------------------------------------------------------------------------- #

# Remove participants with incomplete MDIB data at baseline for this computation,
# which needs to be done before "prefer not to answer" values are recoded as NA

temp_dat <- mdib_hd_dat[!(mdib_hd_dat$record_id %in% incompl_mdib_bl_data_ids), ]

# Define function to compute number of scale scores across specified time points
# that will be missing due to "prefer not to answer" (coded as 99) for all items

compute_all_item_missingness <- function(dat, scale, items, time_points) {
  dat <- dat[dat$redcap_event_name %in% time_points, ]
  
  pna <- 99
  
  rows_all_items_pna <- rowSums(dat[, items] == pna, na.rm = TRUE) == length(items)
  n_rows_all_items_pna <- sum(rows_all_items_pna)
  
  cat(scale, ": ", n_rows_all_items_pna, "\n", sep = "")
  
  print(table(dat[rows_all_items_pna, "redcap_event_name"]))
  cat("\n", "-----", "\n\n")
}

# Run function and write results

missing_rates_path <- "~/code/"
dir.create(missing_rates_path)

sink(file = paste0(missing_rates_path, "all_item_missingness.txt"))

cat("Number of Scale Scores Missing Due to 'Prefer Not to Answer' for All Items:", "\n\n")

compute_all_item_missingness(temp_dat, "mdib_neg_9_int_m" , mdib_dat_items$mdib_neg_9_int, both)
compute_all_item_missingness(temp_dat, "mdib_neg_9_ext_m" , mdib_dat_items$mdib_neg_9_ext, both)
compute_all_item_missingness(temp_dat, "bbsiq_neg_int_m"  , mdib_dat_items$bbsiq_neg_int,  both)
compute_all_item_missingness(temp_dat, "bbsiq_neg_ext_m"  , mdib_dat_items$bbsiq_neg_ext,  both)
compute_all_item_missingness(temp_dat, "asi_m"            , mdib_dat_items$asi,            bl)
compute_all_item_missingness(temp_dat, "asi_red_phy_m"    , mdib_dat_items$asi_red_phy,    bl)
compute_all_item_missingness(temp_dat, "asi_red_cog_m"    , mdib_dat_items$asi_red_cog,    bl)
compute_all_item_missingness(temp_dat, "asi_red_soc_m"    , mdib_dat_items$asi_red_soc,    bl)
compute_all_item_missingness(temp_dat, "bfne2_8_m"        , mdib_dat_items$bfne2_8,        bl)
compute_all_item_missingness(temp_dat, "neuroqol_anx_m"   , mdib_dat_items$neuroqol_anx,   both)
compute_all_item_missingness(temp_dat, "sads_m"           , mdib_dat_items$sads,           bl)
compute_all_item_missingness(temp_dat, "sads_red_m"       , mdib_dat_items$sads_red,       fu)
compute_all_item_missingness(temp_dat, "auditc_m"         , mdib_dat_items$auditc,         bl)

sink()

# ---------------------------------------------------------------------------- #
# Recode "prefer not to answer" values ----
# ---------------------------------------------------------------------------- #

# Recode "prefer not to answer" (coded as 99) as NA

target_items <- c(mdib_dat_items$mdib_neg,
                  mdib_dat_items$mdib_ben,
                  mdib_dat_items$bbsiq_neg,
                  mdib_dat_items$bbsiq_ben,
                  mdib_dat_items$asi,
                  mdib_dat_items$bfne2,
                  mdib_dat_items$neuroqol_anx,
                  mdib_dat_items$sads,
                  mdib_dat_items$sads_red,
                  mdib_dat_items$auditc)

mdib_hd_dat[, target_items][mdib_hd_dat[, target_items] == 99] <- NA

# ---------------------------------------------------------------------------- #
# Recode BFNE-II ----
# ---------------------------------------------------------------------------- #

# Given that Carleton et al. (2007) uses a 0-4 scale, recode our 1-5 scale to a
# 0-4 scale (response options on REDCap did not have numbers listed next to them)

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(1, 5))

mdib_hd_dat[, mdib_dat_items$bfne2] <- mdib_hd_dat[, mdib_dat_items$bfne2] - 1

all(range(mdib_hd_dat[, mdib_dat_items$bfne2], na.rm = TRUE) == c(0, 4))

# ---------------------------------------------------------------------------- #
# Compute scores ----
# ---------------------------------------------------------------------------- #

# Compute mean of available items. For MDIB, use 9 reduced negative items retained 
# from EFA (see "run_efa.R")

mdib_hd_dat$mdib_neg_9_int_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_9_int], na.rm = TRUE)
mdib_hd_dat$mdib_neg_9_ext_m <- rowMeans(mdib_hd_dat[, mdib_dat_items$mdib_neg_9_ext], na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_int_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_int],  na.rm = TRUE)
mdib_hd_dat$bbsiq_neg_ext_m  <- rowMeans(mdib_hd_dat[, mdib_dat_items$bbsiq_neg_ext],  na.rm = TRUE)
mdib_hd_dat$asi_m            <- rowMeans(mdib_hd_dat[, mdib_dat_items$asi],            na.rm = TRUE)
mdib_hd_dat$asi_red_phy_m    <- rowMeans(mdib_hd_dat[, mdib_dat_items$asi_red_phy],    na.rm = TRUE)
mdib_hd_dat$asi_red_cog_m    <- rowMeans(mdib_hd_dat[, mdib_dat_items$asi_red_cog],    na.rm = TRUE)
mdib_hd_dat$asi_red_soc_m    <- rowMeans(mdib_hd_dat[, mdib_dat_items$asi_red_soc],    na.rm = TRUE)
mdib_hd_dat$bfne2_8_m        <- rowMeans(mdib_hd_dat[, mdib_dat_items$bfne2_8],        na.rm = TRUE)
mdib_hd_dat$neuroqol_anx_m   <- rowMeans(mdib_hd_dat[, mdib_dat_items$neuroqol_anx],   na.rm = TRUE)
mdib_hd_dat$sads_m           <- rowMeans(mdib_hd_dat[, mdib_dat_items$sads],           na.rm = TRUE)
mdib_hd_dat$sads_red_m       <- rowMeans(mdib_hd_dat[, mdib_dat_items$sads_red],       na.rm = TRUE)
mdib_hd_dat$auditc_m         <- rowMeans(mdib_hd_dat[, mdib_dat_items$auditc],         na.rm = TRUE)

mdib_hd_dat$mdib_neg_9_int_m[is.nan(mdib_hd_dat$mdib_neg_9_int_m)] <- NA
mdib_hd_dat$mdib_neg_9_ext_m[is.nan(mdib_hd_dat$mdib_neg_9_ext_m)] <- NA
mdib_hd_dat$bbsiq_neg_int_m[is.nan(mdib_hd_dat$bbsiq_neg_int_m)]   <- NA
mdib_hd_dat$bbsiq_neg_ext_m[is.nan(mdib_hd_dat$bbsiq_neg_ext_m)]   <- NA
mdib_hd_dat$asi_m[is.nan(mdib_hd_dat$asi_m)]                       <- NA
mdib_hd_dat$asi_red_phy_m[is.nan(mdib_hd_dat$asi_red_phy_m)]       <- NA
mdib_hd_dat$asi_red_cog_m[is.nan(mdib_hd_dat$asi_red_cog_m)]       <- NA
mdib_hd_dat$asi_red_soc_m[is.nan(mdib_hd_dat$asi_red_soc_m)]       <- NA
mdib_hd_dat$bfne2_8_m[is.nan(mdib_hd_dat$bfne2_8_m)]               <- NA
mdib_hd_dat$neuroqol_anx_m[is.nan(mdib_hd_dat$neuroqol_anx_m)]     <- NA
mdib_hd_dat$sads_m[is.nan(mdib_hd_dat$sads_m)]                     <- NA
mdib_hd_dat$sads_red_m[is.nan(mdib_hd_dat$sads_red_m)]             <- NA
mdib_hd_dat$auditc_m[is.nan(mdib_hd_dat$auditc_m)]                 <- NA

# ---------------------------------------------------------------------------- #
# Create table of item-level missingness for MDIB at baseline ----
# ---------------------------------------------------------------------------- #

# Restrict to MDIB items at baseline

mdib_bl <- mdib_hd_dat[mdib_hd_dat$redcap_event_name == bl,
                       c(mdib_dat_items$mdib_neg, mdib_dat_items$mdib_ben)]

# Order columns by item number

mdib_item_map <- mdib_item_map[order(mdib_item_map$item_number), ]

mdib_bl <- mdib_bl[match(mdib_item_map$items_rename, names(mdib_bl))]

# Compute number and proportion of participants (out of 70) missing each item

#(n <- nrow(mdib_bl)) == 70
(n <- nrow(mdib_bl)) == 88

item_n_missing      <- colSums(is.na(mdib_bl))
item_perc_missing   <- format(round((item_n_missing / n) * 100, 1),
                              nsmall = 1, trim = TRUE)
item_n_perc_missing <- paste0(item_n_missing, " (", item_perc_missing, ")")

mdib_bl_item_missing_tbl <- data.frame(domain         = mdib_item_map$domain,
                                       item           = names(mdib_bl),
                                       meaning        = mdib_item_map$meaning,
                                       n_missing      = item_n_missing,
                                       n_perc_missing = item_n_perc_missing)

  # Restrict to items not answered by at least 1 participant

mdib_bl_item_missing_tbl <- mdib_bl_item_missing_tbl[mdib_bl_item_missing_tbl$n_missing > 0, ]

row.names(mdib_bl_item_missing_tbl) <- 1:nrow(mdib_bl_item_missing_tbl)

# Export table to CSV

write.csv(mdib_bl_item_missing_tbl, paste0(missing_rates_path, "mdib_bl_item_missing_tbl.csv"), row.names = FALSE)

# ---------------------------------------------------------------------------- #
# Remove participants with incomplete MDIB data at baseline ----
# ---------------------------------------------------------------------------- #

# Remove 6 participants with incomplete MDIB data at baseline, leaving an overall 
# analysis sample of 82 participants

mdib_hd_dat <- mdib_hd_dat[!(mdib_hd_dat$record_id %in% incompl_mdib_bl_data_ids), ]

length(unique(mdib_hd_dat$record_id)) == 85

# ---------------------------------------------------------------------------- #
# Compute rates of item-level missingness ----
# ---------------------------------------------------------------------------- #

# Define function to compute percentage of scale scores across specified time 
# points computed with at least one item missing for given scale

compute_some_item_missingness <- function(dat, scale, items, time_points) {
  dat <- dat[dat$redcap_event_name %in% time_points, ]
  
  denom <- sum(!is.na(dat[, scale]))
  
  rows_at_least_one_item_na <- rowSums(is.na(dat[, items])) > 0
  rows_all_items_na <- rowSums(!is.na(dat[, items])) == 0
  
  numer <- nrow(dat[rows_at_least_one_item_na & !rows_all_items_na, ])
  
  prop <- numer/denom
  percent <- prop*100
  
  cat(scale, ": ", percent, "%", "\n", sep = "")
}

# Run function for ITT participants and write results

sink(file = paste0(missing_rates_path, "some_item_missingness.txt"))

cat("Percentages of Scale Scores Computed With At Least One Item Missing:", "\n\n")

compute_some_item_missingness(mdib_hd_dat, "mdib_neg_9_int_m" , mdib_dat_items$mdib_neg_9_int, both)
compute_some_item_missingness(mdib_hd_dat, "mdib_neg_9_ext_m" , mdib_dat_items$mdib_neg_9_ext, both)
compute_some_item_missingness(mdib_hd_dat, "bbsiq_neg_int_m"  , mdib_dat_items$bbsiq_neg_int,  both)
compute_some_item_missingness(mdib_hd_dat, "bbsiq_neg_ext_m"  , mdib_dat_items$bbsiq_neg_ext,  both)
compute_some_item_missingness(mdib_hd_dat, "asi_m"            , mdib_dat_items$asi,            bl)
compute_some_item_missingness(mdib_hd_dat, "asi_red_phy_m"    , mdib_dat_items$asi_red_phy,    bl)
compute_some_item_missingness(mdib_hd_dat, "asi_red_cog_m"    , mdib_dat_items$asi_red_cog,    bl)
compute_some_item_missingness(mdib_hd_dat, "asi_red_soc_m"    , mdib_dat_items$asi_red_soc,    bl)
compute_some_item_missingness(mdib_hd_dat, "bfne2_8_m"        , mdib_dat_items$bfne2_8,        bl)
compute_some_item_missingness(mdib_hd_dat, "neuroqol_anx_m"   , mdib_dat_items$neuroqol_anx,   both)
compute_some_item_missingness(mdib_hd_dat, "sads_m"           , mdib_dat_items$sads,           bl)
compute_some_item_missingness(mdib_hd_dat, "sads_red_m"       , mdib_dat_items$sads_red,       fu)
compute_some_item_missingness(mdib_hd_dat, "auditc_m"         , mdib_dat_items$auditc,         bl)

sink()

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./data/further_clean")

save(mdib_hd_dat, file = "~/data/further_clean/mdib_hd_dat.RData")

dir.create("./data/helper")

save(mdib_dat_items, file = "~/data/helper/mdib_dat_items.RData")
save(mdib_item_map,  file = "~/data/helper/mdib_item_map.RData")
