# ---------------------------------------------------------------------------- #
# Define version_control() ----
# ---------------------------------------------------------------------------- #

# Define a helper function to record the R version used for the reproducibility
# snapshot, load the groundhog package, and set the package-version date used
# throughout the analysis scripts.

version_control <- function() {
  
  # Record the R version used when the PD analyses were finalized. The script
  # issues a warning, rather than stopping, if a different R version is used.
  # This allows the scripts to run on other systems while making version
  # differences explicit.
  
  analysis_R_version <- "R version 4.5.1 (2025-06-13)"
  current_R_version <- R.Version()$version.string
  
  if (current_R_version != analysis_R_version) {
    warning(paste0("These analyses were finalized using ", analysis_R_version,
                   ". You are running ", current_R_version, "."))
  }
  
  # Load packages using groundhog. The groundhog_day specifies the package
  # versions available on that date, which helps ensure that everyone running the
  # analysis scripts uses the same package versions.
  
  # Packages may take longer to load the first time they are called with
  # groundhog.library(), because groundhog may need to install the specified
  # package versions alongside any other versions already installed.
  
  # On Windows, if groundhog needs to install packages from source and the console
  # asks for Rtools, install Rtools from:
  # https://cran.r-project.org/bin/windows/Rtools/
  # Then rerun the script.
  
  library(groundhog)
  
  groundhog_day <- "2025-09-01"
  meta.groundhog(groundhog_day)
  
  return(groundhog_day)
}
