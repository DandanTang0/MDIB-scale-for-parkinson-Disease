# ---------------------------------------------------------------------------- #

# Define project-wide section and text-formatting objects used when exporting
# Word documents with the officer package. These objects are sourced by scripts
# that create manuscript tables or supplemental documents.

library(officer)

psect_prop <- officer::prop_section(
  page_size = officer::page_size(orient = "portrait", width = 8.5, height = 11),
  type = "nextPage"
)

lsect_prop <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape", width = 8.5, height = 11),
  type = "nextPage"
)

text_prop <- officer::fp_text_lite(
  color = "black",
  font.size = 12,
  font.family = "Times New Roman"
)

text_prop_bold <- officer::fp_text_lite(
  color = "black",
  font.size = 12,
  font.family = "Times New Roman",
  bold = TRUE
)

text_prop_italic <- officer::fp_text_lite(
  color = "black",
  font.size = 12,
  font.family = "Times New Roman",
  italic = TRUE
)

