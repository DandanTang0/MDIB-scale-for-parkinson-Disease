# ---------------------------------------------------------------------------- #

# Set project-wide formatting defaults for tables created with the flextable
# package. These defaults are used by scripts that export manuscript tables to
# Word documents.

library(flextable)

set_flextable_defaults(
  font.size = 12,
  font.family = "Times New Roman",
  font.color = "black", 
  border.width = 0.5,
  border.color = "black",
  padding.bottom = 0,
  padding.top = 0, 
  padding.left = 0.08,
  padding.right = 0.08,
  line_spacing = 1
)
