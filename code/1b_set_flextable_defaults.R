# ---------------------------------------------------------------------------- #
# Set "flextable" Package Defaults
# Original code by: Jeremy W. Eberle
# Minor modifications by: Dandan Tang
# ---------------------------------------------------------------------------- #

# Set defaults for "flextable" package (mimic those of MS Word) that can be
# sourced in other scripts
library(flextable)

set_flextable_defaults(font.size = 12, font.family = "Times New Roman", font.color = "black", 
                       border.width = 0.5, border.color = "black",
                       padding.bottom = 0, padding.top = 0, 
                       padding.left = 0.08, padding.right = 0.08,
                       line_spacing = 1)
