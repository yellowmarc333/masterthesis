# #replace some symbols that are not utf-8 encoding and lead to encoding errors
# working:

# data[, headline := gsub(pattern = "’", replacement = "'",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "‘", replacement = "'",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "′", replacement = "'",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "”", replacement = "'",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "“", replacement = "'",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "—", replacement = "-",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "–", replacement = "-",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "‐", replacement = "-",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "-", replacement = "-",
#                           x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "…", replacement = "...",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "\u2028", replacement = "",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "é", replacement = "e",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "™", replacement = "Trademark",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "ö", replacement = "oe",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "ä", replacement = "ae",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "ü", replacement = "ue",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "Â", replacement = "A",
#                         x = headline, fixed = TRUE )]



# from here not working---------------------------------------

# data[, headline := gsub(pattern = "―", replacement = "-",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "  ", replacement = " ",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = " ", replacement = " ",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "S", replacement = "S",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "A", replacement = "A",
#                         x = headline, fixed = TRUE )]
# data[, headline := gsub(pattern = "C", replacement = "C",
#                         x = headline, fixed = TRUE )]



