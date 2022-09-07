# Make sure the .bib file is up-to-date
df <- bib2df::bib2df("R/bib/keaven.bib")
df <- df[order(df$YEAR, decreasing = TRUE), ]
c(
  "---",
  "output: md_document",
  "bibliography: keaven.bib",
  "csl: apa-numeric-superscript-brackets.csl",
  "---",
  "",
  paste0("@", df$BIBTEXKEY)
) |>
  writeLines(con = "R/bib/pub.Rmd.txt")

withr::with_dir("R/bib/", rmarkdown::render("pub.Rmd.txt", output_file = "pub.md.txt"))

readLines("R/bib/pub.md.txt") |>
  gsub("^<sup>.*</sup>", "", x = _) |>
  gsub("^\\d+\\\\\\.\\s", "- ", x = _) |>
  writeLines("R/bib/pub.md.txt")

# Open pub.md.txt, copy into content/publication.md
# Remind the (year) headings, add the DOI links to title
