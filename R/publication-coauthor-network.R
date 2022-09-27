# Generate a co-author network visualization for the publications page

# Read authors of publications
df <- bib2df::bib2df("content/keaven.bib") |> suppressWarnings()
lst_author <- df$AUTHOR

# Split author names into first and last names
split_names <- function(x) do.call(rbind, strsplit(x, split = ", "))
lst_author <- lapply(lst_author, split_names)

# Remove items that don't have co-author info
lst_author <- lst_author[sapply(lst_author, ncol) > 1L]

# Only keep initials for first name
extract_initial <- function(x) gsub("[^::A-Z::]", "", x)
for (i in seq_along(lst_author)) lst_author[[i]][, 2] <- extract_initial(lst_author[[i]][, 2])

# Concatenate first and last name
for (i in seq_along(lst_author)) lst_author[[i]] <- paste(lst_author[[i]][, 2], lst_author[[i]][, 1])

# Remove consortium authors
cauthor <- c(
  " others",
  "EPIC {EPIC} investigators",
  "EPIC EPIC investigators",
  "EPICI EPIC Investigators",
  "EPICSI {EPIC Study Investigators}",
  "FHSG Framingham Heart Study Group",
  "GEPICI Group EPIC Investigators"
)
remove_consortium <- function(x) x[!(x %in% cauthor)]
lst_author <- lapply(lst_author, remove_consortium)

# Remove items that have only one author
lst_author <- lst_author[!(sapply(lst_author, length) == 1L)]

# Consolidate same names but different spellings

# Check names need to be consolidated
tmp1 <- strsplit(sort(unique(unlist(lst_author))), split = " ")
tmp2 <- do.call(rbind, tmp1[which(sapply(tmp1, length) == 2)])
# View(tmp2[order(tmp2[, 2]), ])

replace_name <- function(x, old, new) {
  x[x == old] <- new
  x
}

lst_author <- lapply(lst_author, replace_name, old = "K Anderson", new = "KM Anderson")
lst_author <- lapply(lst_author, replace_name, old = "R Achenbach", new = "RE Achenbach")
lst_author <- lapply(lst_author, replace_name, old = "F Aguirre", new = "FV Aguirre")
lst_author <- lapply(lst_author, replace_name, old = "E Barnathan", new = "ES Barnathan")
lst_author <- lapply(lst_author, replace_name, old = "T Bennett", new = "TD Bennett")
lst_author <- lapply(lst_author, replace_name, old = "J Booth", new = "JE Booth")
lst_author <- lapply(lst_author, replace_name, old = "R Califf", new = "RM Califf")
lst_author <- lapply(lst_author, replace_name, old = "J Christiansen", new = "JC Christiansen")
lst_author <- lapply(lst_author, replace_name, old = "J Clark", new = "JB Clark")
lst_author <- lapply(lst_author, replace_name, old = "P Coussement", new = "PK Coussement")
lst_author <- lapply(lst_author, replace_name, old = "S Ebbinghaus", new = "SW Ebbinghaus")
lst_author <- lapply(lst_author, replace_name, old = "S Ellenberg", new = "SS Ellenberg")
lst_author <- lapply(lst_author, replace_name, old = "D Fintel", new = "DJ Fintel")
lst_author <- lapply(lst_author, replace_name, old = "D Fuller", new = "DL Fuller")
lst_author <- lapply(lst_author, replace_name, old = "R Ivanhoe", new = "RJ Ivanhoe")
lst_author <- lapply(lst_author, replace_name, old = "D Kereiakes", new = "DJ Kereiakes")
lst_author <- lapply(lst_author, replace_name, old = "N Kleiman", new = "NS Kleiman")
lst_author <- lapply(lst_author, replace_name, old = "M Kurz", new = "MA Kurz")
lst_author <- lapply(lst_author, replace_name, old = "LF Le{\\'o}n", new = "LF Leon")
lst_author <- lapply(lst_author, replace_name, old = "TW Lebien", new = "TW LeBien")
lst_author <- lapply(lst_author, replace_name, old = "R Lin", new = "RS Lin")
lst_author <- lapply(lst_author, replace_name, old = "R Melsheimer", new = "RM Melsheimer")
lst_author <- lapply(lst_author, replace_name, old = "S Murphy", new = "SA Murphy")
lst_author <- lapply(lst_author, replace_name, old = "W Olson", new = "WH Olson")
lst_author <- lapply(lst_author, replace_name, old = "J Plehn", new = "JF Plehn")
lst_author <- lapply(lst_author, replace_name, old = "H Poehlman", new = "HW Poehlman")
lst_author <- lapply(lst_author, replace_name, old = "D Savage", new = "DD Savage")
lst_author <- lapply(lst_author, replace_name, old = "J Scherer", new = "JC Scherer")
lst_author <- lapply(lst_author, replace_name, old = "K Sigmon", new = "KN Sigmon")
lst_author <- lapply(lst_author, replace_name, old = "M Simoons", new = "ML Simoons")
lst_author <- lapply(lst_author, replace_name, old = "L Sun", new = "LZ Sun")
lst_author <- lapply(lst_author, replace_name, old = "D Talley", new = "JD Talley")
lst_author <- lapply(lst_author, replace_name, old = "E Topol", new = "EJ Topol")
lst_author <- lapply(lst_author, replace_name, old = "H Weisman", new = "HF Weisman")
lst_author <- lapply(lst_author, replace_name, old = "PW Wilson", new = "PWF Wilson")

# Convert author list in each item to edge list
all_edges <- function(x) t(combn(x, m = 2))
lst_edge <- lapply(lst_author, all_edges)

# Bind edge list together
edge <- do.call(rbind, lst_edge)

# Export to Gephi gexf format
network <- igraph::graph_from_data_frame(d = edge, directed = FALSE)
rgexf::write.gexf(rgexf::igraph.to.gexf(network), output = "coauthors.gexf")

# Open `coauthors.gexf` in Gephi
# - Edges merge strategy: Sum
# - Choose a layout (left panel): Fruchterman Reingold, run
# - Modularity (right panel), run
# - Appearance - Nodes - Partition: choose "Modularity class", apply
# - Appearance - Nodes - color icon, Partition, Palette..., Fancy (light background), apply
# - Drag hub nodes out of the circle
# - Appearance - Nodes - label size icon, Ranking - choose "degree", min 2.5, max 8, apply
# - Preview - Nodes - Border color - custom #FFFFFF
# - Preview - Node labels - check "Show labels"
# - Preview - Node labels - Font, choose Avenir Next Condensed 16
# - Adjust node locations until no obvious overlapping
# - Save the project as a `.gephi` file
# - Export as PDF, convert to PNG
