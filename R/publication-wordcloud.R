# Generate a word cloud visualization for the publications page

# brew install python@3.9
# python3.9 -m pip install PyPaperBot

# Read DOI of publications
df <- bib2df::bib2df("content/keaven.bib") |> suppressWarnings()
doi <- df$DOI[!is.na(df$DOI)]

# Remove DOI that full text PDF "not found" or if it's a book
doi_remove <- c(
  "10.1200/jco.2009.27.15\\_suppl.2517",
  "10.1200/jco.2010.28.15\\_suppl.e13159",
  "10.48550/arXiv.2206.12536",
  "10.1007/978-1-4939-1100-4"
)
doi <- doi[!(doi %in% doi_remove)]

doi_file <- tempfile(fileext = ".txt")
writeLines(doi, con = doi_file)

pdf_dir <- file.path(tempdir(), "pdf_dir")
if (!dir.exists(pdf_dir)) dir.create(pdf_dir)
browseURL(pdf_dir)

# Copy and run in terminal
cat(paste0(
  "python3.9 -m PyPaperBot ",
  '--doi-file="', doi_file, '" ',
  '--dwn-dir="', pdf_dir, '"'
))

# Convert PDF to txt
pdf_path <- list.files(pdf_dir, pattern = "*.pdf", full.names = TRUE)
txt_path <- file.path(pdf_dir, paste0(seq_along(pdf_path), ".txt"))

cmd <- paste0("pdftotext ", '"', pdf_path, '" "', txt_path, '"')

for (i in seq_along(pdf_path)) {
  cat(i, "\n")
  system(cmd[i])
}

# Read txt
txt_lst <- vector("list", length(txt_path))
for (i in seq_along(txt_path)) txt_lst[[i]] <- readLines(txt_path[i], warn = FALSE)

# Split into words
for (i in length(txt_lst)) txt_lst[[i]] <- paste(txt_lst[[i]], collapse = " ")
txt <- paste0(unlist(txt_lst), collapse = " ")

# Keep only alphanumeric characters, convert to ASCII, to lowercase
txt <- gsub("[^[:alnum:][:space:]]", replacement = " ", x = txt)
txt <- stringi::stri_trans_general(txt, "latin-ascii")
txt <- tolower(txt)

# Count word frequency
# writeLines(txt, con = "txt.txt")
# txt <- readLines("txt.txt")
word_freq <- sort(table(unlist(strsplit(txt, " "))), decreasing = TRUE)

# Remove stop words
stop_words <- tidytext::stop_words$word
word_freq <- word_freq[!(names(word_freq) %in% stop_words)]

# Remove numbers and space
word_freq <- word_freq[!(names(word_freq) %in% as.character(0:1000))]

# Remove additional stop words
word_freq <- word_freq[
  !(names(word_freq) %in%
    c(
      "", "al", "lv", "001", "md", "mg", "mass", "table", "university",
      "downloaded", "anderson", "st", "figure", "ii", "kg", "http", "mm",
      "2015", "05", "2018", "kannel", "01", "wb", "000", "iii", "2014", "june",
      "z1", "doi", "de", "1988", "jama", "engl", "1987", "dr", "keaven",
      "april", "1986", "castelli", "1994", "1983", "levy", "ad", "institute",
      "1989", "department", "1990", "ann", "pa", "liu", "wp", "ac", "index",
      "ma", "rj", "\\f", "00", "clin", "jr", "merck", "1984", "boston",
      "03", "1997", "massachusetts", "copyright", "1985", "1991",
      "ahajournals", "topol", "09", "biometrics", "circ", "usa",
      "07", "1979", "02", "1981", "coll", "references",
      "phd", "2000", "mb", "1980", "lancet", "yr", "04", "society",
      "1982", "1999", "nj", "n1", "ut", "author", "ca", "www",
      "ia", "06", "aacrjournals", "km", "08", "id", "pd",
      "wiley", "califf", "dd", "authors", "na", "1995", "1998",
      "ml", "cm", "1996", "iv", "suppl", "tr", "2001", "ej",
      "0432", "1158", "sed", "wilson", "1078", "pm", "1978",
      "onlinefirst", "paper", "2006", "ool", "1993", "ba",
      "ti", "van", "002", "ast", "ed", "la", "1977", "march",
      "rb", "2010", "school", "cr", "0001", "2002", "2016",
      "a1", "john", "est", "rr", "tion", "peer", "wbc", "edited",
      "vitae", "2007", "ck", "manuscripts", "rm", "white",
      "m2", "se", "dis", "005", "cl0", "library", "n2",
      "x2", "\\fauthor", "ns", "amet", "b1", "bs", "december",
      "november", "p1", "cl", "ing", "l1", "pre", "pro",
      "epidemiol", "february", "jstor", "li", "sagepub",
      "pennsylvania", "public", "jm", "wang", "malvern",
      "mcnamara", "michael", "january", "article", "min",
      "due", "fh", "hg", "journal", "mi", "dl", "based",
      "org"
    ))
]

# Correct case
names(word_freq)[names(word_freq) == "framingham"] <- "Framingham"
names(word_freq)[names(word_freq) == "iib"] <- "IIb"
names(word_freq)[names(word_freq) == "iiia"] <- "IIIa"
names(word_freq)[names(word_freq) == "cox"] <- "Cox"
names(word_freq)[names(word_freq) == "american"] <- "American"
names(word_freq)[names(word_freq) == "ecg"] <- "ECG"
names(word_freq)[names(word_freq) == "c7e3"] <- "c7E3"
names(word_freq)[names(word_freq) == "timi"] <- "TIMI"
names(word_freq)[names(word_freq) == "ci"] <- "CI"
names(word_freq)[names(word_freq) == "hdl"] <- "HDL"
names(word_freq)[names(word_freq) == "ldl"] <- "LDL"
names(word_freq)[names(word_freq) == "hr"] <- "HR"
names(word_freq)[names(word_freq) == "ptca"] <- "PTCA"
names(word_freq)[names(word_freq) == "lvh"] <- "LVH"
names(word_freq)[names(word_freq) == "epic"] <- "EPIC"
names(word_freq)[names(word_freq) == "cvd"] <- "CVD"
names(word_freq)[names(word_freq) == "fab"] <- "Fab"
names(word_freq)[names(word_freq) == "chd"] <- "CHD"

writeLines(
  rep(names(word_freq[1:250]), times = as.numeric(word_freq[1:250])),
  "words.txt",
  sep = " "
)

# Copy and paste into d3-cloud <https://www.jasondavies.com/wordcloud/>
#
# Parameters:
#
# - Scale: n
# - 2 orientations from -90 to 0
# - Number of words: 200
# - Font: Avenir Next Condensed Medium
#
# - Try different until getting a comfortable layout
# - Download as `wordcloud.svg`
# - Open and print in browser as `wordcloud.pdf`
# - Run `pdfcrop wordcloud.pdf`
# - Run `convert -density 300 wordcloud-crop.pdf wordcloud.png`
# - Copy SVG and PNG into `static/images/`
