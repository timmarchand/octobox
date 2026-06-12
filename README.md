# Octobox

**Online Toolbox for Corpus Linguistics**

Octobox is an R Shiny application for corpus analysis, bringing common corpus
linguistics techniques together in a single browser-based interface. It is
designed for researchers, teachers, and students who want to explore texts
without writing code.

## 🚀 Try it online

You can use Octobox directly in your browser — no installation required:

**[▶ Launch Octobox (Cloud Connect version)](https://timmarchand-octobox.share.connect.posit.cloud/)**

## Features

- **📊 Corpus Summary** — overview statistics for your uploaded texts
- **📈 Frequency Analysis** — word and headword frequency lists with reference-corpus comparison
- **📝 Part-of-Speech** — POS distribution and analysis
- **📍 Dispersion** — how evenly terms are spread across the corpus
- **🎯 Keyword Analysis** — keyness comparison against a reference corpus
- **🔍 KWIC & Collocation** — keyword-in-context concordances and collocation measures
- **POS Tagging** — automatic tagging powered by a udpipe model
- **🧭 Explore Further** — research references behind each module, downloadable standalone R Projects that reproduce the analyses outside Shiny, and a link to the companion [MDA Tagger](https://github.com/timmarchand/mda_tagger) app

## Running locally

1. Clone or download this repository.
2. Open `Octobox.Rproj` in RStudio (or set the working directory to the project root).
3. Install the required packages:

   ```r
   install.packages(c(
     "shiny", "DT", "quanteda", "quanteda.textstats", "dplyr",
     "ggplot2", "readr", "stringr", "tidyr", "tibble", "data.table",
     "tidyselect", "purrr", "memoise", "cachem", "digest"
   ))
   ```

4. Launch the app:

   ```r
   shiny::runApp()
   ```

The repository includes the bundled English udpipe model
(`english-ewt-ud-2.5-191206.udpipe`) and supporting data files
(`wordFreq.csv`, `US_UK_spelling.csv`), so no extra downloads are needed.

> **Note:** the upload limit is set to 100 MB.

## License

Released under the [MIT License](LICENSE). © 2026 timmarchand.
