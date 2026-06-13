# Octobox

**Online Toolbox for Corpus Linguistics**

Octobox is an R Shiny application for corpus analysis, bringing common corpus
linguistics techniques together in a single browser-based interface. It is
designed for researchers, teachers, and students who want to explore texts
without writing code.

## 🌐 Live App

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

## Citation

If you use Octobox in your research or teaching, please cite it as:

> Marchand, T. (2026). *Octobox: An Online Toolbox for Corpus Linguistics* [Software]. Retrieved from https://github.com/timmarchand/octobox

If you wish to cite the conference presentation:

> Marchand, T. (2026, July). *Octobox: A frictionless web-based toolkit for corpus-informed pedagogy in an AI-mediated classroom.* Paper presented at the 17th International Teaching and Language Corpora (TaLC) Conference, University of Extremadura, Jarandilla de la Vera, Spain.

### Please also cite

Octobox is built on the following tools and methods, which you should cite where relevant:

> Benoit, K., Watanabe, K., Wang, H., Nulty, P., Obeng, A., Müller, S., & Matsuo, A. (2018). quanteda: An R package for the quantitative analysis of textual data. *Journal of Open Source Software*, 3(30), 774. https://doi.org/10.21105/joss.00774

> Wijffels, J. (2023). *udpipe: Tokenization, Parts of Speech Tagging, Lemmatization and Dependency Parsing with the 'UDPipe' 'NLP' Toolkit* [R package]. https://CRAN.R-project.org/package=udpipe

> Straka, M., & Straková, J. (2017). Tokenizing, POS tagging, lemmatizing and parsing UD 2.0 with UDPipe. In *Proceedings of the CoNLL 2017 Shared Task: Multilingual Parsing from Raw Text to Universal Dependencies* (pp. 88–99). Association for Computational Linguistics.

If you use the dispersion measures:

> Gries, S. Th. (2008). Dispersions and adjusted frequencies in corpora. *International Journal of Corpus Linguistics*, 13(4), 403–437. https://doi.org/10.1075/ijcl.13.4.02gri

## License

Released under the [MIT License](LICENSE). © 2026 timmarchand.
