# =============================================================================
# modules/ui_references.R - Academic References & Further Reading ----
# =============================================================================

# Helper: a download card for the Code tab.
# Files live in www/code/ and are served statically, so a plain anchor with
# the `download` attribute works reliably (no server-side downloadHandler).
download_card <- function(emoji, title, accent, description, href) {
  div(
    style = paste0("border: 1px solid #e1e8ed; border-top: 4px solid ", accent,
                   "; border-radius: 8px; padding: 18px; display: flex; flex-direction: column; ",
                   "background-color: #ffffff; box-shadow: 0 1px 4px rgba(0,0,0,0.04);"),
    div(style = "font-size: 30px; margin-bottom: 8px;", emoji),
    h4(title, style = paste0("margin: 0 0 8px 0; color: ", accent, ";")),
    p(description, style = "color: #555; font-size: 13px; line-height: 1.5; flex-grow: 1;"),
    tags$a(
      href = href,
      download = NA,
      class = "btn btn-default",
      style = paste0("margin-top: 12px; border-color: ", accent, "; color: ", accent, ";"),
      icon("download"), " Download R Project"
    )
  )
}

referencesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "max-width: 1200px; margin: 0 auto; padding: 20px;",
      
      h2("🧭 Explore Further", style = "color: #2c3e50; margin-bottom: 20px;"),
      
      div(
        style = "background-color: #e8f4f8; border-left: 4px solid #3498db; padding: 15px; margin-bottom: 25px; border-radius: 4px;",
        p(strong("About this page:"), "Go beyond the app: read the research behind each technique, download standalone R code that reproduces the analyses outside Shiny, and discover related tools.", style = "margin: 0; font-size: 14px;")
      ),
      
      tabsetPanel(
        id = ns("explore_tabs"),
        type = "tabs",
        
        # =====================================================================
        # SUB-TAB 1: READING ----
        # =====================================================================
        tabPanel(
          title = tagList(icon("book"), " Reading"),
          br(),
          div(
            style = "background-color: #f8f9fa; border-radius: 4px; padding: 12px 15px; margin-bottom: 25px;",
            p("Theoretical foundations and methodological guidance for the corpus analysis techniques in this tool. Each section corresponds to a module in the application.", style = "margin: 0; font-size: 14px; color: #555;")
          ),
      
      # TTR Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("📊 Type-Token Ratio (TTR) & Lexical Diversity", 
           style = "color: #e74c3c; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Covington, M. A., & McFall, J. D. (2010)."), "Cutting the Gordian knot: The moving-average type–token ratio (MATTR).", em("Journal of Quantitative Linguistics"), ", 17(2), 94-100."),
          p(tags$a(href = "https://doi.org/10.1080/09296171003643098", "https://doi.org/10.1080/09296171003643098", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("McCarthy, P. M., & Jarvis, S. (2010)."), "MTLD, vocd-D, and HD-D: A validation study of sophisticated approaches to lexical diversity assessment.", em("Behavior Research Methods"), ", 42(2), 381-392."),
          p(tags$a(href = "https://doi.org/10.3758/BRM.42.2.381", "https://doi.org/10.3758/BRM.42.2.381", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Jarvis, S. (2013)."), "Capturing the diversity in lexical diversity.", em("Language Learning"), ", 63(s1), 87-106."),
          p(tags$a(href = "https://doi.org/10.1111/j.1467-9922.2012.00739.x", "https://doi.org/10.1111/j.1467-9922.2012.00739.x", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # Lexical Range Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("📈 Lexical Range & Frequency Bands", 
           style = "color: #9b59b6; border-bottom: 2px solid #9b59b6; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Nation, I. S. P. (2006)."), "How large a vocabulary is needed for reading and listening?", em("Canadian Modern Language Review"), ", 63(1), 59-82."),
          p(tags$a(href = "https://doi.org/10.3138/cmlr.63.1.59", "https://doi.org/10.3138/cmlr.63.1.59", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Laufer, B., & Nation, P. (1995)."), "Vocabulary size and use: Lexical richness in L2 written production.", em("Applied Linguistics"), ", 16(3), 307-322."),
          p(tags$a(href = "https://doi.org/10.1093/applin/16.3.307", "https://doi.org/10.1093/applin/16.3.307", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Webb, S., & Nation, P. (2017)."), "How vocabulary is learned.", em("Oxford University Press"), "."),
          p(tags$a(href = "https://doi.org/10.1093/oso/9780194878845.001.0001", "https://doi.org/10.1093/oso/9780194878845.001.0001", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # N-grams Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("🔢 N-grams & Multi-word Units", 
           style = "color: #27ae60; border-bottom: 2px solid #27ae60; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Biber, D., Conrad, S., & Cortes, V. (2004)."), "If you look at...: Lexical bundles in university teaching and textbooks.", em("Applied Linguistics"), ", 25(3), 371-405."),
          p(tags$a(href = "https://doi.org/10.1093/applin/25.3.371", "https://doi.org/10.1093/applin/25.3.371", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Evert, S. (2009)."), "Corpora and collocations. In A. Lüdeling & M. Kytö (Eds.),", em("Corpus linguistics: An international handbook"), "(pp. 1212-1248). Mouton de Gruyter."),
          p(tags$a(href = "https://doi.org/10.1515/9783110213881.2.1212", "https://doi.org/10.1515/9783110213881.2.1212", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Bestgen, Y., & Granger, S. (2014)."), "Quantifying the development of phraseological competence in L2 English writing: An automated approach.", em("Journal of Second Language Writing"), ", 26, 28-41."),
          p(tags$a(href = "https://doi.org/10.1016/j.jslw.2014.09.004", "https://doi.org/10.1016/j.jslw.2014.09.004", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # POS Tagging Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("🏷️ Part-of-Speech Tagging", 
           style = "color: #f39c12; border-bottom: 2px solid #f39c12; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Straka, M., & Straková, J. (2017)."), "Tokenizing, POS tagging, lemmatizing and parsing UD 2.0 with UDPipe. In", em("Proceedings of the CoNLL 2017 Shared Task"), "(pp. 88-99)."),
          p(tags$a(href = "https://doi.org/10.18653/v1/K17-3009", "https://doi.org/10.18653/v1/K17-3009", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("De Marneffe, M. C., Manning, C. D., Nivre, J., & Zeman, D. (2021)."), "Universal Dependencies.", em("Computational Linguistics"), ", 47(2), 255-308."),
          p(tags$a(href = "https://doi.org/10.1162/coli_a_00402", "https://doi.org/10.1162/coli_a_00402", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Marcus, M., Santorini, B., & Marcinkiewicz, M. A. (1993)."), "Building a large annotated corpus of English: The Penn Treebank.", em("Computational Linguistics"), ", 19(2), 313-330."),
          p("Available at: ", tags$a(href = "https://aclanthology.org/J93-2004/", "https://aclanthology.org/J93-2004/", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # POS Distribution Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("📊 POS Distribution Analysis", 
           style = "color: #e67e22; border-bottom: 2px solid #e67e22; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Biber, D. (1988)."), em("Variation across speech and writing"), ". Cambridge University Press."),
          p(tags$a(href = "https://doi.org/10.1017/CBO9780511621024", "https://doi.org/10.1017/CBO9780511621024", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Granger, S., & Rayson, P. (1998)."), "Automatic profiling of learner texts. In S. Granger (Ed.),", em("Learner English on computer"), "(pp. 119-131). Longman."),
        
          
          p(style = "margin-top: 15px;", strong("Kyle, K., & Crossley, S. A. (2015)."), "Automatically assessing lexical sophistication: Indices, tools, findings, and application.", em("TESOL Quarterly"), ", 49(4), 757-786."),
          p(tags$a(href = "https://doi.org/10.1002/tesq.194", "https://doi.org/10.1002/tesq.194", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # Keyword Analysis Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("🔑 Keyword Analysis", 
           style = "color: #c0392b; border-bottom: 2px solid #c0392b; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Scott, M., & Tribble, C. (2006)."), em("Textual patterns: Key words and corpus analysis in language education"), ". John Benjamins."),
          p(tags$a(href = "https://doi.org/10.1075/scl.22", "https://doi.org/10.1075/scl.22", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Gabrielatos, C., & Marchi, A. (2012)."), "Keyness: Appropriate metrics and practical issues. In", em("CADS International Conference 2012"), "."),
          p("Available at: ", tags$a(href = "http://repository.lancaster.ac.uk/id/eprint/51449/", "http://repository.lancaster.ac.uk/id/eprint/51449/", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Dunning, T. (1993)."), "Accurate methods for the statistics of surprise and coincidence.", em("Computational Linguistics"), ", 19(1), 61-74."),
          p("Available at: ", tags$a(href = "https://aclanthology.org/J93-1003/", "https://aclanthology.org/J93-1003/", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # Dispersion Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("📍 Dispersion & Distribution Analysis", 
           style = "color: #16a085; border-bottom: 2px solid #16a085; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Gries, S. Th. (2008)."), "Dispersions and adjusted frequencies in corpora.", em("International Journal of Corpus Linguistics"), ", 13(4), 403-437."),
          p(tags$a(href = "https://doi.org/10.1075/ijcl.13.4.02gri", "https://doi.org/10.1075/ijcl.13.4.02gri", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Lijffijt, J., & Gries, S. Th. (2012)."), "Correction to Stefan Th. Gries' 'Dispersions and adjusted frequencies in corpora'.", em("International Journal of Corpus Linguistics"), ", 17(1), 147-149."),
          p(tags$a(href = "https://doi.org/10.1075/ijcl.17.1.08lij", "https://doi.org/10.1075/ijcl.17.1.08lij", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Brezina, V., McEnery, T., & Wattam, S. (2015)."), "Collocations in context: A new perspective on collocation networks.", em("International Journal of Corpus Linguistics"), ", 20(2), 139-173."),
          p(tags$a(href = "https://doi.org/10.1075/ijcl.20.2.01bre", "https://doi.org/10.1075/ijcl.20.2.01bre", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # Collocations Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("🔗 Collocations & Association Measures", 
           style = "color: #8e44ad; border-bottom: 2px solid #8e44ad; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("Evert, S. (2008)."), "Corpora and collocations. In A. Lüdeling & M. Kytö (Eds.),", em("Corpus linguistics: An international handbook"), "(Vol. 2, pp. 1212-1248). Mouton de Gruyter."),
          p(tags$a(href = "https://doi.org/10.1515/9783110213881.2.1212", "https://doi.org/10.1515/9783110213881.2.1212", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Gries, S. Th. (2013)."), "50-something years of work on collocations: What is or should be next...", em("International Journal of Corpus Linguistics"), ", 18(1), 137-166."),
          p(tags$a(href = "https://doi.org/10.1075/ijcl.18.1.09gri", "https://doi.org/10.1075/ijcl.18.1.09gri", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Pecina, P. (2010)."), "Lexical association measures and collocation extraction.", em("Language Resources and Evaluation"), ", 44(1), 137-158."),
          p(tags$a(href = "https://doi.org/10.1007/s10579-009-9101-4", "https://doi.org/10.1007/s10579-009-9101-4", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
      # General Corpus Linguistics Section ----
      div(
        style = "margin-bottom: 40px;",
        h4("📖 General Corpus Linguistics Methodology", 
           style = "color: #34495e; border-bottom: 2px solid #34495e; padding-bottom: 10px;"),
        
        div(style = "margin-left: 20px;",
          p(strong("McEnery, T., & Hardie, A. (2012)."), em("Corpus linguistics: Method, theory and practice"), ". Cambridge University Press."),
          p(tags$a(href = "https://doi.org/10.1017/CBO9780511981395", "https://doi.org/10.1017/CBO9780511981395", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Gries, S. Th. (2017)."), em("Quantitative corpus linguistics with R: A practical introduction"), " (2nd ed.). Routledge."),
          p(tags$a(href = "https://doi.org/10.4324/9781315746210", "https://doi.org/10.4324/9781315746210", target = "_blank", style = "color: #3498db;")),
          
          p(style = "margin-top: 15px;", strong("Brezina, V. (2018)."), em("Statistics in corpus linguistics: A practical guide"), ". Cambridge University Press."),
          p(tags$a(href = "https://doi.org/10.1017/9781316410899", "https://doi.org/10.1017/9781316410899", target = "_blank", style = "color: #3498db;"))
        )
      ),
      
          # Reading footer ----
          hr(),
          div(
            style = "text-align: center; color: #7f8c8d; font-size: 13px; margin-top: 30px;",
            p("This tool implements computational methods from corpus linguistics research."),
            p("For questions about specific implementations, consult the referenced papers above.")
          )
        ),  # close Reading tabPanel
        
        # =====================================================================
        # SUB-TAB 2: CODE ----
        # =====================================================================
        tabPanel(
          title = tagList(icon("code"), " Code"),
          br(),
          div(
            style = "background-color: #f8f9fa; border-radius: 4px; padding: 12px 15px; margin-bottom: 25px;",
            p("Each download is a self-contained R Project that reproduces one of Octobox's analyses ", strong("outside Shiny"), " - a plain script you can open in RStudio, run on the included sample corpus, and adapt to your own data. Each zip contains an ", code(".Rproj"), ", a commented ", code(".R"), " script, a sample CSV, and a short README.", style = "margin: 0; font-size: 14px; color: #555;")
          ),
          
          # Card builder is defined inline below via download_card()
          div(
            style = "display: grid; grid-template-columns: repeat(auto-fill, minmax(330px, 1fr)); gap: 18px;",
            
            download_card(
              "📈", "Frequency Analysis", "#9b59b6",
              "Word and n-gram frequency lists, with optional stopword removal and grouping by metadata.",
              "code/frequency_analysis.zip"
            ),
            download_card(
              "🔍", "KWIC & Collocation", "#16a085",
              "Keyword-in-context concordance lines plus statistically notable collocations.",
              "code/kwic_collocation.zip"
            ),
            download_card(
              "🎯", "Keyword Analysis", "#e67e22",
              "Keyness comparison of a target sub-corpus against a reference sub-corpus.",
              "code/keyword_analysis.zip"
            ),
            download_card(
              "📍", "Dispersion Analysis", "#2980b9",
              "How evenly words spread across a corpus, via Gries' Deviation of Proportions and range.",
              "code/dispersion_analysis.zip"
            ),
            download_card(
              "🏷️", "POS Tagging", "#c0392b",
              "Part-of-speech tags and lemmas with a udpipe model, plus a POS distribution summary.",
              "code/pos_tagging.zip"
            ),
            download_card(
              "📊", "Tokenization & Diversity", "#27ae60",
              "Tokenisation, token/type counts, and lexical diversity measures (TTR, CTTR, MATTR).",
              "code/tokenization_diversity.zip"
            )
          ),
          
          hr(style = "margin-top: 30px;"),
          div(
            style = "text-align: center; color: #7f8c8d; font-size: 13px;",
            p("Scripts are released under the same MIT licence as Octobox. First run of the POS script downloads the udpipe model (~16 MB).")
          )
        ),  # close Code tabPanel
        
        # =====================================================================
        # SUB-TAB 3: MDA TAGGER ----
        # =====================================================================
        tabPanel(
          title = tagList(icon("diagram-project"), " MDA Tagger"),
          br(),
          div(
            style = "max-width: 720px; margin: 20px auto; background-color: #ffffff; border: 1px solid #e1e8ed; border-radius: 8px; padding: 30px; text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,0.05);",
            div(style = "font-size: 46px; margin-bottom: 10px;", "🧩"),
            h3("MDA Tagger", style = "color: #2c3e50; margin-bottom: 12px;"),
            p("A Multi-Dimensional Analysis toolkit for linguistic corpus analysis, based on Biber (1988). Upload texts (TXT, CSV, DOCX), tag them with UDPipe, and extract 67+ linguistic features to produce 5-dimensional MDA scores and text-type classifications, with interactive visualisations throughout.",
              style = "color: #555; font-size: 15px; line-height: 1.6; margin-bottom: 18px;"),
            div(
              style = "text-align: left; max-width: 460px; margin: 0 auto 22px auto; color: #555; font-size: 14px; line-height: 1.7;",
              tags$ul(
                style = "padding-left: 20px; margin: 0;",
                tags$li("Multi-format upload with corpus metadata management"),
                tags$li("POS tagging with UDPipe and 67+ feature extraction"),
                tags$li("5-dimensional MDA scoring and text-type classification"),
                tags$li("KWIC concordancing of tags and exportable plot code")
              )
            ),
            tags$a(
              href = "https://github.com/timmarchand/mda_tagger",
              target = "_blank",
              class = "btn btn-primary",
              style = "background-color: #24292e; border-color: #24292e; padding: 10px 26px; font-size: 15px;",
              icon("github"), " View on GitHub"
            ),
            p(style = "margin-top: 16px; font-size: 12px; color: #95a5a6;",
              "Opens in a new tab. A hosted version on Connect Cloud is coming soon.")
          )
        )  # close MDA Tagger tabPanel
        
      )  # close tabsetPanel
    )
  )
}