if ( !require("pacman") ) install.packages( "pacman" )

pacman::p_load( "gsheet", "readtext", "DT", "tidyverse",
                "tidytext", "dplyr", "stringr", "wordcloud",
                "ggplot2", "pdftools", "tidyr","readr", "stopwords",
                "scales", "RColorBrewer", "viridis" )

