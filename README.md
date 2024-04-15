# opentranslatr

<!-- badges: start -->
[![CodeFactor](https://www.codefactor.io/repository/github/myanesp/opentranslatr/badge)](https://www.codefactor.io/repository/github/myanesp/opentranslatr) [![](https://img.shields.io/github/languages/code-size/myanesp/opentranslatr.svg)](https://github.com/myanesp/opentranslatr) [![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![](https://badgen.net/github/stars/myanesp/opentranslatr?icon=github&label=stars)
![Github last-commit](https://img.shields.io/github/last-commit/myanesp/opentranslatr)
![Github license](https://badgen.net/github/license/myanesp/opentranslatr)
<!-- badges: end -->

## Translate in R using opensource frontends for common translations engines
This R package allows you to translate text directly from R: from dataframes to strings,
it lets you to use common translations engines like Google Translate using a frontend,
which it is a benefit in terms of privacy. For now, these are the supported ones:

- [SimplyTranslate](https://codeberg.org/ManeraKai/simplytranslate). It supports Google Translate,
Reverso and inCIBA.
- [Lingva](https://github.com/thedaviddelta/lingva-translate). It supports Google Translate. 
- [gtranslate](https://git.sr.ht/~yerinalexey/gtranslate). Lightweight frontend for Google Translate.
You can selfhosted it with [my Docker image](https://github.com/myanesp/docker-gtranslate/). (Experimental support) 

### Features
- Translate texts, strings of R objects without leaving R using frontends for Google Translate or Reverso.
- Automatically detect languages.
- Get a list of available languages.

### Installation
This package is not yet available on CRAN, so you must install from GitHub using `remotes` package.
```r
if (!require("remotes")) install.packages("remotes")
remotes::install_github("myanesp/opentranslatr")
# And then load into your current session
library(opentranslatr)
```

### Examples and usage
```r
lingva(from = "es", to = "fr", str = "hola, ¿cómo estamos?")
[1] "Bonjour, comment allez-vous?"

simplytranslate(from = "en", to = "de", str = "welcome!")
[1] "Willkommen!"

simplytranslate(from = "en", to = "fr", str = "welcome!", engine = "reverso") # select engine
[1] "bienvenue!"

get_languages() # Get available languagues for the frontend you choose
```
