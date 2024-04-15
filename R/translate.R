#' Translate strings using SimplyTranslate
#'
#' SimplyTranslate has support for three translation engines: Google Translate,
#' inCIBA and Reverso. You can specify which one do you want to use session-wide
#' or every time you execute the function. This function also helps with the selection
#' of the SimplyTranslate instance you want to use.
#'
#' @param from Source language. Specify it with the code following the ISO 639 standard
#' @param to Specify it with the code following the ISO 639 standard
#' @param str Text to translate
#' @param instance The SimplyTranslate URL you want to use
#' @param engine The translation engine you want to use: "google", "reverso" or "inciba"
#' @export
#' @examples
#' \dontrun{
#' simplytranslate(from = "en", to = "es", str = "hello", instance = "https://simplytranslate.org)")
#' }

simplytranslate <- function(from, to, str, instance = NULL, engine = NULL) {

  if (is.null(instance)) {
    if (!nzchar(Sys.getenv("st_inst"))) {
      message("You did not specify any SimplyTranslate instance.")
      cat("\n")
      message("Would you like to select the official one or to specify a custom one?")

      instance <- utils::menu(
        choices = c(
          "Use the official one, simplytranslate.org",
          "I'd prefer specifying a custom one"
        )
      )

      if (instance == 1) {

        Sys.setenv(st_inst = "https://simplytranslate.org")
        message("Using the default one, simplytranslate.org")

      } else {
        st_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", st_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          st_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", st_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }
        Sys.setenv(st_inst = st_instance)
      }
    }
  }

  if(is.null(engine)) {
    if (!nzchar(Sys.getenv("st_eng"))) {
      message("You did not select any of the available translation engines.")
      cat("\n")
      message("Please, select one. Keep in mind that the selected one will be the default engine for this session unless you declare another one on the function.")

      engine <- utils::menu(
        choices = c(
          "Google Translate",
          "iCIBA",
          "Reverso"
        )
      )

      if (engine == 1){
        Sys.setenv(st_eng = "google")
        message("Using Google Translate engine")
      } else if (engine == 2) {
        Sys.setenv(st_eng = "iciba")
        message("Using iCIBA engine")
      } else if (engine == 3) {
        Sys.setenv(st_eng = "reverso")
        message("Using Reverso engine")
      }
    }
  }
  str <- utils::URLencode(str, reserved = T)
  req <- httr2::request(Sys.getenv("st_inst")) |>
    httr2::req_url_path_append("/api/translate") |>
    httr2::req_url_query(engine = Sys.getenv("st_eng"), from = from, to = to, text = str) |>
    httr2::req_perform()

  resp <- req |>
    httr2::resp_body_json()

  translation <- utils::URLdecode(resp$translated_text)

  return(translation)

}


#' Translate strings using Lingva frontend
#'
#' Lingva is a privacy-respecting frontend for the Google Translate engine.
#'
#' @param from Source language. Specify it with the code following the ISO 639 standard
#' @param to Specify it with the code following the ISO 639 standard
#' @param str Text to translate
#' @param instance The Lingva instance URL you want to use. If it's not set, it will ask you to select one
#' @export
#' @examples
#' \dontrun{
#' lingva(from = "en", to = "es", str = "hello")
#' }

lingva <- function(from, to, str, instance = NULL) {
  if(is.null(instance)){

    if (!nzchar(Sys.getenv("ling_inst"))) {
      message("You have not choose an instance.")
      cat("\n")
      message("Would you like to select the official one or to specify a custom one?")
      cat("\n")
      message("Please, keep in mind that the mantainer of the official one has asked to not abuse the API as the costs of the bandwidth are expensive.")

      instance <- utils::menu(
        choices = c(
          "I'm going to use responsibly the official one, lingva.thedaviddelta.com",
          "I'd prefer specifying a custom one"
        )
      )

      if (instance == 1){
        message("Selected the official one.")
        Sys.setenv(ling_inst = "https://lingva.thedaviddelta.com")
      } else {
        ling_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", ling_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          ling_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", ling_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }

        Sys.setenv(ling_inst = ling_instance)
      }
    }
  }

  str <- utils::URLencode(str, reserved = T)

  req <- httr2::request(Sys.getenv("ling_inst")) |>
    httr2::req_url_path_append("/api/v1") |>
    httr2::req_url_path_append(from, to, str)

  resp <- req |>
    httr2::req_perform()

  resp_details <- resp |>
    httr2::resp_body_json()

  translation <- resp_details$translation
  return(translation)
}

#' Translate strings using gtranslate frontend
#'
#' gtranslate is a privacy-respecting frontend for the Google Translate engine.
#'
#' @param from Source language. Specify it with the code following the ISO 639 standard
#' @param to Specify it with the code following the ISO 639 standard
#' @param str Text to translate
#' @export
#' @examples
#' \dontrun{
#' gtranslate(from = "en", to = "es", str = "hello")
#' }

gtranslate <- function(from, to, str) {

  if (!nzchar(Sys.getenv("gt_inst"))) {
    message("You have to input your gtranslate instance")
    cat("\n")
    gt_inst <- readline("Input here the URL with http or https of your gtranslate instance: ")
    Sys.setenv("gt_inst" = gt_inst)
  }

  str <- utils::URLencode(str, reserved = T)
  req <- httr2::request(Sys.getenv("gt_inst")) |>
    httr2::req_url_path_append("/api") |>
    httr2::req_url_query(from = from, to = to, text = str) |>
    httr2::req_perform()

  resp <- req |>
    httr2::resp_body_string()

  translation <- utils::URLdecode(resp)

  return(translation)
}

#' Get a list of supported languages for the instances
#'
#' Run this function and you will be able to know the supported languages for the frontend you choose
#' and also their ISO-639 code, needed to set the from and to languages
#' when using the translation functions.
#'
#' @export
#' @examples
#' \dontrun{
#' get_languages()
#' }

get_languages <- function() {
  message("Please, select the frontend of which you want to obtain the available languages")
  cat("\n")

  frontend <- utils::menu(
    choices = c(
      "Lingva",
      "SimplyTranslate"
    )
  )

  if (frontend == 1) {
    if (!nzchar(Sys.getenv("ling_inst"))) {
      message("You don't have a default Lingva instance.")
      cat("\n")
      message("Would you like to select the official one or to specify a custom one?")
      cat("\n")
      message("Please, keep in mind that the mantainer of the official one has asked to not abuse the API as the costs of the bandwidth are expensive.")

      instance <- utils::menu(
        choices = c(
          "I'm going to use responsibly the official one, lingva.thedaviddelta.com",
          "I'd prefer specifying a custom one"
        )
      )

      if (instance == 1) {
        message("Selected the official one.")
        Sys.setenv(ling_inst = "https://lingva.thedaviddelta.com")

      } else {
        ling_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", ling_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          ling_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", ling_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }
        Sys.setenv(ling_inst = ling_instance)
      }
    }

    req <- httr2::request(Sys.getenv("ling_inst")) |>
      httr2::req_url_path_append("/api/v1/languages") |>
      httr2::req_perform()

    resp_details <- req |>
      httr2::resp_body_json()

    languages <- resp_details$languages

    return(cat(paste("These are the available languages in your configured instance: ", "\n",
                     languages, collapse = "\n")))

  } else {
    if (!nzchar(Sys.getenv("st_inst"))) {
      message("You did not specify any SimplyTranslate instance.")
      cat("\n")
      message("Would you like to select the official one or to specify a custom one?")

      instance <- utils::menu(
        choices = c(
          "Use the official one, simplytranslate.org",
          "I'd prefer specifying a custom one"
        )
      )

      if (instance == 1) {

        Sys.setenv(st_inst = "https://simplytranslate.org")
        message("Using the default one, simplytranslate.org")

      } else {
        st_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", st_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          st_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", st_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }

        Sys.setenv(st_inst = st_instance)
      }
    }

    req <- httr2::request(Sys.getenv("st_inst")) |>
      httr2::req_url_path_append("/api/target_languages") |>
      httr2::req_perform()

    resp <- req |>
      httr2::resp_body_json()

    lang_list <- lapply(names(resp), function(key) {
      paste(key, ": ", resp[[key]], sep = "")
    })

    languages <- paste(lang_list, collapse = "\n")
    return(cat(languages))
  }
}

