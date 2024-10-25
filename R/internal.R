#' Check if SimpleTranslate instance was declared or use system default
#'
#' @param instance URL of the instance
#'
#' @noRd

check_st_instance <- function(instance = NULL) {
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
        check <- "sys"
        return("sys")

      } else if (instance == 2) {
        st_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", st_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          st_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", st_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }
        Sys.setenv(st_inst = st_instance)
        check <- "sys"
        return(check)

      }
    } else {
      check <- "sys"
      return(check)
    }
  } else if (!is.null(instance)) {
    if (!grepl("^https?://", instance)) {

      message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      st_instance <- readline("Please, type it again: ")

      if (!grepl("^https?://", st_instance)) {
        stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      }
    }

    req <- httr2::request(st_instance) |>
      httr2::req_url_path_append("/api/translate") |>
      httr2::req_url_query(engine = "google", from = "en", to = "es", text = "hello") |>
      httr2::req_perform()

    if (req$status_code == 200) {
      return(st_instance)
    } else {
      stop("The provided URL is not a SimpleTranslate instance or it is not working right now.")
    }
  }
}

#' Check SimpleTranslate engine
#'
#' @param engine Translation engine
#'
#' @noRd
check_st_engine <- function(engine = NULL) {

  if(is.null(engine)) {
    if (!nzchar(Sys.getenv("st_eng"))) {
      message("You did not select any of the available translation engines.")
      cat("\n")
      message("Please, select one. Keep in mind that the selected one will be the default engine for this session unless you declare another one in the function.")

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
        check <- "sys"
        return(check)
      } else if (engine == 2) {
        Sys.setenv(st_eng = "iciba")
        message("Using iCIBA engine")
        check <- "sys"
        return(check)
      } else if (engine == 3) {
        Sys.setenv(st_eng = "reverso")
        message("Using Reverso engine")
        check <- "sys"
        return(check)
      }
    }
  } else if (!is.null(engine)) {
    allowed <- "google|iciba|reverso"
    if (grepl(allowed, engine)) {
      return(engine)
    } else {
      stop("You have not submitted any valid engine. Only available options are google,
           iciba and reverso. Check your spell and try again")
    }
  }
}


#' Check Mozhi instance
#'
#' @param instance URL of the Mozhi instance
#'
#' @noRd
check_mz_instance <- function(instance = NULL) {
  if (is.null(instance)) {
    if (!nzchar(Sys.getenv("mz_inst"))) {
      message("You did not specify any Mozhi instance.")
      cat("\n")
      message("Would you like to select the Mozhi's mantainer one or to specify a custom one?")

      instance_menu <- utils::menu(
        choices = c(
          "Use the Mozhi's mantainer one, mozhi.aryak.me",
          "I'd prefer specifying a custom one"
        )
      )

      if (instance_menu == 1) {

        Sys.setenv(mz_inst = "https://mozhi.aryak.me")
        message("Using the default one, mozhi.aryak.me")
        check <- "sys"
        return(check)

      } else if (instance_menu == 2) {
        mz_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", mz_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          mz_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", mz_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }
        Sys.setenv(mz_inst = mz_instance)
        check <- "sys"
        return(check)
      }
    } else {
      check <- "sys"
      return(check)
    }
  } else if (!is.null(instance)) {
    if (!grepl("^https?://", instance)) {

      message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      mz_instance <- readline("Please, type it again: ")

      if (!grepl("^https?://", mz_instance)) {
        stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
      }
    } else {
      mz_instance <- instance
    }

    req <- httr2::request(mz_instance) |>
      httr2::req_url_path_append("/api/translate") |>
      httr2::req_url_query(engine = "duckduckgo", from = "en", to = "es", text = "hello") |>
      httr2::req_perform()

    if (req$status_code == 200) {
      return(mz_instance)
    } else {
      stop("The provided URL is not a Mozhi instance or it is not working right now.")
    }
  }
}

#' Check Mozhi engine
#'
#' @param instance Selected Mozhi engine
#'
#' @noRd
check_mz_engine <- function(engine = NULL, instance = NULL) {

  availables <- suppressMessages(get_mozhi_engines(instance = instance))

  if(is.null(engine)) {
    if (!nzchar(Sys.getenv("mz_eng"))) {
      cat("\n")
      message("You did not select any of the available translation engines.")
      cat("\n")
      message("Please, select one. Keep in mind that the selected one will be the default engine for this session unless you declare another one on the function.")
      cat("\n")
      message("Take into account that if you want to use all engines at once, you will need to run `mozhi_all()` and that")
      message("the option 'some engines' is not available.")

      engine <- utils::menu(
        choices = availables
      )

      selected_engine <- availables[engine]
      Sys.setenv(mz_eng = names(selected_engine))
      message("Using ", selected_engine, " engine")
      check <- "sys"
      return(check)
    } else {
      check <- "sys"
      return(check)
    }
  } else if (!is.null(engine)) {
    allowed <- paste(names(availables), collapse = "|")
    if (grepl(allowed, engine)) {
      return(engine)
    } else {
      stop("You have not submitted any valid engine. Only available options are ",
           paste(names(availables), collapse = ", "),
        ". Check your spell and try again")
    }
  }
}

#' Check Lingva instance
#'
#' @param instance URL of the Lingva instance
#' @noRd

check_ling_instance <- function(instance = NULL) {

  if (is.null(instance)) {
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

      if (instance == 1) {
        message("Selected the official one.")
        Sys.setenv(ling_inst = "https://lingva.thedaviddelta.com")
        check <- "sys"
        return(check)
      } else if (instance == 2) {
        ling_instance <- readline("Please, write the URL of your instance, including http or https: ")

        if (!grepl("^https?://", ling_instance)) {

          message("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          ling_instance <- readline("Please, type it again: ")

          if (!grepl("^https?://", ling_instance)) {
            stop("You have not provided a valid URL. Check it and be sure that contains http:// or https://.")
          }
        }
        Sys.setenv(ling_inst = ling_instance)
        check <- "sys"
        return(check)
      }
    } else {
      check <- "sys"
      return(check)
    }
  } else if (!is.null(instance)) {
    req <- httr2::request(instance) |>
      httr2::req_url_path_append("/api/v1") |>
      httr2::req_url_path_append(from = "en", to = "es", str = "hello")
      httr2::req_perform()

      if (req$status_code == 200) {
        return(instance)
      } else {
        stop("The provided URL is not a Lingva instance or it is not working right now.")
      }
  }
}
