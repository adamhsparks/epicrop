if (requireNamespace("spelling", quietly = TRUE)) {
  if (tolower(Sys.getenv("NOT_CRAN")) %in% c("1", "yes", "true")) {
    dict <- tryCatch(
      hunspell::dictionary("en_AU"),
      error = function(e) {
        stop(
          "Could not load the en_AU Hunspell dictionary: ",
          conditionMessage(e),
          ". See CONTRIBUTING.md for setup instructions.",
          call. = FALSE
        )
      }
    )
    if (hunspell::hunspell_info(dict)$encoding != "UTF-8") {
      stop(
        "Spell checking requires a UTF-8 en_AU Hunspell dictionary. ",
        "See CONTRIBUTING.md for setup instructions.",
        call. = FALSE
      )
    }
  }

  spelling::spell_check_test(
    vignettes = TRUE,
    error = FALSE,
    skip_on_cran = TRUE
  )
}
