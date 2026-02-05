#' Extract Disease Model Parameters and Create sysdata.rda
#'
#' This script:
#' 1. Extracts parameter tables from disease model functions
#' 2. Stores tables >5 rows in sysdata.rda
#' 3. Generates optimized helper functions with custom parameter support
#'
#' Run with: source("data-raw/extract_disease_parameters.R")

library(data.table)

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SYSDATA_THRESHOLD <- 5L

DISEASE_MODELS <- c(
  "bacterial_blight",
  "brown_spot",
  "leaf_blast",
  "sheath_blight",
  "tungro",
  "modified_kim_leaf_blast",
  "modified_kim_sheath_blight",
  "leaf_rust",
  "s_tritici_blotch"
)

# ==============================================================================
# PARAMETER EXTRACTION
# ==============================================================================

extract_disease_params <- function(func_name) {
  message("Extracting parameters from: ", func_name)

  func <- get(func_name, envir = asNamespace("epicrop"))
  func_body <- body(func)

  # Extract inline assignments (for wheat diseases)
  inline_params <- list()
  seir_call <- NULL

  for (i in seq_along(func_body)) {
    expr <- func_body[[i]]

    # Check for assignments: var <- value
    if (is.call(expr) && expr[[1]] == as.name("<-")) {
      var_name <- as.character(expr[[2]])
      var_value <- tryCatch(
        eval(expr[[3]], envir = new.env()),
        error = function(e) NULL
      )

      if (!is.null(var_value) && is.matrix(var_value) && ncol(var_value) == 2) {
        # Map variable names to parameter names
        param_name <- if (grepl("RcA", var_name)) {
          "RcA"
        } else if (grepl("RcT", var_name)) {
          "RcT"
        } else if (grepl("RRS", var_name)) {
          "RRS"
        } else if (grepl("RRG", var_name)) {
          "RRG"
        } else {
          NULL
        }

        if (!is.null(param_name)) {
          inline_params[[param_name]] <- list(
            type = "table",
            value = var_value,
            nrows = nrow(var_value),
            store_in_sysdata = nrow(var_value) > SYSDATA_THRESHOLD
          )
        }
      }
    }

    # Find seir() call
    if (
      is.call(expr) && as.character(expr[[1]]) %in% c("seir", "epicrop_sim")
    ) {
      seir_call <- expr
    }
  }

  if (is.null(seir_call)) {
    stop("Could not find seir() call in ", func_name)
  }

  # Extract parameters from seir() call
  seir_params <- list()
  param_names <- names(seir_call)[-1]

  for (param_name in param_names) {
    param_expr <- seir_call[[param_name]]

    # Skip variable references
    if (is.name(param_expr)) {
      next
    }

    # Try to evaluate
    param_value <- tryCatch(
      eval(param_expr, envir = new.env()),
      error = function(e) NULL
    )

    if (is.null(param_value)) {
      # Handle function/list definitions
      if (is.call(param_expr) && param_expr[[1]] == as.name("function")) {
        param_value <- eval(param_expr)
        seir_params[[param_name]] <- list(
          type = "function",
          value = param_value,
          store_in_sysdata = FALSE
        )
      } else if (is.call(param_expr) && param_expr[[1]] == as.name("list")) {
        param_value <- eval(param_expr)
        seir_params[[param_name]] <- list(
          type = "list",
          value = param_value,
          store_in_sysdata = FALSE
        )
      }
    } else {
      # Classify by type
      if (is.matrix(param_value) && ncol(param_value) == 2) {
        seir_params[[param_name]] <- list(
          type = "table",
          value = param_value,
          nrows = nrow(param_value),
          store_in_sysdata = nrow(param_value) > SYSDATA_THRESHOLD
        )
      } else if (is.function(param_value)) {
        seir_params[[param_name]] <- list(
          type = "function",
          value = param_value,
          store_in_sysdata = FALSE
        )
      } else if (is.list(param_value)) {
        seir_params[[param_name]] <- list(
          type = "list",
          value = param_value,
          store_in_sysdata = FALSE
        )
      } else if (
        is.logical(param_value) ||
          is.numeric(param_value) ||
          is.character(param_value)
      ) {
        seir_params[[param_name]] <- list(
          type = "scalar",
          value = param_value,
          store_in_sysdata = FALSE
        )
      }
    }
  }

  # Merge inline and seir params (inline takes precedence)
  c(
    inline_params,
    seir_params[setdiff(names(seir_params), names(inline_params))]
  )
}

# ==============================================================================
# EXTRACT ALL PARAMETERS
# ==============================================================================

all_params <- lapply(DISEASE_MODELS, extract_disease_params)
names(all_params) <- DISEASE_MODELS

# ==============================================================================
# BUILD SYSDATA OBJECTS
# ==============================================================================

sysdata_objects <- list()

for (disease in DISEASE_MODELS) {
  for (param_name in names(all_params[[disease]])) {
    param_info <- all_params[[disease]][[param_name]]

    if (isTRUE(param_info$store_in_sysdata)) {
      obj_name <- paste0(".", param_name, "_", disease)
      sysdata_objects[[obj_name]] <- param_info$value

      message(sprintf(
        "  → %s: %s (%d rows) → sysdata.rda",
        disease,
        param_name,
        param_info$nrows
      ))
    }
  }
}

# ==============================================================================
# CREATE SYSDATA.RDA
# ==============================================================================

if (length(sysdata_objects) > 0) {
  message(
    "\n=== Creating sysdata.rda with ",
    length(sysdata_objects),
    " objects ===\n"
  )

  for (obj_name in names(sysdata_objects)) {
    obj <- sysdata_objects[[obj_name]]
    message(sprintf("  %s: %d × %d matrix", obj_name, nrow(obj), ncol(obj)))
  }

  if (!dir.exists("R")) {
    dir.create("R", recursive = TRUE)
  }

  sysdata_env <- new.env(parent = emptyenv())
  for (obj_name in names(sysdata_objects)) {
    assign(obj_name, sysdata_objects[[obj_name]], envir = sysdata_env)
  }

  save(
    list = names(sysdata_objects),
    file = file.path("R", "sysdata.rda"),
    envir = sysdata_env,
    compress = "bzip2",
    version = 2
  )

  file_size <- file.size(file.path("R", "sysdata.rda")) / 1024
  message(sprintf("\n✓ Created R/sysdata.rda (%.1f KB)\n", file_size))
} else {
  message("\n⚠ No parameters exceed threshold for sysdata.rda\n")
}

# ==============================================================================
# GENERATE HELPER FUNCTIONS
# ==============================================================================

format_matrix <- function(mat, indent = "  ") {
  code <- paste0(indent, "cbind(\n")

  col1 <- paste(
    format(mat[, 1], scientific = FALSE, trim = TRUE),
    collapse = ", "
  )
  code <- paste0(code, indent, "  c(", col1, "),\n")

  col2 <- paste(
    format(mat[, 2], scientific = FALSE, trim = TRUE),
    collapse = ", "
  )
  code <- paste0(code, indent, "  c(", col2, ")\n")

  code <- paste0(code, indent, ")")
  code
}

generate_function <- function(disease, params) {
  code <- character()

  # Identify customizable parameters
  customizable <- c("RcA", "RcT", "RRG", "RRS")
  has_custom <- intersect(names(params), customizable)

  # Function signature
  sig <- sprintf("%s <- function(wth, emergence", disease)
  for (p in has_custom) {
    sig <- paste0(sig, ", ", p, " = NULL")
  }
  sig <- paste0(sig, ", ...) {")

  code <- c(code, "#' @rdname disease_models", "#' @export", sig, "")

  # Local defaults (functions only)
  for (param_name in has_custom) {
    param_info <- params[[param_name]]
    if (param_info$type == "function") {
      func_code <- deparse(param_info$value)
      code <- c(
        code,
        sprintf(
          "  %s_default <- %s",
          param_name,
          paste(func_code, collapse = "\n  ")
        ),
        ""
      )
    }
  }

  # seir() call
  code <- c(code, "  seir(", "    wth = wth,", "    emergence = emergence,")

  for (param_name in names(params)) {
    param_info <- params[[param_name]]

    if (param_name %in% has_custom) {
      # Customizable parameter
      if (param_info$store_in_sysdata) {
        obj_name <- paste0(".", param_name, "_", disease)
        code <- c(
          code,
          sprintf("    %s = %s %%||%% %s,", param_name, param_name, obj_name)
        )
      } else if (param_info$type == "function") {
        code <- c(
          code,
          sprintf(
            "    %s = %s %%||%% %s_default,",
            param_name,
            param_name,
            param_name
          )
        )
      } else if (param_info$type == "table") {
        # Small table - inline
        code <- c(code, sprintf("    %s = %s %%||%% ", param_name, param_name))
        code <- c(code, format_matrix(param_info$value, "      "), ",")
      }
    } else if (param_info$type == "scalar") {
      # Inline scalar
      value <- param_info$value
      if (is.character(value)) {
        code <- c(code, sprintf("    %s = \"%s\",", param_name, value))
      } else if (is.integer(value)) {
        code <- c(code, sprintf("    %s = %dL,", param_name, value))
      } else if (is.logical(value)) {
        code <- c(code, sprintf("    %s = %s,", param_name, toupper(value)))
      } else {
        code <- c(
          code,
          sprintf("    %s = %s,", param_name, format(value, scientific = FALSE))
        )
      }
    } else if (param_info$type == "list") {
      # Format list
      parts <- character()
      for (n in names(param_info$value)) {
        v <- param_info$value[[n]]
        if (is.character(v)) {
          parts <- c(parts, sprintf("%s = \"%s\"", n, v))
        } else if (is.integer(v)) {
          parts <- c(parts, sprintf("%s = %dL", n, v))
        } else {
          parts <- c(
            parts,
            sprintf("%s = %s", n, format(v, scientific = FALSE))
          )
        }
      }
      code <- c(
        code,
        sprintf("    %s = list(%s),", param_name, paste(parts, collapse = ", "))
      )
    }
  }

  code <- c(code, "    ...", "  )", "}", "")
  code
}

message("\n=== Generating disease_models.R ===\n")

helper_code <- c(
  "#' Rice and Wheat Disease Models",
  "#'",
  "#' Helper functions with pre-loaded parameter tables for optimal performance.",
  "#' Custom parameter tables can be provided to override defaults.",
  "#'",
  "#' @name disease_models",
  "#' @inheritParams seir",
  "#' @param RcA Optional custom age modifier table (2-column matrix).",
  "#' @param RcT Optional custom temperature modifier table (2-column matrix).",
  "#' @param RRG Optional custom growth rate (scalar, vector, or table).",
  "#' @param RRS Optional custom senescence rate (scalar, vector, or function).",
  "#'",
  "#' @references",
  "#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. (2012).",
  "#' Modeling and mapping potential epidemics of rice diseases globally.",
  "#' *Crop Protection*, 34: 6-17. \\doi{10.1016/j.cropro.2011.11.009}",
  "#'",
  "#' Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015).",
  "#' Predicting potential epidemics of rice leaf blast and sheath blight in South Korea.",
  "#' *Agricultural and Forest Meteorology*, 203: 191-207.",
  "#' \\doi{10.1016/j.agrformet.2015.01.011}",
  "#'",
  "#' Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).",
  "#' Modelling and mapping potential epidemics of wheat diseases.",
  "#' *European Journal of Plant Pathology*, 142: 771-790.",
  "#' \\doi{10.1007/s10658-015-0650-7}",
  "#'",
  "#' @author Adam H. Sparks, \\email{adamhsparks@@gmail.com}",
  "#' @autoglobal",
  "NULL",
  ""
)

for (disease in DISEASE_MODELS) {
  message("  Generating: ", disease)
  helper_code <- c(
    helper_code,
    generate_function(disease, all_params[[disease]])
  )
}

# Add aliases
aliases <- list(
  bb = "bacterial_blight",
  bs = "brown_spot",
  lb = "leaf_blast",
  sb = "sheath_blight",
  tg = "tungro",
  mk_lb = "modified_kim_leaf_blast",
  mk_sb = "modified_kim_sheath_blight",
  lr = "leaf_rust",
  brown_rust = "leaf_rust",
  stb = "s_tritici_blotch"
)

for (alias in names(aliases)) {
  helper_code <- c(
    helper_code,
    "#' @rdname disease_models",
    "#' @export",
    sprintf("%s <- %s", alias, aliases[[alias]]),
    ""
  )
}

writeLines(helper_code, "R/disease_models.R")
message("\n✓ Generated R/disease_models.R\n")

# ==============================================================================
# DOCUMENTATION
# ==============================================================================

doc_lines <- c(
  "# Disease Model Parameters",
  "",
  "## Stored in sysdata.rda",
  "",
  "| Disease | Parameter | Rows |",
  "|---------|-----------|------|"
)

for (disease in DISEASE_MODELS) {
  for (param_name in names(all_params[[disease]])) {
    param_info <- all_params[[disease]][[param_name]]
    if (isTRUE(param_info$store_in_sysdata)) {
      doc_lines <- c(
        doc_lines,
        sprintf("| %s | %s | %d |", disease, param_name, param_info$nrows)
      )
    }
  }
}

doc_lines <- c(
  doc_lines,
  "",
  sprintf(
    "**Total:** %d parameter tables (%.1f KB)",
    length(sysdata_objects),
    sum(sapply(sysdata_objects, object.size)) / 1024
  )
)

writeLines(doc_lines, "data-raw/DISEASE_PARAMETERS.md")
message("✓ Created data-raw/DISEASE_PARAMETERS.md\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

message("=== SUMMARY ===")
message("✓ Extracted ", length(DISEASE_MODELS), " disease models")
message("✓ Created sysdata.rda with ", length(sysdata_objects), " tables")
message("✓ Generated R/disease_models.R")
message("\nNext: devtools::document() && devtools::load_all()\n")
