#' @title Claude Code CLI Interface
#' @description Functions to interact with Claude Code, Anthropic's agentic coding CLI tool.
#' @name claude_code
#' @family claude_code
NULL

# ============================================================================
# Internal Formatting Helpers
# ============================================================================

.cc_header <- function(text, width = 60, char = "\u2500") {
  pad <- max(0, width - nchar(text) - 4)
  left_pad <- floor(pad / 2)
  right_pad <- ceiling(pad / 2)
  paste0(strrep(char, left_pad), " ", text, " ", strrep(char, right_pad))
}

.cc_kv <- function(key, value, key_width = 15) {
  sprintf("  %-*s : %s", key_width, key, value)
}

.cc_item <- function(text, bullet = "\u2022") {

  paste0("  ", bullet, " ", text)
}

.cc_success <- function(text) paste0("\u2713 ", text)
.cc_error <- function(text) paste0("\u2717 ", text)
.cc_warn <- function(text) paste0("\u26A0 ", text)
.cc_info <- function(text) paste0("\u2139 ", text)
.cc_ln <- function(..., sep = "") cat(..., "\n", sep = sep)

`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================================
# Configuration
# ============================================================================

#' Configure Claude Code CLI settings
#'
#' Get or set configuration options for Claude Code CLI integration.
#'
#' @param cli_path Path to the claude executable. If NULL, searches PATH.
#' @param default_model Default model to use for Claude Code operations.
#' @param timeout Default timeout in seconds.
#' @param quiet Logical; if TRUE, suppress configuration messages.
#'
#' @return A list containing current configuration settings. Returns invisibly
#'   when setting values.
#'
#' @examples
#' \dontrun{
#' # View current configuration
#' claude_code_config()
#'
#' # Set custom path and timeout
#' claude_code_config(cli_path = "/usr/local/bin/claude", timeout = 600)
#' }
#'
#' @family claude_code
#' @export
claude_code_config <- function(cli_path = NULL,
                               default_model = NULL,
                               timeout = NULL,
                               quiet = FALSE) {

  if (!exists(".claude_code_config", envir = .GlobalEnv)) {
    assign(".claude_code_config", new.env(parent = emptyenv()), envir = .GlobalEnv)
    cfg <- get(".claude_code_config", envir = .GlobalEnv)
    cfg$path <- Sys.which("claude")
    cfg$model <- NULL
    cfg$timeout <- 300
  }

  cfg <- get(".claude_code_config", envir = .GlobalEnv)
  setting <- !is.null(cli_path) || !is.null(default_model) || !is.null(timeout)

  if (!is.null(cli_path)) cfg$path <- cli_path
  if (!is.null(default_model)) cfg$model <- default_model
  if (!is.null(timeout)) cfg$timeout <- timeout

  result <- list(path = cfg$path, model = cfg$model, timeout = cfg$timeout)

  if (setting && !quiet) .cc_ln(.cc_success("Configuration updated"))
  if (setting) invisible(result) else result
}

#' Print Claude Code configuration
#'
#' Display current Claude Code CLI configuration in a formatted way.
#'
#' @return Invisibly returns the configuration list.
#'
#' @family claude_code
#' @export
claude_code_config_print <- function() {
  cfg <- claude_code_config()
  avail <- claude_code_available()

  cat("\n")
  .cc_ln(.cc_header("Claude Code Configuration"))
  cat("\n")
  .cc_ln(.cc_kv("CLI Path", if (nzchar(cfg$path)) cfg$path else "(not found)"))
  .cc_ln(.cc_kv("Status", if (avail) .cc_success("Available") else .cc_error("Not available")))
  .cc_ln(.cc_kv("Default Model", cfg$model %||% "(auto)"))
  .cc_ln(.cc_kv("Timeout", paste0(cfg$timeout, " seconds")))
  cat("\n")
  if (!avail) .cc_ln(.cc_warn("Install: npm install -g @anthropic-ai/claude-code"))
  cat("\n")

  invisible(cfg)
}

#' Check if Claude Code CLI is available
#'
#' @param verbose Logical; if TRUE, print status message.
#' @return Logical indicating whether the Claude Code CLI is available.
#'
#' @family claude_code
#' @export
claude_code_available <- function(verbose = FALSE) {
  cfg <- claude_code_config()
  avail <- nzchar(cfg$path) && file.exists(cfg$path)

  if (verbose) {
    if (avail) {
      .cc_ln(.cc_success("Claude Code CLI is available"))
    } else {
      .cc_ln(.cc_error("Claude Code CLI not found"))
      .cc_ln(.cc_info("Install with: npm install -g @anthropic-ai/claude-code"))
    }
  }
  avail
}

#' Get Claude Code CLI version
#'
#' @param verbose Logical; if TRUE, print formatted version info.
#' @return Character string with version info, or NULL if unavailable.
#'
#' @family claude_code
#' @export
claude_code_version <- function(verbose = TRUE) {
  if (!claude_code_available()) {
    if (verbose) {
      .cc_ln(.cc_error("Claude Code CLI not found"))
      .cc_ln(.cc_info("Install with: npm install -g @anthropic-ai/claude-code"))
    }
    return(invisible(NULL))
  }

  result <- tryCatch(
    system2(claude_code_config()$path, "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )

  if (is.null(result)) {
    if (verbose) .cc_ln(.cc_error("Failed to get version"))
    return(invisible(NULL))
  }

  version <- paste(result, collapse = " ")
  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Claude Code"))
    cat("\n")
    .cc_ln(.cc_kv("Version", version))
    cat("\n")
  }
  invisible(version)
}

# ============================================================================
# Pipe Context Management
# ============================================================================

#' Get or initialize the pipe context environment
#'
#' The pipe context is a persistent environment that accumulates objects
#' across chained \code{claude_pipe()} calls. This allows you to reference
#' objects created in previous pipes in subsequent operations.
#'
#' @return The pipe context environment (invisibly when setting).
#'
#' @examples
#' \dontrun{
#' # Objects created in pipes are available in subsequent pipes
#' mtcars |>
#'   claude_pipe("Create a linear model called 'fit' for mpg ~ wt + hp") |>
#'   claude_pipe("Create a summary of the 'fit' model from the previous step")
#'
#' # Check what objects are available in the context
#' claude_pipe_context_objects()
#'
#' # Clear the context to start fresh
#' claude_pipe_context_clear()
#' }
#'
#' @family claude_code
#' @export
claude_pipe_context <- function() {
  if (!exists(".claude_code_config", envir = .GlobalEnv)) {
    claude_code_config(quiet = TRUE)
  }

  cfg <- get(".claude_code_config", envir = .GlobalEnv)

  if (is.null(cfg$pipe_context) || !is.environment(cfg$pipe_context)) {
    cfg$pipe_context <- new.env(parent = .GlobalEnv)
  }

  cfg$pipe_context
}

#' Clear the pipe context
#'
#' Removes all objects from the pipe context environment, allowing you to
#' start fresh with a new chain of pipe operations.
#'
#' @param verbose Logical; if TRUE, print confirmation message.
#' @return Invisibly returns TRUE.
#'
#' @examples
#' \dontrun{
#' # Clear context before starting a new analysis
#' claude_pipe_context_clear()
#'
#' mtcars |> claude_pipe("Create a summary")
#' }
#'
#' @family claude_code
#' @export
claude_pipe_context_clear <- function(verbose = TRUE) {
  ctx <- claude_pipe_context()
  rm(list = ls(ctx, all.names = TRUE), envir = ctx)
  if (verbose) .cc_ln(.cc_success("Pipe context cleared"))
  invisible(TRUE)
}

#' List objects in the pipe context
#'
#' Shows all objects currently available in the pipe context from previous
#' pipe operations.
#'
#' @param verbose Logical; if TRUE, print formatted object info.
#' @return A named list with object names, classes, and brief descriptions.
#'
#' @examples
#' \dontrun{
#' # See what objects are available from previous pipes
#' claude_pipe_context_objects()
#' }
#'
#' @family claude_code
#' @export
claude_pipe_context_objects <- function(verbose = TRUE) {
  ctx <- claude_pipe_context()
  obj_names <- ls(ctx, all.names = FALSE)

  # Check for original data
  has_original_data <- exists(".original_data", envir = ctx)

  if (length(obj_names) == 0 && !has_original_data) {
    if (verbose) .cc_ln(.cc_info("Pipe context is empty"))
    return(invisible(list()))
  }

  obj_info <- lapply(obj_names, function(nm) {
    obj <- get(nm, envir = ctx)
    cls <- class(obj)[1]
    desc <- if (is.data.frame(obj)) {
      sprintf("data.frame [%d x %d]", nrow(obj), ncol(obj))
    } else if (inherits(obj, "lm")) {
      sprintf("lm (%s)", deparse(obj$call)[1])
    } else if (inherits(obj, "glm")) {
      sprintf("glm (%s)", deparse(obj$call)[1])
    } else if (is.function(obj)) {
      "function"
    } else if (is.list(obj)) {
      sprintf("list [%d elements]", length(obj))
    } else if (is.vector(obj)) {
      sprintf("%s [length %d]", cls, length(obj))
    } else {
      cls
    }
    list(name = nm, class = cls, description = desc)
  })

  names(obj_info) <- obj_names

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Pipe Objects (available in R environment)"))
    cat("\n")

    # Show original data first if present
    if (has_original_data) {
      orig <- get(".original_data", envir = ctx)
      orig_name <- if (exists(".original_data_name", envir = ctx)) {
        get(".original_data_name", envir = ctx)
      } else {
        "data"
      }
      if (is.data.frame(orig)) {
        .cc_ln(.cc_kv(".original_data", sprintf("data.frame [%d x %d] (from %s)", nrow(orig), ncol(orig), orig_name)))
      }
    }

    for (info in obj_info) {
      .cc_ln(.cc_kv(info$name, info$description))
    }
    cat("\n")
  }

  invisible(obj_info)
}

# Internal: Format pipe context info for Claude
.cc_format_context_info <- function(ctx) {
  obj_names <- ls(ctx, all.names = FALSE)

  # Check for original data (stored with leading dot)
  has_original_data <- exists(".original_data", envir = ctx)
  original_data_info <- NULL

  if (has_original_data) {
    orig <- get(".original_data", envir = ctx)
    orig_name <- if (exists(".original_data_name", envir = ctx)) {
      get(".original_data_name", envir = ctx)
    } else {
      "original_data"
    }
    if (is.data.frame(orig)) {
      cols <- paste(names(orig), collapse = ", ")
      original_data_info <- sprintf(
        "ORIGINAL PIPED DATA (available as `.original_data`):\n  - %s: data.frame [%d rows x %d cols] with columns: %s\n  - Use `.original_data` when you need to access the original data frame columns (e.g., mpg, cyl)",
        orig_name, nrow(orig), ncol(orig), cols
      )
    }
  }

  if (length(obj_names) == 0 && is.null(original_data_info)) {
    return(NULL)
  }

  obj_descriptions <- if (length(obj_names) > 0) {
    sapply(obj_names, function(nm) {
      obj <- get(nm, envir = ctx)
      cls <- class(obj)[1]
      if (is.data.frame(obj)) {
        cols <- paste(names(obj), collapse = ", ")
        sprintf("  - %s: data.frame [%d rows x %d cols] with columns: %s",
                nm, nrow(obj), ncol(obj), cols)
      } else if (inherits(obj, "lm")) {
        sprintf("  - %s: linear model (%s)", nm, deparse(obj$call)[1])
      } else if (inherits(obj, "glm")) {
        sprintf("  - %s: generalized linear model (%s)", nm, deparse(obj$call)[1])
      } else if (inherits(obj, "ggplot")) {
        sprintf("  - %s: ggplot object", nm)
      } else if (is.function(obj)) {
        sprintf("  - %s: function", nm)
      } else if (is.list(obj)) {
        sprintf("  - %s: list with %d elements", nm, length(obj))
      } else if (is.vector(obj)) {
        sprintf("  - %s: %s vector [length %d]", nm, cls, length(obj))
      } else if (is.matrix(obj)) {
        sprintf("  - %s: matrix [%d x %d]", nm, nrow(obj), ncol(obj))
      } else {
        sprintf("  - %s: %s", nm, cls)
      }
    })
  } else {
    character(0)
  }

  parts <- c()
  if (!is.null(original_data_info)) {
    parts <- c(parts, original_data_info)
  }
  if (length(obj_descriptions) > 0) {
    parts <- c(parts, paste0(
      "Objects created in previous pipe operations:\n",
      paste(obj_descriptions, collapse = "\n")
    ))
  }

  paste0(
    paste(parts, collapse = "\n\n"),
    "\n\nYou can reference these objects directly by name in your code."
  )
}

# ============================================================================
# Core Execution
# ============================================================================

#' Execute a prompt with Claude Code CLI
#'
#' Send a prompt to Claude Code CLI and receive a response. This function
#' provides access to Claude's agentic coding capabilities including file
#' operations, code execution, and multi-turn interactions.
#'
#' @param prompt The prompt to send to Claude.
#' @param print Logical; if TRUE (default), returns response without streaming.
#' @param output_format Output format: "text", "json", or "stream-json".
#' @param model Model to use (overrides default from config).
#' @param max_turns Maximum number of agentic turns.
#' @param system_prompt Custom system prompt.
#' @param append_system_prompt Additional system prompt to append.
#' @param allowed_tools Character vector of allowed tools.
#' @param disallowed_tools Character vector of disallowed tools.
#' @param mcp_config Path to MCP configuration file.
#' @param permission_mode Permission mode: "default", "acceptEdits", or "bypassPermissions".
#' @param working_dir Working directory for Claude to operate in.
#' @param timeout Timeout in seconds.
#' @param verbose Logical; if TRUE, print status messages.
#'
#' @return Response from Claude. Character string for text output, list for JSON.
#'
#' @details
#' This function requires the Claude Code CLI to be installed. Install with:
#' \code{npm install -g @anthropic-ai/claude-code}
#'
#' For simple API-based completions without agentic capabilities, use
#' \code{\link{claudeR}} instead.
#'
#' @examples
#' \dontrun{
#' # Simple prompt
#' claude_code("What files are in the current directory?")
#'
#' # With JSON output
#' result <- claude_code("List 3 R packages for data viz", output_format = "json")
#'
#' # With custom system prompt
#' claude_code("Review this code",
#'             system_prompt = "You are a senior R developer")
#'
#' # Restrict to specific tools
#' claude_code("Analyze data.csv",
#'             allowed_tools = c("Read", "Bash"))
#' }
#'
#' @family claude_code
#' @seealso \code{\link{claudeR}} for API-based completions
#' @export
claude_code <- function(prompt,
                        print = TRUE,
                        output_format = c("text", "json", "stream-json"),
                        model = NULL,
                        max_turns = NULL,
                        system_prompt = NULL,
                        append_system_prompt = NULL,
                        allowed_tools = NULL,
                        disallowed_tools = NULL,
                        mcp_config = NULL,
                        permission_mode = NULL,
                        working_dir = NULL,
                        timeout = NULL,
                        verbose = FALSE) {

  if (!claude_code_available()) {
    .cc_ln(.cc_error("Claude Code CLI not found"))
    .cc_ln(.cc_info("Install with: npm install -g @anthropic-ai/claude-code"))
    stop("Claude Code CLI not available", call. = FALSE)
  }

  output_format <- match.arg(output_format)
  cfg <- claude_code_config()

  args <- c("-p", shQuote(prompt))
  if (print) args <- c(args, "--print")
  if (output_format != "text") args <- c(args, "--output-format", output_format)
  if (!is.null(model) || !is.null(cfg$model)) args <- c(args, "--model", model %||% cfg$model)
  if (!is.null(max_turns)) args <- c(args, "--max-turns", as.character(max_turns))
  if (!is.null(system_prompt)) args <- c(args, "--system-prompt", shQuote(system_prompt))
  if (!is.null(append_system_prompt)) args <- c(args, "--append-system-prompt", shQuote(append_system_prompt))
  if (!is.null(allowed_tools)) args <- c(args, "--allowedTools", paste(allowed_tools, collapse = ","))
  if (!is.null(disallowed_tools)) args <- c(args, "--disallowedTools", paste(disallowed_tools, collapse = ","))
  if (!is.null(mcp_config)) args <- c(args, "--mcp-config", mcp_config)

  if (!is.null(permission_mode)) {
    permission_mode <- match.arg(permission_mode, c("default", "acceptEdits", "bypassPermissions"))
    args <- c(args, "--permission-mode", permission_mode)
  }

  wd <- working_dir %||% getwd()
  timeout_val <- timeout %||% cfg$timeout

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Executing Claude Code"))
    cat("\n")
    .cc_ln(.cc_kv("Working Dir", wd))
    .cc_ln(.cc_kv("Timeout", paste0(timeout_val, "s")))
    .cc_ln(.cc_kv("Output Format", output_format))
    cat("\n")
    .cc_ln(.cc_info("Running..."))
  }

  result <- tryCatch({
    if (requireNamespace("processx", quietly = TRUE)) {
      proc <- processx::run(
        command = cfg$path,
        args = gsub("^'|'$", "", args),
        wd = wd,
        timeout = timeout_val,
        error_on_status = FALSE
      )
      list(stdout = proc$stdout, stderr = proc$stderr, status = proc$status)
    } else {
      stdout_file <- tempfile()
      stderr_file <- tempfile()
      on.exit(unlink(c(stdout_file, stderr_file)), add = TRUE)

      status <- system2(cfg$path, args = args,
                        stdout = stdout_file, stderr = stderr_file,
                        wait = TRUE, timeout = timeout_val)

      list(
        stdout = paste(readLines(stdout_file, warn = FALSE), collapse = "\n"),
        stderr = paste(readLines(stderr_file, warn = FALSE), collapse = "\n"),
        status = status
      )
    }
  }, error = function(e) {
    .cc_ln(.cc_error(paste("Execution failed:", e$message)))
    stop("Claude Code execution failed: ", e$message, call. = FALSE)
  })

  if (verbose) {
    if (result$status == 0) .cc_ln(.cc_success("Complete"))
    else .cc_ln(.cc_warn(paste("Exit status:", result$status)))
    cat("\n")
  }

  if (result$status != 0 && nzchar(result$stderr)) {
    warning("Claude Code returned non-zero exit status: ", result$status,
            "\nStderr: ", result$stderr, call. = FALSE)
  }

  output <- result$stdout

  if (output_format %in% c("json", "stream-json")) {
    tryCatch(
      jsonlite::fromJSON(output, simplifyVector = FALSE),
      error = function(e) {
        warning("Failed to parse JSON output: ", e$message, call. = FALSE)
        output
      }
    )
  } else {
    output
  }
}

# ============================================================================
# Session Management
# ============================================================================

#' Resume a Claude Code session
#'
#' Continue a previous Claude Code conversation session.
#'
#' @param prompt The prompt to send.
#' @param session_id Session ID to resume. Use "last" for most recent session.
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Response from Claude.
#'
#' @family claude_code
#' @export
claude_code_session <- function(prompt, session_id = "last", verbose = FALSE, ...) {
  if (!claude_code_available()) stop("Claude Code CLI not found", call. = FALSE)

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Resuming Session"))
    .cc_ln(.cc_kv("Session", session_id))
    cat("\n")
  }

  cfg <- claude_code_config()
  args <- c("--resume", session_id, "-p", shQuote(prompt), "--print")

  result <- system2(cfg$path, args = args, stdout = TRUE, stderr = TRUE)
  paste(result, collapse = "\n")
}

#' List Claude Code sessions
#'
#' @param verbose Logical; print formatted session list.
#' @return Data frame of sessions or NULL if none found.
#'
#' @family claude_code
#' @export
claude_code_sessions <- function(verbose = TRUE) {
  if (!claude_code_available()) stop("Claude Code CLI not found", call. = FALSE)

  sessions_dir <- file.path(Sys.getenv("HOME"), ".claude", "projects")

  if (!dir.exists(sessions_dir)) {
    if (verbose) .cc_ln(.cc_info("No sessions directory found"))
    return(invisible(NULL))
  }

  sessions <- list.dirs(sessions_dir, recursive = FALSE, full.names = TRUE)

  if (length(sessions) == 0) {
    if (verbose) .cc_ln(.cc_info("No sessions found"))
    return(invisible(NULL))
  }

  df <- data.frame(
    path = sessions,
    name = basename(sessions),
    modified = file.mtime(sessions),
    stringsAsFactors = FALSE
  )
  df <- df[order(df$modified, decreasing = TRUE), ]

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Claude Code Sessions"))
    cat("\n")
    for (i in seq_len(min(10, nrow(df)))) {
      age <- format(round(difftime(Sys.time(), df$modified[i], units = "hours"), 1))
      .cc_ln(.cc_item(sprintf("%s (%s ago)", df$name[i], age)))
    }
    if (nrow(df) > 10) .cc_ln(sprintf("\n  ... and %d more", nrow(df) - 10))
    cat("\n")
  }

  invisible(df)
}

# ============================================================================
# Convenience Functions
# ============================================================================

#' Pipe content to Claude Code
#'
#' Send data frame, list, or text content to Claude Code for analysis.
#'
#' @param content Content to send (data.frame, list, or character).
#' @param prompt Prompt describing what to do with the content.
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Response from Claude.
#'
#' @examples
#' \dontrun{
#' mtcars |> claude_code_pipe("Describe this dataset")
#' readLines("script.R") |> claude_code_pipe("Review this code")
#' }
#'
#' @family claude_code
#' @export
claude_code_pipe <- function(content, prompt, verbose = FALSE, ...) {
  if (verbose) .cc_ln(.cc_info("Preparing content..."))

  if (is.data.frame(content)) {
    content_str <- paste(capture.output(print(content)), collapse = "\n")
    if (verbose) .cc_ln(.cc_kv("Content", sprintf("data.frame [%d x %d]", nrow(content), ncol(content))))
  } else if (is.list(content)) {
    content_str <- jsonlite::toJSON(content, auto_unbox = TRUE, pretty = TRUE)
    if (verbose) .cc_ln(.cc_kv("Content", "list (JSON)"))
  } else {
    content_str <- paste(as.character(content), collapse = "\n")
    if (verbose) .cc_ln(.cc_kv("Content", sprintf("text (%d chars)", nchar(content_str))))
  }

  full_prompt <- paste0("<content>\n", content_str, "\n</content>\n\n", prompt)
  claude_code(full_prompt, verbose = verbose, ...)
}

#' Send file to Claude Code
#'
#' @param file_path Path to file.
#' @param prompt Prompt describing what to do with the file.
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Response from Claude.
#'
#' @family claude_code
#' @export
claude_code_file <- function(file_path, prompt, verbose = FALSE, ...) {
  if (!file.exists(file_path)) {
    .cc_ln(.cc_error(paste("File not found:", file_path)))
    stop("File not found: ", file_path, call. = FALSE)
  }

  if (verbose) {
    fi <- file.info(file_path)
    cat("\n")
    .cc_ln(.cc_header("Analyzing File"))
    cat("\n")
    .cc_ln(.cc_kv("File", basename(file_path)))
    .cc_ln(.cc_kv("Size", paste(format(fi$size, big.mark = ","), "bytes")))
    .cc_ln(.cc_kv("Modified", format(fi$mtime, "%Y-%m-%d %H:%M")))
    cat("\n")
  }

  content <- readLines(file_path, warn = FALSE)
  full_prompt <- paste0(
    "File: ", basename(file_path), "\n\n",
    "<file_content>\n", paste(content, collapse = "\n"), "\n</file_content>\n\n",
    prompt
  )

  claude_code(full_prompt, verbose = verbose, ...)
}

#' Code review with Claude Code
#'
#' @param code Code to review (character vector or file path).
#' @param focus Focus area: "bugs", "style", "performance", "security", or "all".
#' @param language Programming language (auto-detected if NULL).
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Review comments from Claude.
#'
#' @family claude_code
#' @export
claude_code_review <- function(code, focus = "all", language = NULL, verbose = FALSE, ...) {
  source_file <- NULL

  if (length(code) == 1 && file.exists(code)) {
    source_file <- code
    code <- readLines(code, warn = FALSE)
    if (is.null(language)) {
      ext <- tolower(tools::file_ext(source_file))
      language <- switch(ext,
        "r" = "R", "py" = "Python", "js" = "JavaScript",
        "ts" = "TypeScript", "sql" = "SQL", NULL)
    }
  }

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Code Review"))
    cat("\n")
    if (!is.null(source_file)) .cc_ln(.cc_kv("File", basename(source_file)))
    .cc_ln(.cc_kv("Language", language %||% "(auto-detect)"))
    .cc_ln(.cc_kv("Focus", focus))
    .cc_ln(.cc_kv("Lines", length(code)))
    cat("\n")
  }

  code_str <- paste(code, collapse = "\n")

  focus_instr <- switch(focus,
    "bugs" = "Focus on identifying potential bugs and errors.",
    "style" = "Focus on code style and readability improvements.",
    "performance" = "Focus on performance optimizations.",
    "security" = "Focus on security vulnerabilities.",
    "all" = "Review for bugs, style, performance, and security.",
    "Review the code comprehensively.")

  lang_ctx <- if (!is.null(language)) paste0("This is ", language, " code.\n") else ""

  prompt <- paste0(lang_ctx, "Please review the following code. ", focus_instr,
                   "\n\n```\n", code_str, "\n```")

  claude_code(prompt, verbose = verbose, ...)
}

#' Generate code with Claude Code
#'
#' @param description Description of what the code should do.
#' @param language Target programming language.
#' @param context Additional context or requirements.
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Generated code from Claude.
#'
#' @family claude_code
#' @export
claude_code_generate <- function(description, language = "R", context = NULL, verbose = FALSE, ...) {
  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Code Generation"))
    cat("\n")
    .cc_ln(.cc_kv("Language", language))
    desc_preview <- if (nchar(description) > 50) paste0(substr(description, 1, 50), "...") else description
    .cc_ln(.cc_kv("Description", desc_preview))
    cat("\n")
  }

  prompt <- paste0("Generate ", language, " code for the following:\n\n", description)
  if (!is.null(context)) prompt <- paste0(prompt, "\n\nAdditional context:\n", context)
  prompt <- paste0(prompt, "\n\nProvide only the code without explanations.")

  claude_code(prompt, verbose = verbose, ...)
}

#' Analyze data with Claude Code
#'
#' @param data Data frame to analyze.
#' @param question Analysis question or task.
#' @param include_summary Logical; include basic summary stats.
#' @param verbose Logical; print status messages.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return Analysis from Claude.
#'
#' @family claude_code
#' @export
claude_code_analyze <- function(data, question, include_summary = TRUE, verbose = FALSE, ...) {
  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Data Analysis"))
    cat("\n")
    .cc_ln(.cc_kv("Rows", format(nrow(data), big.mark = ",")))
    .cc_ln(.cc_kv("Columns", ncol(data)))
    col_preview <- paste(head(names(data), 5), collapse = ", ")
    if (ncol(data) > 5) col_preview <- paste0(col_preview, " ... +", ncol(data) - 5, " more")
    .cc_ln(.cc_kv("Column Names", col_preview))
    cat("\n")
  }

  data_str <- paste(capture.output(head(data, 20)), collapse = "\n")

  summary_str <- ""
  if (include_summary) {
    col_info <- sapply(data, function(x) {
      cl <- class(x)[1]
      if (is.numeric(x)) {
        rng <- range(x, na.rm = TRUE)
        sprintf("%s [%.1f-%.1f]", cl, rng[1], rng[2])
      } else cl
    })
    summary_str <- paste0(
      "\n\nDataset summary:\n",
      "- Rows: ", format(nrow(data), big.mark = ","), "\n",
      "- Columns: ", ncol(data), "\n",
      "- Column types:\n",
      paste(sprintf("  - %s: %s", names(col_info), col_info), collapse = "\n"), "\n"
    )
    if (nrow(data) > 20) summary_str <- paste0(summary_str, "- (showing first 20 rows)\n")
  }

  prompt <- paste0("Analyze this dataset:\n\n```\n", data_str, "\n```",
                   summary_str, "\n\nQuestion/Task: ", question)

  claude_code(prompt, verbose = verbose, ...)
}

# ============================================================================
# Batch Operations
# ============================================================================

#' Run multiple prompts in batch
#'
#' @param prompts Character vector of prompts.
#' @param parallel Logical; run in parallel if backend available.
#' @param progress Logical; show progress bar.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return List of responses.
#'
#' @family claude_code
#' @export
claude_code_batch <- function(prompts, parallel = FALSE, progress = TRUE, ...) {
  n <- length(prompts)

  if (progress) {
    cat("\n")
    .cc_ln(.cc_header("Batch Processing"))
    cat("\n")
    .cc_ln(.cc_kv("Total Prompts", n))
    .cc_ln(.cc_kv("Mode", if (parallel) "Parallel" else "Sequential"))
    cat("\n")
  }

  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    if (progress) .cc_ln(.cc_info("Running in parallel..."))
    results <- future.apply::future_lapply(prompts, function(p) {
      tryCatch(claude_code(p, ...), error = function(e) list(error = e$message, prompt = p))
    })
    if (progress) {
      cat("\n")
      .cc_ln(.cc_success(sprintf("Complete: %d/%d", n, n)))
    }
  } else {
    results <- vector("list", n)
    errors <- 0

    for (i in seq_along(prompts)) {
      if (progress) {
        pct <- round(100 * i / n)
        bar_width <- 30
        filled <- round(bar_width * i / n)
        bar <- paste0("[", strrep("\u2588", filled), strrep("\u2591", bar_width - filled), "]")
        cat(sprintf("\r  %s %3d%% (%d/%d)", bar, pct, i, n))
        flush.console()
      }

      results[[i]] <- tryCatch(claude_code(prompts[[i]], ...), error = function(e) {
        errors <<- errors + 1
        list(error = e$message, prompt = prompts[[i]])
      })
    }

    if (progress) {
      cat("\n\n")
      .cc_ln(.cc_success(sprintf("Complete: %d succeeded, %d failed", n - errors, errors)))
    }
  }

  cat("\n")
  results
}

# ============================================================================
# Utilities
# ============================================================================

#' Extract code blocks from response
#'
#' @param response Response text from Claude.
#' @param language Optional language filter.
#' @param verbose Logical; print extraction summary.
#'
#' @return Character vector of code blocks.
#'
#' @family claude_code
#' @export
claude_code_extract <- function(response, language = NULL, verbose = FALSE) {
  pattern <- "```([a-zA-Z]*)\\n([\\s\\S]*?)```"
  matches <- gregexpr(pattern, response, perl = TRUE)
  blocks <- regmatches(response, matches)[[1]]

  if (length(blocks) == 0) {
    if (verbose) .cc_ln(.cc_info("No code blocks found"))
    return(character(0))
  }

  extracted <- lapply(blocks, function(block) {
    lines <- strsplit(block, "\n")[[1]]
    lang <- gsub("```", "", lines[1])
    code <- paste(lines[-c(1, length(lines))], collapse = "\n")
    list(language = lang, code = code)
  })

  if (!is.null(language)) {
    extracted <- Filter(function(x) tolower(x$language) == tolower(language) || x$language == "", extracted)
  }

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Extracted Code Blocks"))
    cat("\n")
    langs <- sapply(extracted, `[[`, "language")
    langs[langs == ""] <- "(unspecified)"
    for (l in unique(langs)) {
      .cc_ln(.cc_item(sprintf("%s: %d block(s)", l, sum(langs == l))))
    }
    cat("\n")
  }

  sapply(extracted, `[[`, "code")
}

#' Parse JSON from Claude response
#'
#' @param response Response from Claude.
#' @param simplify Passed to jsonlite::fromJSON.
#' @param verbose Logical; print parsing status.
#'
#' @return Parsed R object or NULL.
#'
#' @family claude_code
#' @export
claude_code_parse <- function(response, simplify = TRUE, verbose = FALSE) {
  json_pattern <- "\\{[\\s\\S]*\\}|\\[[\\s\\S]*\\]"
  matches <- regmatches(response, gregexpr(json_pattern, response, perl = TRUE))[[1]]

  if (length(matches) == 0) {
    if (verbose) .cc_ln(.cc_warn("No JSON found in response"))
    warning("No JSON found in response", call. = FALSE)
    return(NULL)
  }

  for (m in matches) {
    result <- tryCatch(jsonlite::fromJSON(m, simplifyVector = simplify), error = function(e) NULL)
    if (!is.null(result)) {
      if (verbose) .cc_ln(.cc_success("JSON parsed successfully"))
      return(result)
    }
  }

  if (verbose) .cc_ln(.cc_error("Could not parse any JSON"))
  warning("Could not parse any JSON from response", call. = FALSE)
  NULL
}

#' Print Claude Code response
#'
#' @param response Response from \code{claude_code}.
#' @param width Maximum line width.
#'
#' @return Invisibly returns response.
#'
#' @family claude_code
#' @export
claude_code_print <- function(response, width = 80) {
  if (is.list(response)) {
    cat("\n")
    .cc_ln(.cc_header("Response (JSON)"))
    cat("\n")
    cat(jsonlite::toJSON(response, auto_unbox = TRUE, pretty = TRUE))
    cat("\n\n")
  } else {
    cat("\n")
    .cc_ln(.cc_header("Response"))
    cat("\n")

    lines <- strsplit(as.character(response), "\n")[[1]]
    in_code <- FALSE
    for (line in lines) {
      if (grepl("^```", line)) {
        in_code <- !in_code
        cat(line, "\n")
      } else if (in_code) {
        cat(line, "\n")
      } else {
        wrapped <- strwrap(line, width = width)
        if (length(wrapped) == 0) cat("\n") else cat(paste(wrapped, collapse = "\n"), "\n")
      }
    }
    cat("\n")
  }
  invisible(response)
}

#' Interactive Claude Code chat
#'
#' Start an interactive chat session with Claude Code in the R console.
#' Supports conversation history, code execution with user approval,
#' and multi-turn conversations.
#'
#' @param system_prompt Optional system prompt.
#' @param working_dir Working directory.
#' @param allow_code_execution Logical; if TRUE, allows executing R code with user approval.
#'
#' @return Invisibly returns conversation history as a list.
#'
#' @details
#' The chat session maintains full conversation history which is passed to Claude
#' on each turn, enabling context-aware multi-turn conversations.
#'
#' When \code{allow_code_execution = TRUE}, the session will detect R code blocks
#' in Claude's responses and prompt for execution approval. Users can type:
#' \itemize{
#'   \item 'y' or 'yes' - Execute all pending code
#'   \item 'n' or 'no' - Skip code execution
#'   \item A number (e.g., '1') - Execute only that code block
#' }
#'
#' Available commands during chat:
#' \itemize{
#'   \item 'exit' or 'quit' - End the session
#'   \item 'clear' - Clear conversation history
#'   \item 'history' - Show conversation history
#' }
#'
#' @family claude_code
#' @export
claude_code_chat <- function(system_prompt = NULL, working_dir = NULL, allow_code_execution = TRUE) {
  if (!interactive()) stop("claude_code_chat() requires an interactive R session", call. = FALSE)

  cat("\n")
  top <- paste0("\u250C", strrep("\u2500", 58), "\u2510")
  mid <- paste0("\u2502 Claude Code Chat Session", strrep(" ", 33), "\u2502")
  bot <- paste0("\u2514", strrep("\u2500", 58), "\u2518")
  .cc_ln(top); .cc_ln(mid); .cc_ln(bot)
  cat("\n")
  .cc_ln(.cc_item("Type your message and press Enter"))
  .cc_ln(.cc_item("Commands: 'exit', 'clear', 'history'"))
  if (allow_code_execution) {
    .cc_ln(.cc_item("Code execution: enabled (will prompt for approval)"))
  }
  cat("\n")
  .cc_ln(strrep("\u2500", 60))
  cat("\n")

  # Store history as list of list(role, content) for better structure
  history <- list()

  repeat {
    prompt <- readline("You \u203A ")

    if (tolower(prompt) %in% c("exit", "quit", "q")) {
      cat("\n"); .cc_ln(.cc_info("Session ended")); cat("\n")
      break
    }

    if (tolower(prompt) == "clear") {
      history <- list()
      .cc_ln(.cc_success("History cleared")); cat("\n")
      next
    }

    if (tolower(prompt) == "history") {
      if (length(history) == 0) {
        .cc_ln(.cc_info("No history yet"))
      } else {
        cat("\n"); .cc_ln(.cc_header("Conversation History")); cat("\n")
        for (i in seq_along(history)) {
          entry <- history[[i]]
          role_label <- if (entry$role == "user") "You" else "Claude"
          cat(sprintf("[%d] %s:\n", i, role_label))
          # Truncate long messages for display
          content_preview <- if (nchar(entry$content) > 500) {
            paste0(substr(entry$content, 1, 500), "\n... (truncated)")
          } else {
            entry$content
          }
          cat(content_preview, "\n\n")
        }
      }
      cat("\n")
      next
    }

    if (nchar(trimws(prompt)) == 0) next

    cat("\n"); .cc_ln(.cc_info("Thinking..."))

    # Build full prompt with conversation history context
    full_prompt <- .cc_build_context_prompt(history, prompt, working_dir)

    # Build system prompt - add code execution instructions if enabled
    effective_system_prompt <- system_prompt
    if (allow_code_execution) {
      code_exec_prompt <- .cc_code_execution_system_prompt()
      effective_system_prompt <- if (is.null(system_prompt)) {
        code_exec_prompt
      } else {
        paste0(system_prompt, "\n\n", code_exec_prompt)
      }
    }

    response <- tryCatch(
      claude_code(full_prompt, system_prompt = effective_system_prompt, working_dir = working_dir),
      error = function(e) paste(.cc_error("Error:"), e$message)
    )

    cat("\n"); .cc_ln("Claude \u203A"); cat("\n")
    cat(response); cat("\n\n")

    # Update history
    history <- append(history, list(list(role = "user", content = prompt)))
    history <- append(history, list(list(role = "assistant", content = response)))

    # Check for executable R code and handle execution
    if (allow_code_execution) {
      code_blocks <- .cc_extract_r_code(response)
      if (length(code_blocks) > 0) {
        .cc_handle_code_execution(code_blocks)
      }
    }

    .cc_ln(strrep("\u2500", 60)); cat("\n")
  }

  invisible(history)
}

# Default system prompt for code execution mode
.cc_code_execution_system_prompt <- function() {
  paste0(
    "You are an R coding assistant. When the user asks you to create, execute, run, ",
    "or generate code, perform analysis, or complete tasks that require R:\n\n",
    "CRITICAL RULES:\n",
    "1. ALWAYS output R code directly in fenced code blocks with ```r marker\n",
    "2. NEVER ask for permission, approval, or confirmation before showing code\n",
    "3. NEVER ask 'Would you like me to...', 'Should I...', 'Do you want me to...'\n",
    "4. NEVER present numbered options for the user to choose from\n",
    "5. The R session has its own approval mechanism - just provide the code\n\n",
    "When the user asks for something like 'create a dataframe' or 'run analysis':\n",
    "- Output the code IMMEDIATELY\n",
    "- Do NOT ask which approach they prefer\n",
    "- Do NOT offer choices between running directly vs creating a file\n",
    "- Just write the code and let the built-in approval system handle execution\n\n",
    "Example - if user says 'Create a random dataframe with 10 rows':\n",
    "CORRECT response:\n",
    "```r\n",
    "df <- data.frame(\n",
    "  id = 1:10,\n",
    "  value = rnorm(10),\n",
    "  category = sample(LETTERS[1:3], 10, replace = TRUE)\n",
    ")\n",
    "print(df)\n",
    "```\n\n",
    "WRONG response:\n",
    "'Would you like me to: 1. Run the command directly 2. Create a script file'\n\n",
    "Always provide complete, runnable R code immediately without asking."
  )
}

# Helper: Build prompt with conversation context
.cc_build_context_prompt <- function(history, current_prompt, working_dir) {
  if (length(history) == 0) {
    # First message - add working directory context
    context <- sprintf(
      "You are an R coding assistant working in directory: %s\n\n%s",
      working_dir %||% getwd(),
      current_prompt
    )
    return(context)
  }

  # Build conversation context
  context_parts <- c(
    sprintf("Working directory: %s", working_dir %||% getwd()),
    "",
    "Previous conversation:",
    "---"
  )

  for (entry in history) {
    role_label <- if (entry$role == "user") "User" else "Assistant"
    # Limit history entries to prevent overly long prompts
    content <- if (nchar(entry$content) > 2000) {
      paste0(substr(entry$content, 1, 2000), "\n[... truncated ...]")
    } else {
      entry$content
    }
    context_parts <- c(context_parts, sprintf("%s: %s", role_label, content), "")
  }

  context_parts <- c(
    context_parts,
    "---",
    "",
    "Current user message:",
    current_prompt
  )

  paste(context_parts, collapse = "\n")
}

# Helper: Extract R code blocks from response
.cc_extract_r_code <- function(response) {
  # Match ```r or ```R code blocks
  pattern <- "```[rR]\\s*\\n([\\s\\S]*?)```"
  matches <- gregexpr(pattern, response, perl = TRUE)
  blocks <- regmatches(response, matches)[[1]]

  if (length(blocks) == 0) return(character(0))

  # Extract just the code content
  code_blocks <- sapply(blocks, function(block) {
    lines <- strsplit(block, "\n")[[1]]
    # Remove first line (```r) and last line (```)
    code_lines <- lines[-c(1, length(lines))]
    paste(code_lines, collapse = "\n")
  }, USE.NAMES = FALSE)

  code_blocks
}

# Helper: Handle code execution with user approval
.cc_handle_code_execution <- function(code_blocks) {
  n <- length(code_blocks)

  cat("\n")
  .cc_ln(.cc_header("Executable R Code Detected"))
  cat("\n")

  # Display code blocks with numbers

for (i in seq_along(code_blocks)) {
    .cc_ln(sprintf("  [%d] Code block:", i))
    code_preview <- if (nchar(code_blocks[i]) > 200) {
      paste0(substr(code_blocks[i], 1, 200), "\n      ... (truncated)")
    } else {
      code_blocks[i]
    }
    # Indent code for display
    indented <- gsub("\n", "\n      ", code_preview)
    cat(sprintf("      %s\n\n", indented))
  }

  choice <- tolower(trimws(readline(sprintf("Execute code? [y/n/1-%d] \u203A ", n))))

  if (choice %in% c("y", "yes", "all")) {
    # Execute all code blocks
    for (i in seq_along(code_blocks)) {
      .cc_execute_code_block(code_blocks[i], i)
    }
  } else if (choice %in% c("n", "no", "")) {
    .cc_ln(.cc_info("Code execution skipped"))
  } else if (grepl("^[0-9]+$", choice)) {
    idx <- as.integer(choice)
    if (idx >= 1 && idx <= n) {
      .cc_execute_code_block(code_blocks[idx], idx)
    } else {
      .cc_ln(.cc_warn(sprintf("Invalid block number. Choose 1-%d", n)))
    }
  } else {
    .cc_ln(.cc_warn("Invalid choice. Code execution skipped."))
  }

  cat("\n")
}

# ============================================================================
# Pipe Operator with Code Execution
# ============================================================================

#' Pipe data to Claude and execute generated code
#'
#' A pipe-friendly function that sends data to Claude with a natural language
#' prompt, receives generated R code, executes it, and returns the result.
#' This enables workflows like \code{mtcars |> claude_pipe("create a scatterplot")}.
#'
#' Objects created during code execution are stored in a persistent pipe context,
#' allowing subsequent pipe operations to reference them. For example, you can
#' create a model in one pipe and summarize it in the next.
#'
#' @param .data Data to pipe to Claude (data.frame, vector, list, or other R object).
#' @param prompt Natural language description of what to do with the data.
#' @param execute Logical; if TRUE (default), execute the generated code.
#'   If FALSE, return the code as a string without executing.
#' @param envir Environment in which to execute the code. Defaults to the
#'   calling environment, which allows the result to access piped data.
#' @param model Model to use (overrides default from config).
#' @param verbose Logical; if TRUE, print status messages and show generated code.
#' @param ... Additional arguments passed to \code{claude_code}.
#'
#' @return If \code{execute = TRUE}, returns the result of executing the
#'   generated code. If \code{execute = FALSE}, returns the generated code
#'   as a character string. Returns NULL if code generation or execution fails.
#'
#' @details
#' The function works by:
#' \enumerate{
#'   \item Serializing the input data (showing structure and head for data frames)
#'   \item Sending the data representation and prompt to Claude
#'   \item Extracting R code from Claude's response
#'   \item Executing the code in a persistent pipe context environment
#'   \item Storing any created objects in the context for future pipes
#'   \item Returning the result
#' }
#'
#' The input data is available in the generated code as \code{.data}, allowing
#' Claude to reference it directly. For data frames, Claude also receives
#' column names, types, and a preview of the data.
#'
#' Objects created in previous pipe operations are also available and can be
#' referenced by name. Use \code{claude_pipe_context_objects()} to see what
#' objects are available, and \code{claude_pipe_context_clear()} to reset
#' the context.
#'
#' @examples
#' \dontrun{
#' # Create a visualization
#' mtcars |> claude_pipe("Create a pretty scatter plot of mpg vs cyl")
#'
#' # Data transformation
#' iris |> claude_pipe("Calculate mean petal length by species")
#'
#' # Chain operations with object references
#' mtcars |>
#'   claude_pipe("Create a linear model called 'fit' for mpg ~ wt + hp") |>
#'   claude_pipe("Show the summary of the 'fit' model")
#'
#' # Check available context objects
#' claude_pipe_context_objects()
#'
#' # Clear context to start fresh
#' claude_pipe_context_clear()
#'
#' # Get code without executing
#' code <- mtcars |> claude_pipe("Create a summary table", execute = FALSE)
#' cat(code)
#'
#' # Verbose mode to see what's happening
#' mtcars |> claude_pipe("Fit a linear model of mpg ~ wt", verbose = TRUE)
#' }
#'
#' @family claude_code
#' @export
claude_pipe <- function(.data,
                        prompt,
                        execute = TRUE,
                        envir = parent.frame(),
                        model = NULL,
                        verbose = FALSE,
                        ...) {

  if (!claude_code_available()) {
    .cc_ln(.cc_error("Claude Code CLI not found"))
    .cc_ln(.cc_info("Install with: npm install -g @anthropic-ai/claude-code"))
    stop("Claude Code CLI not available", call. = FALSE)
  }

  # Get the persistent pipe context
  pipe_ctx <- claude_pipe_context()

  # Store original data in context if this is a data frame and we don't already have original data
  # This preserves the initial piped data for subsequent operations in a chain
  if (is.data.frame(.data) && !exists(".original_data", envir = pipe_ctx)) {
    pipe_ctx$.original_data <- .data
    pipe_ctx$.original_data_name <- deparse(substitute(.data))
  }

  # Prepare data representation for Claude
  data_info <- .cc_prepare_data_for_pipe(.data, verbose)

  # Get context info for objects from previous pipes
  context_info <- .cc_format_context_info(pipe_ctx)

  # Build the prompt with data context and instructions
  system_prompt <- .cc_pipe_system_prompt(has_context = !is.null(context_info))

  full_prompt <- paste0(
    "The user has piped the following R data to you:\n\n",
    "<data_info>\n",
    data_info,
    "\n</data_info>\n\n",
    "The data is available in the variable `.data` in the R environment.\n\n",
    if (!is.null(context_info)) paste0("<context>\n", context_info, "\n</context>\n\n") else "",
    "User request: ", prompt, "\n\n",
    "Generate R code to accomplish this task. The code should use `.data` to reference the input data.",
    if (!is.null(context_info)) " You may also reference any objects from previous pipe operations." else ""
  )

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Claude Pipe"))
    cat("\n")
    .cc_ln(.cc_kv("Data type", class(.data)[1]))
    if (is.data.frame(.data)) {
      .cc_ln(.cc_kv("Dimensions", sprintf("%d x %d", nrow(.data), ncol(.data))))
    }
    .cc_ln(.cc_kv("Prompt", if (nchar(prompt) > 50) paste0(substr(prompt, 1, 50), "...") else prompt))
    .cc_ln(.cc_kv("Execute", execute))
    ctx_objs <- ls(pipe_ctx, all.names = FALSE)
    if (length(ctx_objs) > 0) {
      .cc_ln(.cc_kv("Context objects", paste(ctx_objs, collapse = ", ")))
    }
    cat("\n")
    .cc_ln(.cc_info("Sending to Claude..."))
  }

  # Call Claude
  response <- tryCatch(
    claude_code(full_prompt, system_prompt = system_prompt, model = model, ...),
    error = function(e) {
      .cc_ln(.cc_error(paste("Claude API error:", e$message)))
      return(NULL)
    }
  )

  if (is.null(response)) return(invisible(NULL))

  # Extract R code from response
  code_blocks <- .cc_extract_r_code(response)

  if (length(code_blocks) == 0) {
    if (verbose) .cc_ln(.cc_warn("No R code blocks found in response"))
    # Try to find any code-like content
    warning("Claude did not return R code in expected format", call. = FALSE)
    if (verbose) {
      cat("\nClaude's response:\n")
      cat(response)
      cat("\n")
    }
    return(invisible(NULL))
  }

  # Combine all code blocks
  code <- paste(code_blocks, collapse = "\n\n")

  if (verbose) {
    cat("\n")
    .cc_ln(.cc_header("Generated Code"))
    cat("\n")
    cat(code)
    cat("\n\n")
  }

  # Return code if not executing

  if (!execute) {
    return(code)
  }

  # Execute the code
  if (verbose) .cc_ln(.cc_info("Executing code..."))

  # Track objects before execution to identify new ones
  objects_before <- ls(pipe_ctx, all.names = FALSE)

  result <- tryCatch({
    # Use the persistent pipe context as execution environment
    # Set .data in the context for this execution
    pipe_ctx$.data <- .data

    # Parse and evaluate in the pipe context
    parsed <- parse(text = code)
    eval_result <- NULL
    for (expr in parsed) {
      eval_result <- eval(expr, envir = pipe_ctx)
    }

    if (verbose) {
      .cc_ln(.cc_success("Execution complete"))

      # Show new objects created
      objects_after <- ls(pipe_ctx, all.names = FALSE)
      new_objects <- setdiff(objects_after, c(objects_before, ".data", ".original_data", ".original_data_name"))
      if (length(new_objects) > 0) {
        .cc_ln(.cc_info(paste("Objects created in R environment:", paste(new_objects, collapse = ", "))))
      }
    }

    # Copy new objects to the global environment so they're accessible to the user
    objects_after <- ls(pipe_ctx, all.names = FALSE)
    new_objects <- setdiff(objects_after, c(objects_before, ".data", ".original_data", ".original_data_name"))
    for (obj_name in new_objects) {
      assign(obj_name, get(obj_name, envir = pipe_ctx), envir = .GlobalEnv)
    }

    eval_result
  }, error = function(e) {
    .cc_ln(.cc_error(paste("Execution failed:", e$message)))
    if (verbose) {
      cat("\nFailed code:\n")
      cat(code)
      cat("\n")
    }
    warning("Code execution failed: ", e$message, call. = FALSE)
    invisible(NULL)
  })

  result
}

#' @rdname claude_pipe
#' @export
`%|c>%` <- function(.data, prompt) {

  claude_pipe(.data, prompt)
}

# Helper: Prepare data representation for Claude
.cc_prepare_data_for_pipe <- function(.data, verbose = FALSE) {
  if (is.data.frame(.data)) {
    # For data frames, provide comprehensive info
    parts <- c(
      sprintf("Type: data.frame (%d rows x %d columns)", nrow(.data), ncol(.data)),
      "",
      "Column information:"
    )

    col_info <- sapply(names(.data), function(nm) {
      col <- .data[[nm]]
      cls <- class(col)[1]
      if (is.numeric(col)) {
        rng <- range(col, na.rm = TRUE)
        sprintf("  - %s: %s [range: %.3g to %.3g]", nm, cls, rng[1], rng[2])
      } else if (is.factor(col)) {
        lvls <- levels(col)
        lvl_str <- if (length(lvls) > 5) {
          paste0(paste(head(lvls, 5), collapse = ", "), ", ... (", length(lvls), " levels)")
        } else {
          paste(lvls, collapse = ", ")
        }
        sprintf("  - %s: factor [%s]", nm, lvl_str)
      } else if (is.character(col)) {
        sprintf("  - %s: character", nm)
      } else {
        sprintf("  - %s: %s", nm, cls)
      }
    })

    parts <- c(parts, col_info, "")

    # Show first few rows
    n_preview <- min(10, nrow(.data))
    preview <- paste(capture.output(print(head(.data, n_preview))), collapse = "\n")
    parts <- c(parts, sprintf("First %d rows:", n_preview), preview)

    if (nrow(.data) > n_preview) {
      parts <- c(parts, sprintf("... (%d more rows)", nrow(.data) - n_preview))
    }

    paste(parts, collapse = "\n")

  } else if (is.list(.data)) {
    # For lists, show structure
    struct <- paste(capture.output(str(.data, max.level = 2)), collapse = "\n")
    paste0("Type: list\n\nStructure:\n", struct)

  } else if (is.vector(.data)) {
    # For vectors
    len <- length(.data)
    cls <- class(.data)[1]
    preview <- if (len > 20) {
      paste0(paste(head(.data, 20), collapse = ", "), ", ... (", len, " elements)")
    } else {
      paste(.data, collapse = ", ")
    }
    sprintf("Type: %s vector (length %d)\n\nValues: %s", cls, len, preview)

  } else if (is.matrix(.data)) {
    # For matrices
    dims <- dim(.data)
    preview <- paste(capture.output(print(head(.data, 10))), collapse = "\n")
    sprintf("Type: matrix (%d x %d)\n\nPreview:\n%s", dims[1], dims[2], preview)

  } else {
    # For other objects
    struct <- paste(capture.output(str(.data)), collapse = "\n")
    sprintf("Type: %s\n\nStructure:\n%s", class(.data)[1], struct)
  }
}

# System prompt for pipe operations
.cc_pipe_system_prompt <- function(has_context = FALSE) {
  base_prompt <- paste0(
    "You are an R coding assistant helping with piped data operations. ",
    "The user has piped R data to you and wants you to perform an operation on it.\n\n",
    "CRITICAL RULES:\n",
    "1. ALWAYS output R code in a fenced code block with ```r marker\n",
    "2. The input data is available as `.data` - always use this variable for the piped data\n",
    "3. Generate complete, runnable R code\n",
    "4. For visualizations, use base R or ggplot2 (assume ggplot2 is available)\n",
    "5. Keep code concise and focused on the task\n",
    "6. Do NOT include library() calls for base packages\n",
    "7. If using ggplot2, include library(ggplot2) at the start\n",
    "8. The last expression should be the result to return (e.g., the plot or computed value)\n"
  )

  context_rules <- if (has_context) {
    paste0(
      "9. Objects from previous pipe operations are available in the environment - ",
      "you can reference them directly by name\n",
      "10. When the user mentions an object from a previous step, use it directly\n",
      "11. If creating new named objects (models, data frames, etc.), use descriptive names ",
      "so they can be referenced in subsequent pipes\n",
      "12. IMPORTANT: The original data frame that started the pipe chain is preserved as `.original_data`. ",
      "When the user asks about columns from the original data (like mpg, cyl, etc.) but `.data` is ",
      "not a data frame (e.g., it's a plot or model), use `.original_data` instead\n\n"
    )
  } else {
    "\n"
  }

  examples <- paste0(
    "Example - if user says 'Create a scatter plot of mpg vs cyl':\n",
    "```r\n",
    "library(ggplot2)\n",
    "ggplot(.data, aes(x = cyl, y = mpg)) +\n",
    "  geom_point() +\n",
    "  labs(title = \"MPG vs Cylinders\", x = \"Cylinders\", y = \"MPG\") +\n",
    "  theme_minimal()\n",
    "```\n\n",
    "Example - if user says 'Calculate mean by group':\n",
    "```r\n",
    "aggregate(. ~ group_column, data = .data, FUN = mean)\n",
    "```\n"
  )

  context_examples <- if (has_context) {
    paste0(
      "\nExample - if user previously created 'fit' model and now says 'Show summary of fit':\n",
      "```r\n",
      "summary(fit)\n",
      "```\n\n",
      "Example - if user says 'Create a linear model called model1 for mpg ~ wt':\n",
      "```r\n",
      "model1 <- lm(mpg ~ wt, data = .original_data)\n",
      "model1\n",
      "```\n\n",
      "Example - if the previous pipe created a plot and now user says 'Create a crosstab of mpg and cyl':\n",
      "```r\n",
      "# Use .original_data since .data is a plot, not the data frame\n",
      "table(.original_data$mpg, .original_data$cyl)\n",
      "```\n"
    )
  } else {
    ""
  }

  paste0(
    base_prompt,
    context_rules,
    examples,
    context_examples,
    "\nAlways reference the piped data as `.data` and provide clean, working code."
  )
}

# Helper: Execute a single code block with error handling
.cc_execute_code_block <- function(code, block_num) {
  cat("\n")
  .cc_ln(.cc_info(sprintf("Executing block %d...", block_num)))
  cat("\n")

  result <- tryCatch({
    # Capture output
    output <- capture.output({
      eval_result <- eval(parse(text = code), envir = globalenv())
    }, type = "output")

    list(
      success = TRUE,
      output = paste(output, collapse = "\n"),
      result = eval_result
    )
  }, error = function(e) {
    list(
      success = FALSE,
      error = e$message
    )
  }, warning = function(w) {
    list(
      success = TRUE,
      warning = w$message
    )
  })

  if (result$success) {
    .cc_ln(.cc_success(sprintf("Block %d executed successfully", block_num)))
    if (nzchar(result$output)) {
      cat("\n  Output:\n")
      # Indent output
      indented_output <- gsub("\n", "\n  ", result$output)
      cat(sprintf("  %s\n", indented_output))
    }
    if (!is.null(result$result) && !identical(result$result, invisible())) {
      # Show result if it's something meaningful
      if (is.data.frame(result$result)) {
        cat("\n  Result: data.frame with", nrow(result$result), "rows,", ncol(result$result), "columns\n")
      } else if (length(result$result) <= 10 && !is.function(result$result)) {
        cat("\n  Result:", utils::capture.output(print(result$result))[1], "\n")
      }
    }
    if (!is.null(result$warning)) {
      .cc_ln(.cc_warn(paste("Warning:", result$warning)))
    }
  } else {
    .cc_ln(.cc_error(sprintf("Block %d failed: %s", block_num, result$error)))
  }
}
