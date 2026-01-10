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
#'
#' @param system_prompt Optional system prompt.
#' @param working_dir Working directory.
#'
#' @return Invisibly returns conversation history.
#'
#' @family claude_code
#' @export
claude_code_chat <- function(system_prompt = NULL, working_dir = NULL) {
  if (!interactive()) stop("claude_code_chat() requires an interactive R session", call. = FALSE)

  cat("\n")
  top <- paste0("\u250C", strrep("\u2500", 58), "\u2510")
  mid <- paste0("\u2502 Claude Code Chat Session", strrep(" ", 33), "\u2502")
  bot <- paste0("\u2514", strrep("\u2500", 58), "\u2518")
  .cc_ln(top); .cc_ln(mid); .cc_ln(bot)
  cat("\n")
  .cc_ln(.cc_item("Type your message and press Enter"))
  .cc_ln(.cc_item("Commands: 'exit', 'clear', 'history'"))
  cat("\n")
  .cc_ln(strrep("\u2500", 60))
  cat("\n")

  history <- character(0)

  repeat {
    prompt <- readline("You \u203A ")

    if (tolower(prompt) %in% c("exit", "quit", "q")) {
      cat("\n"); .cc_ln(.cc_info("Session ended")); cat("\n")
      break
    }

    if (tolower(prompt) == "clear") {
      history <- character(0)
      .cc_ln(.cc_success("History cleared")); cat("\n")
      next
    }

    if (tolower(prompt) == "history") {
      if (length(history) == 0) {
        .cc_ln(.cc_info("No history yet"))
      } else {
        cat("\n"); .cc_ln(.cc_header("Conversation History")); cat("\n")
        cat(paste(history, collapse = "\n\n")); cat("\n")
      }
      cat("\n")
      next
    }

    if (nchar(trimws(prompt)) == 0) next

    cat("\n"); .cc_ln(.cc_info("Thinking..."))

    response <- tryCatch(
      claude_code(prompt, system_prompt = system_prompt, working_dir = working_dir),
      error = function(e) paste(.cc_error("Error:"), e$message)
    )

    cat("\n"); .cc_ln("Claude \u203A"); cat("\n")
    cat(response); cat("\n\n")
    .cc_ln(strrep("\u2500", 60)); cat("\n")

    history <- c(history, paste0("You: ", prompt), paste0("Claude: ", response))
  }

  invisible(history)
}
