#' @importFrom curl curl_fetch_stream
#' @importFrom curl handle_setopt
#' @importFrom curl handle_setheaders
#' @importFrom curl new_handle
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom httr http_status
#' @importFrom httr POST
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom utils capture.output
#' @importFrom utils flush.console
#' @importFrom utils head
#' @importFrom utils str
NULL

.claude_api_url <- function(base_url, path) {
  if (!is.character(base_url) || length(base_url) != 1L || !nzchar(base_url)) {
    stop("`base_url` must be a non-empty string.", call. = FALSE)
  }

  paste0(sub("/+$", "", base_url), path)
}

.claude_api_key <- function(api_key) {
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
  }

  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop(
      "Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.",
      call. = FALSE
    )
  }

  api_key
}

.claude_headers <- function(api_key, betas = NULL) {
  headers <- c(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "content-type" = "application/json"
  )

  if (!is.null(betas)) {
    if (!is.character(betas) || anyNA(betas) || any(!nzchar(betas))) {
      stop("`betas` must be a character vector of beta header names.", call. = FALSE)
    }
    headers <- c(headers, "anthropic-beta" = paste(betas, collapse = ","))
  }

  headers
}

.claude_model_aliases <- function() {
  c(
    fable = "claude-fable-5",
    opus = "claude-opus-4-8",
    sonnet = "claude-sonnet-5",
    haiku = "claude-haiku-4-5-20251001"
  )
}

.resolve_claude_model <- function(model) {
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    stop("`model` must be a non-empty string.", call. = FALSE)
  }

  aliases <- .claude_model_aliases()
  shortcut <- tolower(model)
  if (shortcut %in% names(aliases)) {
    return(unname(aliases[[shortcut]]))
  }

  model
}

.normalize_claude_messages <- function(prompt) {
  if (is.character(prompt)) {
    if (length(prompt) != 1L || is.na(prompt)) {
      stop("A character `prompt` must be a single, non-missing string.", call. = FALSE)
    }
    return(list(list(role = "user", content = prompt)))
  }

  if (!is.list(prompt) || length(prompt) == 0L) {
    stop(
      "`prompt` must be a string or a non-empty list of message objects.",
      call. = FALSE
    )
  }

  valid <- vapply(
    prompt,
    function(message) {
      is.list(message) &&
        !is.null(message$role) &&
        is.character(message$role) &&
        length(message$role) == 1L &&
        !is.na(message$role) &&
        nzchar(message$role) &&
        !is.null(message$content)
    },
    logical(1)
  )

  if (!all(valid)) {
    stop(
      "Each message in `prompt` must contain a non-empty `role` and a `content` value.",
      call. = FALSE
    )
  }

  # Preserve complete content blocks, cache controls, tool results, and thinking
  # blocks. Earlier versions rebuilt each message and silently discarded them.
  prompt
}

.validate_number <- function(value, name, lower = -Inf, upper = Inf, integer = FALSE) {
  if (is.null(value)) {
    return(invisible(NULL))
  }

  valid <- is.numeric(value) && length(value) == 1L && is.finite(value) &&
    value >= lower && value <= upper
  if (integer) {
    valid <- valid && value == floor(value)
  }

  if (!valid) {
    range <- paste0("between ", lower, " and ", upper)
    kind <- if (integer) "an integer" else "a number"
    stop(sprintf("`%s` must be %s %s.", name, kind, range), call. = FALSE)
  }

  invisible(NULL)
}

.modern_adaptive_model <- function(model) {
  model %in% c(
    "claude-fable-5",
    "claude-mythos-5",
    "claude-opus-4-7",
    "claude-opus-4-8",
    "claude-sonnet-5"
  )
}

.always_thinking_model <- function(model) {
  model %in% c("claude-fable-5", "claude-mythos-5")
}

.validate_current_model_options <- function(model, messages, thinking,
                                            temperature, top_k, top_p) {
  if (.modern_adaptive_model(model)) {
    sampling <- c(
      temperature = !is.null(temperature) && temperature != 1,
      top_k = !is.null(top_k),
      top_p = !is.null(top_p) && top_p < 0.99
    )
    if (any(sampling)) {
      stop(
        paste0(
          model,
          " does not accept non-default sampling parameters. Remove: ",
          paste(names(sampling)[sampling], collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    if (!is.null(thinking) &&
        (identical(thinking$type, "enabled") || !is.null(thinking$budget_tokens))) {
      stop(
        paste0(
          model,
          " does not support manual thinking budgets. Use ",
          "`thinking = list(type = \"adaptive\")` and `effort`, or omit ",
          "`thinking` when adaptive thinking is already enabled by the model."
        ),
        call. = FALSE
      )
    }

    if (length(messages) > 0L &&
        identical(messages[[length(messages)]]$role, "assistant")) {
      stop(
        paste0(
          model,
          " does not support assistant-message prefills. End `prompt` with a user message."
        ),
        call. = FALSE
      )
    }
  }

  if (.always_thinking_model(model) &&
      !is.null(thinking) && identical(thinking$type, "disabled")) {
    stop(
      paste0(model, " has always-on adaptive thinking and cannot disable it."),
      call. = FALSE
    )
  }

  invisible(NULL)
}

.build_claude_request <- function(prompt,
                                  model = "sonnet",
                                  max_tokens = 1024,
                                  stop_sequences = NULL,
                                  temperature = NULL,
                                  top_k = NULL,
                                  top_p = NULL,
                                  system_prompt = NULL,
                                  thinking = NULL,
                                  effort = NULL,
                                  output_config = NULL,
                                  stream = FALSE,
                                  extra = list()) {
  model <- .resolve_claude_model(model)
  if (grepl("^claude-2(?:[.-]|$)", model)) {
    stop(
      paste0(
        "Claude 2 and the Text Completions API are retired. ",
        "Use a current Messages API model such as `sonnet`, `opus`, or `fable`."
      ),
      call. = FALSE
    )
  }

  messages <- .normalize_claude_messages(prompt)
  .validate_number(max_tokens, "max_tokens", lower = 0, integer = TRUE)
  .validate_number(temperature, "temperature", lower = 0, upper = 1)
  .validate_number(top_k, "top_k", lower = 0, integer = TRUE)
  .validate_number(top_p, "top_p", lower = 0, upper = 1)

  if (!is.null(stop_sequences) &&
      (!is.character(stop_sequences) || anyNA(stop_sequences))) {
    stop("`stop_sequences` must be a character vector.", call. = FALSE)
  }
  if (!is.null(thinking) && !is.list(thinking)) {
    stop("`thinking` must be a list.", call. = FALSE)
  }
  if (!is.null(output_config) && !is.list(output_config)) {
    stop("`output_config` must be a list.", call. = FALSE)
  }
  if (!is.null(effort) &&
      (!is.character(effort) || length(effort) != 1L || is.na(effort) || !nzchar(effort))) {
    stop("`effort` must be a non-empty string.", call. = FALSE)
  }
  if (!is.logical(stream) || length(stream) != 1L || is.na(stream)) {
    stop("`stream` must be TRUE or FALSE.", call. = FALSE)
  }

  .validate_current_model_options(
    model, messages, thinking, temperature, top_k, top_p
  )

  if (!is.list(extra)) {
    stop("Internal error: additional request parameters must be a list.", call. = FALSE)
  }
  if (length(extra) > 0L &&
      (is.null(names(extra)) || anyNA(names(extra)) || any(!nzchar(names(extra))))) {
    stop("Additional request parameters in `...` must all be named.", call. = FALSE)
  }

  request <- list(
    model = model,
    max_tokens = max_tokens,
    messages = messages
  )

  optional <- list(
    stop_sequences = stop_sequences,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    system = system_prompt,
    thinking = thinking
  )
  optional <- optional[!vapply(optional, is.null, logical(1))]
  request <- c(request, optional)

  if (!is.null(effort)) {
    if (is.null(output_config)) {
      output_config <- list()
    }
    if (!is.null(output_config$effort) && !identical(output_config$effort, effort)) {
      stop(
        "`effort` conflicts with `output_config$effort`; provide only one value.",
        call. = FALSE
      )
    }
    output_config$effort <- effort
  }
  if (!is.null(output_config)) {
    request$output_config <- output_config
  }
  if (stream) {
    request$stream <- TRUE
  }

  duplicates <- intersect(names(extra), names(request))
  if (length(duplicates) > 0L) {
    stop(
      paste0(
        "Additional request parameters cannot override: ",
        paste(duplicates, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  c(request, extra)
}

.parse_json <- function(text) {
  tryCatch(
    fromJSON(text, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

.protect_claude_json_arrays <- function(x, field = NULL) {
  # R does not distinguish a scalar from a one-element atomic vector. Preserve
  # API and JSON Schema fields that are always arrays before auto-unboxing the
  # scalar fields expected by the Messages API. Callers can use I() for other
  # one-element arrays in future or custom request fields.
  array_fields <- c("stop_sequences", "required", "enum", "examples")
  if (!is.list(x)) {
    if (!is.null(field) && field %in% array_fields && !inherits(x, "AsIs")) {
      return(I(x))
    }
    return(x)
  }

  fields <- names(x)
  for (index in seq_along(x)) {
    child_field <- if (is.null(fields) || !nzchar(fields[[index]])) {
      NULL
    } else {
      fields[[index]]
    }
    x[[index]] <- .protect_claude_json_arrays(x[[index]], child_field)
  }
  x
}

.encode_claude_request <- function(request) {
  toJSON(
    .protect_claude_json_arrays(request),
    auto_unbox = TRUE,
    null = "null"
  )
}

.stop_claude_api_error <- function(status_code, body) {
  parsed <- .parse_json(body)
  detail <- NULL
  if (!is.null(parsed$error$message)) {
    detail <- parsed$error$message
  } else if (!is.null(parsed$message)) {
    detail <- parsed$message
  }
  if (is.null(detail) || !nzchar(detail)) {
    detail <- tryCatch(
      http_status(status_code)$message,
      error = function(e) "Unknown HTTP error"
    )
  }

  stop(
    sprintf("Anthropic API request failed (%s): %s", status_code, detail),
    call. = FALSE
  )
}

.extract_claude_content <- function(content_blocks) {
  text <- ""
  thinking <- ""

  if (is.character(content_blocks)) {
    return(list(response = paste0(content_blocks, collapse = ""), thinking = thinking))
  }
  if (!is.list(content_blocks)) {
    return(list(response = text, thinking = thinking))
  }

  for (block in content_blocks) {
    if (!is.list(block) || is.null(block$type)) {
      next
    }
    if (identical(block$type, "text") && !is.null(block$text)) {
      text <- paste0(text, block$text)
    } else if (identical(block$type, "thinking") && !is.null(block$thinking)) {
      thinking <- paste0(thinking, block$thinking)
    }
  }

  list(response = text, thinking = thinking)
}

.as_claude_response <- function(message, events = NULL) {
  extracted <- .extract_claude_content(message$content)
  response <- list(
    id = message$id,
    model = message$model,
    content = message$content,
    response = extracted$response,
    thinking = extracted$thinking,
    stop_reason = message$stop_reason,
    stop_sequence = message$stop_sequence,
    stop_details = message$stop_details,
    usage = message$usage
  )
  if (!is.null(events)) {
    response$events <- events
  }

  class(response) <- c("claude_response", "list")
  response
}

.return_claude_response <- function(response, return_thinking, return_response) {
  if (identical(response$stop_reason, "refusal") && !return_response) {
    detail <- response$stop_details$explanation
    if (is.null(detail) || !nzchar(detail)) {
      detail <- "Claude declined the request."
    }
    warning(detail, call. = FALSE)
    return(NULL)
  }

  if (return_response) {
    return(response)
  }
  if (return_thinking) {
    return(list(thinking = response$thinking, response = response$response))
  }

  response$response
}

.parse_sse_event <- function(event) {
  event <- gsub("\r\n", "\n", event, fixed = TRUE)
  lines <- strsplit(event, "\n", fixed = TRUE)[[1L]]
  type_lines <- grep("^event:", lines, value = TRUE)
  data_lines <- grep("^data:", lines, value = TRUE)

  if (length(data_lines) == 0L) {
    return(NULL)
  }

  event_type <- if (length(type_lines) > 0L) {
    trimws(sub("^event:[[:space:]]?", "", type_lines[[length(type_lines)]]))
  } else {
    NULL
  }
  data <- paste(sub("^data:[[:space:]]?", "", data_lines), collapse = "\n")
  parsed <- .parse_json(data)
  if (is.null(parsed)) {
    return(NULL)
  }

  list(type = event_type, data = parsed)
}

.split_sse_buffer <- function(buffer) {
  split <- .split_sse_raw_buffer(charToRaw(buffer))
  list(
    events = vapply(split$events, rawToChar, character(1)),
    remainder = rawToChar(split$remainder)
  )
}

.find_sse_separator <- function(buffer) {
  bytes <- as.integer(buffer)
  size <- length(bytes)
  if (size < 2L) {
    return(NULL)
  }

  for (index in seq_len(size - 1L)) {
    if (bytes[[index]] == 10L && bytes[[index + 1L]] == 10L) {
      return(list(position = index, length = 2L))
    }
    if (index <= size - 3L &&
        identical(bytes[index:(index + 3L)], c(13L, 10L, 13L, 10L))) {
      return(list(position = index, length = 4L))
    }
  }

  NULL
}

.split_sse_raw_buffer <- function(buffer) {
  events <- list()

  repeat {
    separator <- .find_sse_separator(buffer)
    if (is.null(separator)) {
      break
    }

    event_end <- separator$position - 1L
    event <- if (event_end > 0L) buffer[seq_len(event_end)] else raw()
    if (length(event) > 0L) {
      events[[length(events) + 1L]] <- event
    }

    consumed <- separator$position + separator$length - 1L
    buffer <- if (consumed < length(buffer)) {
      buffer[(consumed + 1L):length(buffer)]
    } else {
      raw()
    }
  }

  list(events = events, remainder = buffer)
}

.merge_named_list <- function(x, y) {
  if (is.null(x)) {
    x <- list()
  }
  if (is.null(y)) {
    return(x)
  }
  for (name in names(y)) {
    x[[name]] <- y[[name]]
  }
  x
}

.stream_claude_message <- function(url, headers, body, requested_model,
                                   show_thinking = FALSE,
                                   keep_events = FALSE) {
  event_buffer <- raw()
  error_body_chunks <- list()
  error_body_size <- 0L
  max_error_body <- 4L * 1024L * 1024L
  events <- if (keep_events) list() else NULL
  blocks <- list()
  partial_json <- list()
  stream_error <- NULL
  saw_message_start <- FALSE
  saw_message_stop <- FALSE
  has_shown_thinking_header <- FALSE
  has_shown_response_header <- FALSE
  response_message <- list(
    id = NULL,
    type = "message",
    role = "assistant",
    model = requested_model,
    content = list(),
    stop_reason = NULL,
    stop_sequence = NULL,
    stop_details = NULL,
    usage = list()
  )

  process_event <- function(parsed_event) {
    if (is.null(parsed_event)) {
      return(invisible(NULL))
    }
    event <- parsed_event$data
    event_type <- event$type
    if (is.null(event_type)) {
      event_type <- parsed_event$type
    }
    if (keep_events) {
      events[[length(events) + 1L]] <<- event
    }

    if (identical(event_type, "message_start") && !is.null(event$message)) {
      saw_message_start <<- TRUE
      started <- event$message
      started$content <- list()
      response_message <<- .merge_named_list(response_message, started)
    } else if (identical(event_type, "content_block_start")) {
      index <- as.integer(event$index) + 1L
      blocks[[index]] <<- event$content_block
      if (identical(event$content_block$type, "fallback") &&
          !is.null(event$content_block$to$model)) {
        response_message$model <<- event$content_block$to$model
      }
    } else if (identical(event_type, "content_block_delta")) {
      index <- as.integer(event$index) + 1L
      block <- if (length(blocks) >= index) blocks[[index]] else NULL
      if (is.null(block)) {
        block <- list(type = NULL)
      }
      delta <- event$delta

      if (identical(delta$type, "text_delta")) {
        block$type <- if (is.null(block$type)) "text" else block$type
        block$text <- paste0(if (is.null(block$text)) "" else block$text, delta$text)
        if (!has_shown_response_header && has_shown_thinking_header) {
          message("\n\n----- RESPONSE -----\n")
          has_shown_response_header <<- TRUE
        }
        cat(delta$text)
      } else if (identical(delta$type, "thinking_delta")) {
        block$type <- if (is.null(block$type)) "thinking" else block$type
        block$thinking <- paste0(
          if (is.null(block$thinking)) "" else block$thinking,
          delta$thinking
        )
        if (show_thinking && !has_shown_thinking_header) {
          message("\n----- THINKING -----")
          has_shown_thinking_header <<- TRUE
        }
        if (show_thinking && nzchar(delta$thinking)) {
          message(delta$thinking, appendLF = FALSE)
        }
      } else if (identical(delta$type, "signature_delta")) {
        block$signature <- paste0(
          if (is.null(block$signature)) "" else block$signature,
          delta$signature
        )
      } else if (identical(delta$type, "input_json_delta")) {
        prior_json <- if (length(partial_json) >= index) {
          partial_json[[index]]
        } else {
          NULL
        }
        partial_json[[index]] <<- paste0(
          if (is.null(prior_json)) "" else prior_json,
          delta$partial_json
        )
      } else {
        if (is.null(block$deltas)) {
          block$deltas <- list()
        }
        block$deltas[[length(block$deltas) + 1L]] <- delta
      }
      blocks[[index]] <<- block
    } else if (identical(event_type, "content_block_stop")) {
      index <- as.integer(event$index) + 1L
      pending_json <- if (length(partial_json) >= index) {
        partial_json[[index]]
      } else {
        NULL
      }
      if (!is.null(pending_json)) {
        parsed_input <- .parse_json(pending_json)
        block <- if (length(blocks) >= index) blocks[[index]] else list()
        block$input <- if (is.null(parsed_input)) {
          pending_json
        } else {
          parsed_input
        }
        blocks[[index]] <<- block
      }
    } else if (identical(event_type, "message_delta")) {
      response_message <<- .merge_named_list(response_message, event$delta)
      response_message$usage <<- .merge_named_list(
        response_message$usage,
        event$usage
      )
    } else if (identical(event_type, "error")) {
      stream_error <<- event$error$message
      if (is.null(stream_error)) {
        stream_error <<- "Unknown streaming API error."
      }
    } else if (identical(event_type, "message_stop")) {
      saw_message_stop <<- TRUE
    }

    invisible(NULL)
  }

  process_buffer <- function(final = FALSE) {
    split <- .split_sse_raw_buffer(event_buffer)
    event_buffer <<- split$remainder
    for (event in split$events) {
      process_event(.parse_sse_event(rawToChar(event)))
    }
    if (final && length(event_buffer) > 0L) {
      # A truncated stream may end in the middle of a UTF-8 code point. Try a
      # final unterminated event, but let terminal-event validation report a
      # clean truncation error if the remainder cannot be decoded.
      tryCatch(
        process_event(.parse_sse_event(rawToChar(event_buffer))),
        error = function(e) invisible(NULL)
      )
      event_buffer <<- raw()
    }
    invisible(NULL)
  }

  callback <- function(data, ...) {
    if (error_body_size < max_error_body) {
      retained <- min(length(data), max_error_body - error_body_size)
      if (retained > 0L) {
        error_body_chunks[[length(error_body_chunks) + 1L]] <<- data[seq_len(retained)]
        error_body_size <<- error_body_size + retained
      }
    }
    event_buffer <<- c(event_buffer, data)
    process_buffer()
    length(data)
  }

  handle <- new_handle()
  handle_setheaders(handle, .list = headers)
  handle_setopt(handle, post = TRUE, postfields = body)
  result <- curl_fetch_stream(url, callback, handle = handle)

  status_code <- result$status_code
  if (is.null(status_code)) {
    status_code <- 0L
  }
  if (status_code < 200L || status_code >= 300L) {
    raw_body <- if (length(error_body_chunks) > 0L) {
      rawToChar(do.call(c, error_body_chunks))
    } else {
      ""
    }
    .stop_claude_api_error(status_code, raw_body)
  }
  process_buffer(final = TRUE)
  if (!is.null(stream_error)) {
    stop(paste0("Anthropic streaming API error: ", stream_error), call. = FALSE)
  }
  if (!saw_message_start || !saw_message_stop) {
    stop(
      "Anthropic stream ended before a complete message was received.",
      call. = FALSE
    )
  }

  response_message$content <- blocks
  .as_claude_response(response_message, events = events)
}

#' Interact with the Anthropic Claude Messages API
#'
#' Send a prompt to a current Claude model. A character prompt is automatically
#' wrapped as a user message; a list can contain complete Messages API message
#' objects, including multimodal content and tool results.
#'
#' @param prompt A single string or a non-empty list of Messages API message
#'   objects containing `role` and `content`.
#' @param model A Claude API model ID or a claudeR shortcut. The shortcuts are
#'   `"fable"`, `"opus"`, `"sonnet"`, and `"haiku"`. The default, `"sonnet"`,
#'   resolves to Claude Sonnet 5 (`claude-sonnet-5`). Unknown model IDs are
#'   passed through unchanged for forward compatibility.
#' @param max_tokens Maximum number of output tokens. The default is 1024.
#' @param stop_sequences Optional character vector of custom stop sequences.
#' @param temperature Optional sampling temperature. Leave `NULL` for current
#'   models; Fable 5, Opus 4.7 and later, and Sonnet 5 reject non-default
#'   sampling parameters.
#' @param top_k Optional top-K sampling value. Leave `NULL` for current models.
#' @param top_p Optional nucleus-sampling value. Leave `NULL` for current models.
#' @param api_key Anthropic API key. When `NULL`, reads
#'   `ANTHROPIC_API_KEY`.
#' @param system_prompt Optional top-level system prompt. May be a string or
#'   content-block list accepted by the Messages API.
#' @param thinking Optional thinking configuration. Current adaptive models use
#'   `list(type = "adaptive")`; add `display = "summarized"` to request a
#'   readable summary. Fable 5 has always-on adaptive thinking and needs no
#'   `thinking` value.
#' @param stream_thinking Compatibility alias for `stream`.
#' @param return_thinking Whether to return a list with `thinking` and `response`
#'   strings. Defaults to `FALSE`.
#' @param stream Whether to stream the response. Defaults to `FALSE`. Text is
#'   written as it arrives; summarized thinking is displayed when
#'   `return_thinking = TRUE`.
#' @param effort Optional reasoning effort, passed as
#'   `output_config$effort` (for example, `"low"`, `"high"`, or `"xhigh"`).
#' @param output_config Optional Messages API output configuration. This can
#'   include structured-output, effort, and task-budget settings.
#' @param betas Optional character vector of Anthropic beta header names.
#' @param return_response Return a structured `claude_response` list containing
#'   text, content blocks, model, usage, stop reason, and refusal details. Use
#'   this for tool use and robust stop-reason handling.
#' @param base_url API base URL. Defaults to the `claudeR.base_url` option, then
#'   `https://api.anthropic.com`.
#' @param ... Additional named Messages API body parameters, such as `tools`,
#'   `tool_choice`, `metadata`, `fallbacks`, or `speed`. Core parameters cannot
#'   be overridden. Wrap custom one-element JSON arrays in `I()`; claudeR does
#'   this automatically for `stop_sequences` and common JSON Schema array fields.
#'
#' @return By default, a character string containing all text blocks. With
#'   `return_thinking = TRUE`, a list containing `thinking` and `response`.
#'   With `return_response = TRUE`, a structured `claude_response` list.
#'
#' @examples
#' \dontrun{
#' # A string prompt and the balanced current default (Claude Sonnet 5)
#' claudeR("What makes an R function pure?", max_tokens = 300)
#'
#' # Claude Opus 4.8 with adaptive thinking
#' result <- claudeR(
#'   "Review this algorithm and find edge cases.",
#'   model = "opus",
#'   thinking = list(type = "adaptive", display = "summarized"),
#'   effort = "high",
#'   return_thinking = TRUE
#' )
#'
#' # Claude Fable 5 has always-on adaptive thinking. Inspect stop_reason so a
#' # successful HTTP refusal is distinguishable from an empty response.
#' result <- claudeR(
#'   "Plan a long-running research task.",
#'   model = "fable",
#'   effort = "high",
#'   return_response = TRUE
#' )
#' result$stop_reason
#' }
#' @export
claudeR <- function(prompt,
                    model = "sonnet",
                    max_tokens = 1024,
                    stop_sequences = NULL,
                    temperature = NULL,
                    top_k = NULL,
                    top_p = NULL,
                    api_key = NULL,
                    system_prompt = NULL,
                    thinking = NULL,
                    stream_thinking = FALSE,
                    return_thinking = FALSE,
                    stream = stream_thinking,
                    effort = NULL,
                    output_config = NULL,
                    betas = NULL,
                    return_response = FALSE,
                    base_url = getOption("claudeR.base_url", "https://api.anthropic.com"),
                    ...) {
  if (!missing(stream) && !missing(stream_thinking) &&
      !identical(stream, stream_thinking)) {
    stop("`stream` and `stream_thinking` cannot have conflicting values.", call. = FALSE)
  }
  for (argument in c("return_thinking", "return_response")) {
    value <- get(argument, inherits = FALSE)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(paste0("`", argument, "` must be TRUE or FALSE."), call. = FALSE)
    }
  }
  api_key <- .claude_api_key(api_key)
  extra <- list(...)
  request <- .build_claude_request(
    prompt = prompt,
    model = model,
    max_tokens = max_tokens,
    stop_sequences = stop_sequences,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    system_prompt = system_prompt,
    thinking = thinking,
    effort = effort,
    output_config = output_config,
    stream = stream,
    extra = extra
  )

  headers <- .claude_headers(api_key, betas)
  url <- .claude_api_url(base_url, "/v1/messages")
  body <- .encode_claude_request(request)

  if (stream) {
    response <- .stream_claude_message(
      url = url,
      headers = headers,
      body = body,
      requested_model = request$model,
      show_thinking = return_thinking,
      keep_events = return_response
    )
  } else {
    raw_response <- POST(
      url,
      add_headers(.headers = headers),
      body = body,
      encode = "raw"
    )
    status <- raw_response$status_code
    response_body <- content(raw_response, "text", encoding = "UTF-8")
    if (status < 200L || status >= 300L) {
      .stop_claude_api_error(status, response_body)
    }
    message <- .parse_json(response_body)
    if (is.null(message)) {
      stop("Anthropic API returned invalid JSON.", call. = FALSE)
    }
    response <- .as_claude_response(message)
  }

  .return_claude_response(response, return_thinking, return_response)
}

#' List or retrieve Claude models
#'
#' Query Anthropic's Models API so applications can discover model IDs and
#' capabilities without waiting for a claudeR release. Package shortcuts are
#' resolved when retrieving a single model.
#'
#' @param model Optional model ID or claudeR shortcut to retrieve. When `NULL`,
#'   returns a page from the model list endpoint.
#' @param api_key Anthropic API key. When `NULL`, reads
#'   `ANTHROPIC_API_KEY`.
#' @param limit Optional positive page size for list requests.
#' @param before_id Optional pagination cursor.
#' @param after_id Optional pagination cursor.
#' @param betas Optional character vector of Anthropic beta header names.
#' @param base_url API base URL. Defaults to the `claudeR.base_url` option, then
#'   `https://api.anthropic.com`.
#'
#' @return The Models API response parsed as nested R lists. List responses
#'   include `data` and pagination fields; single-model responses contain that
#'   model's metadata and capabilities.
#'
#' @examples
#' \dontrun{
#' models <- claude_models()
#' opus <- claude_models("opus")
#' }
#' @export
claude_models <- function(model = NULL,
                          api_key = NULL,
                          limit = 100,
                          before_id = NULL,
                          after_id = NULL,
                          betas = NULL,
                          base_url = getOption("claudeR.base_url", "https://api.anthropic.com")) {
  api_key <- .claude_api_key(api_key)
  headers <- .claude_headers(api_key, betas)

  if (is.null(model)) {
    .validate_number(limit, "limit", lower = 1, integer = TRUE)
    url <- .claude_api_url(base_url, "/v1/models")
    query <- list(limit = limit, before_id = before_id, after_id = after_id)
    query <- query[!vapply(query, is.null, logical(1))]
  } else {
    model <- .resolve_claude_model(model)
    url <- .claude_api_url(
      base_url,
      paste0("/v1/models/", utils::URLencode(model, reserved = TRUE))
    )
    query <- NULL
  }

  raw_response <- GET(
    url,
    add_headers(.headers = headers),
    query = query
  )
  status <- raw_response$status_code
  response_body <- content(raw_response, "text", encoding = "UTF-8")
  if (status < 200L || status >= 300L) {
    .stop_claude_api_error(status, response_body)
  }

  result <- .parse_json(response_body)
  if (is.null(result)) {
    stop("Anthropic Models API returned invalid JSON.", call. = FALSE)
  }
  result
}
