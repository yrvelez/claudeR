test_that("API keys are isolated and explicit values take precedence", {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY", unset = NA_character_)
  on.exit({
    if (is.na(old_key)) {
      Sys.unsetenv("ANTHROPIC_API_KEY")
    } else {
      Sys.setenv(ANTHROPIC_API_KEY = old_key)
    }
  }, add = TRUE)

  Sys.unsetenv("ANTHROPIC_API_KEY")
  expect_error(.claude_api_key(NULL), "Please provide an API key")

  Sys.setenv(ANTHROPIC_API_KEY = "environment-key")
  expect_identical(.claude_api_key(NULL), "environment-key")
  expect_identical(.claude_api_key("explicit-key"), "explicit-key")

  expect_error(.claude_api_key(""), "Please provide an API key")
  expect_error(.claude_api_key(c("one", "two")), "Please provide an API key")
})

test_that("model shortcuts resolve case-insensitively and unknown IDs pass through", {
  expected <- c(
    fable = "claude-fable-5",
    opus = "claude-opus-4-8",
    sonnet = "claude-sonnet-5",
    haiku = "claude-haiku-4-5-20251001"
  )

  for (shortcut in names(expected)) {
    expect_identical(.resolve_claude_model(shortcut), unname(expected[[shortcut]]))
    expect_identical(
      .resolve_claude_model(toupper(shortcut)),
      unname(expected[[shortcut]])
    )
  }

  future_model <- "claude-future-model-20300101"
  expect_identical(.resolve_claude_model(future_model), future_model)
  expect_error(.resolve_claude_model(""), "non-empty string")
  expect_error(.resolve_claude_model(NA_character_), "non-empty string")
  expect_error(.resolve_claude_model(c("opus", "sonnet")), "non-empty string")
})

test_that("string prompts are wrapped and complete message objects are preserved", {
  expect_identical(
    .normalize_claude_messages("hello\nworld"),
    list(list(role = "user", content = "hello\nworld"))
  )

  messages <- list(
    list(
      role = "user",
      content = list(
        list(type = "text", text = "inspect this", cache_control = list(type = "ephemeral")),
        list(type = "image", source = list(type = "base64", media_type = "image/png", data = "AA=="))
      ),
      metadata = list(turn = 1L)
    ),
    list(
      role = "assistant",
      content = list(list(type = "tool_result", tool_use_id = "tool-1", content = "ok"))
    )
  )
  expect_identical(.normalize_claude_messages(messages), messages)

  expect_error(.normalize_claude_messages(character()), "single, non-missing")
  expect_error(.normalize_claude_messages(NA_character_), "single, non-missing")
  expect_error(.normalize_claude_messages(list()), "non-empty list")
  expect_error(
    .normalize_claude_messages(list(list(role = "user"))),
    "must contain a non-empty `role` and a `content`"
  )
})

test_that("the default request resolves Sonnet 5 and omits optional sampling and streaming", {
  request <- .build_claude_request("hello")

  expect_identical(request$model, "claude-sonnet-5")
  expect_identical(request$max_tokens, 1024)
  expect_identical(
    request$messages,
    list(list(role = "user", content = "hello"))
  )
  expect_identical(names(request), c("model", "max_tokens", "messages"))
  expect_false(any(c("temperature", "top_k", "top_p", "stream") %in% names(request)))

  streamed <- .build_claude_request("hello", model = "haiku", stream = TRUE)
  expect_true(streamed$stream)
})

test_that("stream is the preferred switch and conflicting compatibility values fail early", {
  expect_error(
    claudeR("hello", stream = TRUE, stream_thinking = FALSE),
    "`stream` and `stream_thinking` cannot have conflicting values"
  )
  expect_error(
    claudeR("hello", stream = FALSE, stream_thinking = TRUE),
    "`stream` and `stream_thinking` cannot have conflicting values"
  )
})

test_that("request options preserve stops, adaptive thinking, effort, output config, and betas", {
  request <- .build_claude_request(
    "reason about this",
    model = "opus",
    stop_sequences = c("STOP", "DONE"),
    thinking = list(type = "adaptive", display = "summarized"),
    effort = "high",
    output_config = list(format = list(type = "json_schema")),
    extra = list(metadata = list(user_id = "test-user"), speed = "fast")
  )

  expect_identical(request$stop_sequences, c("STOP", "DONE"))
  expect_identical(
    request$thinking,
    list(type = "adaptive", display = "summarized")
  )
  expect_identical(request$output_config$effort, "high")
  expect_identical(request$output_config$format$type, "json_schema")
  expect_identical(request$metadata$user_id, "test-user")
  expect_identical(request$speed, "fast")

  expect_error(
    .build_claude_request(
      "hello",
      model = "opus",
      effort = "high",
      output_config = list(effort = "low")
    ),
    "conflicts with `output_config\\$effort`"
  )

  headers <- .claude_headers("secret", c("feature-one", "feature-two"))
  expect_identical(unname(headers[["anthropic-version"]]), "2023-06-01")
  expect_identical(unname(headers[["anthropic-beta"]]), "feature-one,feature-two")
  expect_error(.claude_headers("secret", c("valid", "")), "character vector")
})

test_that("one-element API and JSON Schema arrays remain JSON arrays", {
  encoded <- .encode_claude_request(list(
    model = "claude-sonnet-5",
    stop_sequences = "STOP",
    output_config = list(
      format = list(
        schema = list(required = "answer", enum = "complete")
      )
    )
  ))
  parsed <- jsonlite::fromJSON(encoded, simplifyVector = FALSE)

  expect_type(parsed$stop_sequences, "list")
  expect_identical(parsed$stop_sequences[[1L]], "STOP")
  expect_type(parsed$output_config$format$schema$required, "list")
  expect_identical(parsed$output_config$format$schema$required[[1L]], "answer")
  expect_type(parsed$output_config$format$schema$enum, "list")
})

test_that("Fable, Opus, and Sonnet reject legacy thinking and sampling controls", {
  for (model in c("fable", "opus", "sonnet")) {
    expect_error(
      .build_claude_request(
        "hello",
        model = model,
        thinking = list(type = "enabled", budget_tokens = 512)
      ),
      "does not support manual thinking budgets"
    )

    expect_error(
      .build_claude_request(
        "hello",
        model = model,
        temperature = 0.5,
        top_k = 20,
        top_p = 0.9
      ),
      "does not accept non-default sampling parameters.*temperature, top_k, top_p"
    )

    expect_error(
      .build_claude_request(
        list(
          list(role = "user", content = "hello"),
          list(role = "assistant", content = "prefill")
        ),
        model = model
      ),
      "does not support assistant-message prefills"
    )
  }
})

test_that("modern models allow API compatibility sampling defaults", {
  for (model in c("fable", "opus", "sonnet")) {
    request <- .build_claude_request(
      "hello",
      model = model,
      temperature = 1,
      top_p = 0.99
    )
    expect_identical(request$temperature, 1)
    expect_identical(request$top_p, 0.99)
    expect_error(
      .build_claude_request("hello", model = model, top_k = 1),
      "does not accept non-default sampling parameters"
    )
  }
})

test_that("Fable cannot disable thinking", {
  expect_error(
    .build_claude_request(
      "hello",
      model = "fable",
      thinking = list(type = "disabled")
    ),
    "always-on adaptive thinking and cannot disable it"
  )
})

test_that("Claude 2 model IDs fail before any API request", {
  for (model in c("claude-2", "claude-2.0", "claude-2-legacy")) {
    expect_error(
      .build_claude_request("hello", model = model),
      "Claude 2 and the Text Completions API are retired"
    )
  }

  expect_identical(
    .build_claude_request("hello", model = "not-claude-2")$model,
    "not-claude-2"
  )
})

test_that("additional request parameters must be named and cannot override core fields", {
  expect_error(
    .build_claude_request("hello", extra = list("fast")),
    "must all be named"
  )
  expect_error(
    .build_claude_request(
      "hello",
      extra = structure(list("fast", "value"), names = c("speed", ""))
    ),
    "must all be named"
  )
  expect_error(
    .build_claude_request("hello", extra = list(messages = list())),
    "cannot override: messages"
  )
  expect_error(
    .build_claude_request("hello", extra = list(model = "another-model")),
    "cannot override: model"
  )
})

test_that("response extraction combines text and thinking while preserving structure", {
  content <- list(
    list(type = "thinking", thinking = "first "),
    list(type = "text", text = "Hello"),
    list(type = "tool_use", id = "tool-1", name = "search", input = list(q = "R")),
    list(type = "thinking", thinking = "second"),
    list(type = "text", text = " world")
  )
  extracted <- .extract_claude_content(content)
  expect_identical(extracted$response, "Hello world")
  expect_identical(extracted$thinking, "first second")
  expect_identical(
    .extract_claude_content(c("one", "two")),
    list(response = "onetwo", thinking = "")
  )

  response <- .as_claude_response(list(
    id = "msg-1",
    model = "claude-sonnet-5",
    content = content,
    stop_reason = "end_turn",
    stop_sequence = NULL,
    stop_details = NULL,
    usage = list(input_tokens = 10L, output_tokens = 4L)
  ))
  expect_s3_class(response, "claude_response")
  expect_identical(response$response, "Hello world")
  expect_identical(response$thinking, "first second")
  expect_identical(response$content, content)

  expect_identical(
    .return_claude_response(response, return_thinking = FALSE, return_response = FALSE),
    "Hello world"
  )
  expect_identical(
    .return_claude_response(response, return_thinking = TRUE, return_response = FALSE),
    list(thinking = "first second", response = "Hello world")
  )
  expect_identical(
    .return_claude_response(response, return_thinking = FALSE, return_response = TRUE),
    response
  )
})

test_that("refusals warn and return NULL unless structured responses are requested", {
  refusal <- .as_claude_response(list(
    id = "msg-refusal",
    model = "claude-fable-5",
    content = list(),
    stop_reason = "refusal",
    stop_sequence = NULL,
    stop_details = list(explanation = "This request cannot be completed."),
    usage = list()
  ))

  result <- "not-null"
  expect_warning(
    result <- .return_claude_response(
      refusal,
      return_thinking = FALSE,
      return_response = FALSE
    ),
    "This request cannot be completed"
  )
  expect_null(result)
  expect_identical(
    suppressWarnings(
      .return_claude_response(refusal, return_thinking = FALSE, return_response = TRUE)
    ),
    refusal
  )

  refusal$stop_details <- NULL
  expect_warning(
    .return_claude_response(refusal, return_thinking = FALSE, return_response = FALSE),
    "Claude declined the request"
  )
})

test_that("SSE buffers split correctly with LF, CRLF, and partial events", {
  first <- paste0(
    "event: message_start\n",
    "data: {\"type\":\"message_start\",\"message\":{\"id\":\"m1\"}}"
  )
  second <- paste0(
    "event: content_block_delta\r\n",
    "data: {\"type\":\"content_block_delta\",\"index\":0,",
    "\"delta\":{\"type\":\"text_delta\",\"text\":\"Hi\"}}"
  )
  partial <- "event: message_stop\ndata: {\"type\":\"message_stop\"}"

  split <- .split_sse_buffer(paste0(first, "\n\n", second, "\r\n\r\n", partial))
  expect_identical(split$events, c(first, second))
  expect_identical(split$remainder, partial)

  parsed_lf <- .parse_sse_event(first)
  expect_identical(parsed_lf$type, "message_start")
  expect_identical(parsed_lf$data$message$id, "m1")

  parsed_crlf <- .parse_sse_event(second)
  expect_identical(parsed_crlf$type, "content_block_delta")
  expect_identical(parsed_crlf$data$delta$text, "Hi")

  expect_null(.parse_sse_event("event: ping"))
  expect_null(.parse_sse_event("event: error\ndata: not-json"))
})

test_that("raw SSE buffering tolerates split UTF-8 code points", {
  complete <- charToRaw(paste0(
    "event: ping\n",
    "data: {\"type\":\"ping\"}\n\n"
  ))
  emoji <- charToRaw("😀")
  buffer <- c(complete, emoji[1:2])
  split <- .split_sse_raw_buffer(buffer)

  expect_length(split$events, 1L)
  expect_identical(.parse_sse_event(rawToChar(split$events[[1L]]))$data$type, "ping")
  expect_identical(split$remainder, emoji[1:2])

  completed <- c(split$remainder, emoji[3:4], charToRaw("\n\n"))
  final <- .split_sse_raw_buffer(completed)
  expect_length(final$events, 1L)
  expect_identical(rawToChar(final$events[[1L]]), "😀")
})

test_that("streaming tracks fallback models and rejects truncated streams", {
  event <- function(type, data) {
    paste0("event: ", type, "\n", "data: ", data, "\n\n")
  }
  stream <- paste0(
    event(
      "message_start",
      paste0(
        '{"type":"message_start","message":{"id":"m1","type":"message",',
        '"role":"assistant","model":"claude-fable-5","content":[],',
        '"stop_reason":null,"stop_sequence":null,"usage":{"input_tokens":2}}}'
      )
    ),
    event(
      "content_block_start",
      paste0(
        '{"type":"content_block_start","index":0,"content_block":',
        '{"type":"fallback","from":{"model":"claude-fable-5"},',
        '"to":{"model":"claude-opus-4-8"}}}'
      )
    ),
    event(
      "content_block_stop",
      '{"type":"content_block_stop","index":0}'
    ),
    event(
      "content_block_start",
      paste0(
        '{"type":"content_block_start","index":1,',
        '"content_block":{"type":"text","text":""}}'
      )
    ),
    event(
      "content_block_delta",
      paste0(
        '{"type":"content_block_delta","index":1,',
        '"delta":{"type":"text_delta","text":"Hi 😀"}}'
      )
    ),
    event(
      "content_block_stop",
      '{"type":"content_block_stop","index":1}'
    ),
    event(
      "message_delta",
      paste0(
        '{"type":"message_delta","delta":{"stop_reason":"end_turn",',
        '"stop_sequence":null,"stop_details":null},',
        '"usage":{"output_tokens":3}}'
      )
    ),
    event("message_stop", '{"type":"message_stop"}')
  )
  bytes <- charToRaw(stream)
  emoji_start <- which(bytes == as.raw(0xF0))[[1L]]
  chunks <- list(
    bytes[seq_len(emoji_start + 1L)],
    bytes[(emoji_start + 2L):length(bytes)]
  )

  local_mocked_bindings(
    new_handle = function() structure(list(), class = "mock_handle"),
    handle_setheaders = function(...) invisible(NULL),
    handle_setopt = function(...) invisible(NULL),
    curl_fetch_stream = function(url, callback, handle) {
      for (chunk in chunks) callback(chunk)
      list(status_code = 200L)
    },
    .package = "claudeR"
  )

  output <- capture.output(
    response <- .stream_claude_message(
      "https://example.invalid/v1/messages",
      headers = character(),
      body = "{}",
      requested_model = "claude-fable-5",
      keep_events = TRUE
    ),
    type = "output"
  )
  expect_match(paste(output, collapse = ""), "Hi 😀", fixed = TRUE)
  expect_identical(response$model, "claude-opus-4-8")
  expect_identical(response$response, "Hi 😀")
  expect_identical(response$stop_reason, "end_turn")
  expect_true(length(response$events) > 0L)

  truncated <- charToRaw(event(
    "message_start",
    '{"type":"message_start","message":{"id":"m1","content":[]}}'
  ))
  local_mocked_bindings(
    curl_fetch_stream = function(url, callback, handle) {
      callback(truncated)
      list(status_code = 200L)
    },
    .package = "claudeR"
  )
  expect_error(
    .stream_claude_message(
      "https://example.invalid/v1/messages",
      headers = character(),
      body = "{}",
      requested_model = "claude-opus-4-8"
    ),
    "ended before a complete message"
  )
})

test_that("API errors prefer structured details and fall back to HTTP status", {
  expect_error(
    .stop_claude_api_error(
      429L,
      '{"type":"error","error":{"type":"rate_limit_error","message":"Slow down."}}'
    ),
    "Anthropic API request failed \\(429\\): Slow down\\."
  )
  expect_error(
    .stop_claude_api_error(400L, '{"message":"Malformed request."}'),
    "Anthropic API request failed \\(400\\): Malformed request\\."
  )
  expect_error(
    .stop_claude_api_error(500L, "not-json"),
    "Anthropic API request failed \\(500\\):"
  )
  expect_null(.parse_json("not-json"))
})

test_that("claudeR sends a complete request and parses a response without network", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      captured$url <- url
      captured$config <- list(...)
      captured$body <- body
      captured$encode <- encode
      list(
        status_code = 200L,
        body = paste0(
          '{"id":"msg-1","model":"claude-sonnet-5",',
          '"content":[{"type":"text","text":"offline response"}],',
          '"stop_reason":"end_turn","stop_sequence":null,',
          '"stop_details":null,"usage":{"input_tokens":2,"output_tokens":2}}'
        )
      )
    },
    content = function(x, ...) x$body,
    .package = "claudeR"
  )

  result <- claudeR(
    "hello",
    api_key = "test-key",
    betas = "test-beta",
    stop_sequences = "STOP",
    metadata = list(user_id = "offline-test")
  )

  expect_identical(result, "offline response")
  expect_identical(captured$url, "https://api.anthropic.com/v1/messages")
  expect_identical(captured$encode, "raw")

  body <- jsonlite::fromJSON(captured$body, simplifyVector = FALSE)
  expect_identical(body$model, "claude-sonnet-5")
  expect_identical(body$messages[[1L]]$role, "user")
  expect_identical(body$messages[[1L]]$content, "hello")
  expect_identical(body$stop_sequences[[1L]], "STOP")
  expect_identical(body$metadata$user_id, "offline-test")
  expect_null(body$stream)
  expect_null(body$temperature)
})

test_that("claude_models lists and retrieves resolved models without network", {
  calls <- list()
  local_mocked_bindings(
    GET = function(url, ..., query = NULL) {
      calls[[length(calls) + 1L]] <<- list(url = url, query = query)
      if (grepl("/v1/models/", url, fixed = TRUE)) {
        list(
          status_code = 200L,
          body = '{"id":"claude-opus-4-8","display_name":"Claude Opus 4.8"}'
        )
      } else {
        list(
          status_code = 200L,
          body = paste0(
            '{"data":[{"id":"claude-sonnet-5"}],',
            '"has_more":false,"first_id":"claude-sonnet-5",',
            '"last_id":"claude-sonnet-5"}'
          )
        )
      }
    },
    content = function(x, ...) x$body,
    .package = "claudeR"
  )

  listed <- claudeR:::claude_models(
    api_key = "test-key",
    limit = 25,
    after_id = "cursor-1"
  )
  retrieved <- claudeR:::claude_models("opus", api_key = "test-key")

  expect_identical(listed$data[[1L]]$id, "claude-sonnet-5")
  expect_identical(calls[[1L]]$url, "https://api.anthropic.com/v1/models")
  expect_identical(calls[[1L]]$query$limit, 25)
  expect_identical(calls[[1L]]$query$after_id, "cursor-1")

  expect_identical(retrieved$id, "claude-opus-4-8")
  expect_identical(
    calls[[2L]]$url,
    "https://api.anthropic.com/v1/models/claude-opus-4-8"
  )
  expect_null(calls[[2L]]$query)
})
