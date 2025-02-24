#' Interact with the Anthropics Claude API
#'
#' @param api_key Your API key for authentication.
#' @param prompt A string vector for Claude-2, or a list for Claude-3 specifying the input for the model.
#' @param model The model to use for the request. Default is the latest Claude-3 model.
#' @param max_tokens A maximum number of tokens to generate before stopping.
#' @param stop_sequences (Optional) A list of strings upon which to stop generating.
#' @param temperature (Optional) Amount of randomness injected into the response.
#' @param top_k (Optional) Only sample from the top K options for each subsequent token.
#' @param top_p (Optional) Does nucleus sampling.
#' @param system_prompt (Optional) An optional system role specification.
#' @return The resulting completion up to and excluding the stop sequences.
#' @export
claudeR <- function(prompt, model = "claude-3-7-sonnet-20250219", max_tokens = 100,
                    stop_sequences = NULL,
                    temperature = .7, top_k = -1, top_p = -1,
                    api_key = NULL, system_prompt = NULL) {

  # Load required libraries
  library(httr)
  library(jsonlite)

  if (grepl("claude-3", model) && !is.list(prompt)) {
    stop("Claude-3 requires the input in a list format, e.g., list(list(role = \"user\", content = \"What is the capital of France?\"))")
  }

  # Check if the API key is provided or available in the environment
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (api_key == "") {
      stop("Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.")
    }
  }

  # Set up conditions for claude-2
  if (grepl("claude-2", model)) {

    url <- "https://api.anthropic.com/v1/complete"
    headers <- add_headers(
      "X-API-Key" = api_key,
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01"
    )

    # Build the prompt with the User/Assistant convention
    prompt <- paste0("\n\nHuman: ", prompt, "\n\nAssistant: ")

    # Stop sequences
    stop_sequences = '\n\nHuman: '

    body <- paste0('{
  "prompt": "', gsub("\n", "\\\\n", prompt), '",
  "model": "', model, '",
  "max_tokens_to_sample": ', max_tokens, ',
  "stop_sequences": ["', paste(gsub("\n", "\\\\n", stop_sequences), collapse = '", "'), '"],
  "temperature": ', temperature, ',
  "top_k": ', top_k, ',
  "top_p": ', top_p, '
}')

    # Send the API request
    response <- POST(url, headers, body = body)

    # Check if the request was successful
    if (http_status(response)$category == "Success") {
      # Parse the JSON response
      result <- fromJSON(content(response, "text", encoding = "UTF-8"))
      return(trimws(result$completion))
    } else {
      # Print the error message
      warning(paste("API request failed with status", http_status(response)$message))
      cat(api_key)
      cat("Error details:\n", content(response, "text", encoding = "UTF-8"), "\n")
      return(NULL)
    }

  }

  # Set up the API request
  url <- "https://api.anthropic.com/v1/messages"

  headers <- add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "Content-Type" = "application/json"
  )

  # Prepare the messages list
  message_list <- lapply(prompt, function(msg) {
    list(role = msg$role, content = msg$content)
  })

  # Prepare the request body as a list
  request_body_list <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    messages = message_list
    )

  # Include the system prompt if provided
  if (!is.null(system_prompt)) {
    request_body_list$system = system_prompt
  }

  # Convert the modified list to JSON
  body <- toJSON(request_body_list, auto_unbox = TRUE)
  

  # Send the API request
  response <- POST(url, headers, body = body)

  # Check if the request was successful
  if (http_status(response)$category == "Success") {
    # Parse the JSON response
    result <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(result$content$text)
  } else {
    # Print the error message
    warning(paste("API request failed with status", http_status(response)$message))
    cat("Error details:\n", content(response, "text", encoding = "UTF-8"), "\n")
    return(NULL)
  }
}
