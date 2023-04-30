#' Interact with the Anthropics Claude API
#'
#' @param api_key Your API key for authentication.
#' @param prompt The prompt you want Claude to complete.
#' @param model The model to use for the request.
#' @param max_tokens A maximum number of tokens to generate before stopping.
#' @param stop_sequences (Optional) A list of strings upon which to stop generating.
#' @param temperature (Optional) Amount of randomness injected into the response.
#' @param top_k (Optional) Only sample from the top K options for each subsequent token.
#' @param top_p (Optional) Does nucleus sampling.
#' @return The resulting completion up to and excluding the stop sequences.
#' @export
claudeR <- function(prompt, model = "claude-v1", max_tokens, 
                                 stop_sequences = '\n\nHuman: ', 
                                 temperature = .7, top_k = -1, top_p = -1,
                                 api_key = NULL) {
  # Load required libraries
  library(httr)
  library(jsonlite)
  
  # Check if the API key is provided or available in the environment
  if (is.null(api_key)) {
    api_key <- paste(Sys.getenv("ANTHROPIC_API_KEY"))
    if (api_key == "") {
      stop("Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.")
    }
  }
  
  # Set up the API request
  url <- "https://api.anthropic.com/v1/complete"
  headers <- add_headers(
    "X-API-Key" = api_key,
    "Content-Type" = "application/json"
  )
  
  # Build the prompt with the User/Assistant convention
  prompt <- paste0("\n\nHuman: ", prompt, "\n\nAssistant: ")
  
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
