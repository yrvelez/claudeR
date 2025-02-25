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
#' @param thinking (Optional) A list with type="enabled" and budget_tokens to enable Claude's thinking mode.
#' @param stream_thinking (Optional) Whether to stream thinking output in real-time. Default is TRUE.
#' @param return_thinking (Optional) Whether to include thinking output in the final response. Default is FALSE.
#' @return The resulting completion up to and excluding the stop sequences.
#' @export
claudeR <- function(prompt, 
                    model = "claude-3-7-sonnet-20250219", 
                    max_tokens = 100,
                    stop_sequences = NULL,
                    temperature = 0.7, 
                    top_k = -1, 
                    top_p = -1,
                    api_key = NULL, 
                    system_prompt = NULL,
                    thinking = NULL, 
                    stream_thinking = TRUE, 
                    return_thinking = FALSE) {
  
  # Check API key: use provided or environment variable
  if (is.null(api_key)) {
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (api_key == "") {
      stop("Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.")
    }
  }
  
  # For Claude-3 the prompt must be a list of messages.
  if (grepl("claude-3", model) && !is.list(prompt)) {
    stop("Claude-3 requires the input in a list format, e.g., list(list(role = \"user\", content = \"Your question here\"))")
  }
  
  # Set up the API request based on the model type.
  if (grepl("claude-2", model)) {
    # Claude-2 branch code remains the same...
    url <- "https://api.anthropic.com/v1/complete"
    headers <- add_headers(
      "X-API-Key" = api_key,
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01"
    )
    
    # Build the prompt with the User/Assistant convention
    full_prompt <- paste0("\n\nHuman: ", prompt, "\n\nAssistant: ")
    # Use stop sequence to delineate user input.
    stop_sequences <- "\n\nHuman: "
    
    body <- paste0('{
      "prompt": "', gsub("\n", "\\\\n", full_prompt), '",
      "model": "', model, '",
      "max_tokens_to_sample": ', max_tokens, ',
      "stop_sequences": ["', paste(gsub("\n", "\\\\n", stop_sequences), collapse = '", "'), '"],
      "temperature": ', temperature, ',
      "top_k": ', top_k, ',
      "top_p": ', top_p, '
    }')
    
    response <- POST(url, headers, body = body)
    if (http_status(response)$category == "Success") {
      result <- fromJSON(content(response, "text", encoding = "UTF-8"))
      if (!is.null(result$content)) {
        # Process thinking and text blocks if present.
        if (is.list(result$content)) {
          thinking_text <- character(0)
          response_text <- character(0)
          for (block in result$content) {
            if (is.list(block)) {
              if (!is.null(block$type)) {
                if (block$type == "thinking") {
                  thinking_text <- c(thinking_text, block$thinking)
                } else if (block$type == "text") {
                  response_text <- c(response_text, block$text)
                }
              }
            } else if (is.character(block)) {
              response_text <- c(response_text, block)
            }
          }
          if (return_thinking) {
            return(list(thinking = thinking_text, response = response_text))
          } else {
            return(response_text[1])
          }
        }
        return(trimws(result$completion))
      }
    } else {
      warning(paste("API request failed with status", http_status(response)$message))
      cat("Error details:\n", content(response, "text", encoding = "UTF-8"), "\n")
      return(NULL)
    }
    
  } else {
    # --- CLAUDE-3 branch with extended thinking support ---
    url <- "https://api.anthropic.com/v1/messages"
    
    # Build the messages list.
    message_list <- lapply(prompt, function(msg) {
      list(role = msg$role, content = msg$content)
    })
    
    # Validate thinking.budget_tokens against max_tokens
    if (!is.null(thinking) && !is.null(thinking$budget_tokens)) {
      if (max_tokens <= thinking$budget_tokens) {
        # Auto-adjust max_tokens if needed
        max_tokens <- thinking$budget_tokens * 2  # Set to double the thinking budget
        message(paste0("Setting max_tokens to ", max_tokens, " (2x thinking budget)"))
      }
    }
    
    # When thinking is enabled, enforce required parameter settings
    if (!is.null(thinking) && !is.null(thinking$type) && thinking$type == "enabled") {
      temperature <- 1  # Must be exactly 1 when thinking is enabled
      top_p <- NULL     # Must be unset when thinking is enabled
      top_k <- NULL     # Similarly, we'll unset top_k to be safe
    }
    
    # Prepare the request body, conditional inclusion of parameters
    request_body_list <- list(
      model = model,
      max_tokens = max_tokens,
      messages = message_list
    )
    
    # Only add parameters that should be included
    if (!is.null(temperature)) {
      request_body_list$temperature <- temperature
    }
    
    if (!is.null(top_k) && is.null(thinking)) {
      request_body_list$top_k <- top_k
    }
    
    if (!is.null(top_p) && is.null(thinking)) {
      request_body_list$top_p <- top_p
    }
    
    if (!is.null(thinking)) {
      request_body_list$thinking <- thinking
    }
    
    if (!is.null(system_prompt)) {
      request_body_list$system <- system_prompt
    }
    
    # If streaming is enabled, add the stream flag.
    if (stream_thinking) {
      request_body_list$stream <- TRUE
    }
    
    body <- toJSON(request_body_list, auto_unbox = TRUE)
    
    # Set up common headers.
    headers <- c(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type" = "application/json"
    )
    
    # --- Streaming Branch ---
    if (stream_thinking) {
      # Variables to accumulate thinking and response content
      thinking_content <- ""
      response_content <- ""
      
      # Flag to track if we've shown headers
      has_shown_thinking_header <- FALSE
      has_shown_response_header <- FALSE
      
      # Event buffer and state variables for parsing SSE
      event_buffer <- ""
      event_type <- NULL
      event_data <- NULL
      
      # Function to process a complete SSE event
      process_event <- function(event_type, event_data) {
        if (event_type == "content_block_delta") {
          # Try to parse the JSON data
          parsed <- tryCatch(fromJSON(event_data), error = function(e) NULL)
          
          if (!is.null(parsed) && !is.null(parsed$delta$type)) {
            # Process thinking delta
            if (parsed$delta$type == "thinking_delta" && !is.null(parsed$delta$thinking)) {
              # Show header once before starting thinking content
              if (!has_shown_thinking_header) {
                message("\n----- THINKING -----")
                has_shown_thinking_header <<- TRUE
              }
              
              # Display thinking content as a message, only if it contains actual content
              if (nchar(parsed$delta$thinking) > 0) {
                message(parsed$delta$thinking, appendLF = FALSE)
              }
              
              # Accumulate thinking content
              thinking_content <<- paste0(thinking_content, parsed$delta$thinking)
            }
            # Process text delta
            else if (parsed$delta$type == "text_delta" && !is.null(parsed$delta$text)) {
              # Show header once before starting response content
              if (!has_shown_response_header) {
                if (has_shown_thinking_header) {
                  # Add a visual separation
                  message("\n\n----- RESPONSE -----\n")
                }
                has_shown_response_header <<- TRUE
              }
              
              # Display response content directly
              cat(parsed$delta$text)
              
              # Accumulate response content
              response_content <<- paste0(response_content, parsed$delta$text)
            }
          }
        }
      }
      
      # Callback function to process the streaming response
      callback <- function(data, ...) {
        chunk <- rawToChar(data)
        
        # Add the new chunk to our buffer
        event_buffer <<- paste0(event_buffer, chunk)
        
        # Keep processing complete events from the buffer
        while (TRUE) {
          # Find the next complete event (separated by double newline)
          event_end <- regexpr("\n\n", event_buffer)
          
          # If no complete event, break and wait for more data
          if (event_end == -1) {
            break
          }
          
          # Extract the complete event
          complete_event <- substr(event_buffer, 1, event_end - 1)
          # Remove it from the buffer
          event_buffer <<- substr(event_buffer, event_end + 2, nchar(event_buffer))
          
          # Split the event into lines
          event_lines <- strsplit(complete_event, "\n")[[1]]
          
          # Extract event type and data
          for (line in event_lines) {
            if (grepl("^event:", line)) {
              event_type <- trimws(substr(line, 7, nchar(line)))
            } else if (grepl("^data:", line)) {
              event_data <- trimws(substr(line, 6, nchar(line)))
              
              # Process the event with type and data
              process_event(event_type, event_data)
            }
          }
        }
        
        return(length(data))
      }
      
      # Create a new curl handle and set headers and POST fields
      h <- new_handle()
      handle_setheaders(h, .list = headers)
      handle_setopt(h, post = TRUE, postfields = body)
      
      # Perform the streaming request
      curl_fetch_stream(url, callback, handle = h)
      
      # Return the accumulated content
      if (return_thinking) {
        return(list(thinking = thinking_content, response = response_content))
      } else {
        return(response_content)
      }
      
    } else {
      # --- Non-Streaming Branch ---
      response <- POST(url, add_headers(.headers = headers), body = body)
      if (http_status(response)$category == "Success") {
        result <- fromJSON(content(response, "text", encoding = "UTF-8"))
        
        thinking_text <- ""
        response_text <- ""
        
        if (is.list(result$content)) {
          for (block in result$content) {
            if (is.list(block) && !is.null(block$type)) {
              if (block$type == "thinking" && !is.null(block$thinking)) {
                thinking_text <- paste0(thinking_text, block$thinking)
              } else if (block$type == "text" && !is.null(block$text)) {
                response_text <- paste0(response_text, block$text)
              }
            }
          }
          
          if (return_thinking) {
            return(list(thinking = thinking_text, response = response_text))
          } else {
            return(response_text)
          }
        } else if (is.character(result$content)) {
          return(result$content)
        }
      } else {
        warning(paste("API request failed with status", http_status(response)$message))
        cat("Error details:\n", content(response, "text", encoding = "UTF-8"), "\n")
        return(NULL)
      }
    }
  }
}