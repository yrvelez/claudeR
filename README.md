## README: Interact with the Anthropic API in R ##
This R package provides a function, claudeR, to interact with Anthropic's API. 
The package allows you to send text prompts to the API and receive generated responses.
You can customize various parameters like model, maximum tokens, stop sequences, temperature, and others.

# Installation
You can install this package from GitHub using the devtools package:
devtools::install_github("yrvelez/claudeR")

# Usage
To use the package, load it and call the claudeR function:

library(claudeR)

Claude 2 Example:

response <- claudeR(prompt = "What is the capital of France?",
                    model = "claude-2",
                    max_tokens = 50,
                    api_key = "your_api_key_here")

cat(response)

Claude 3 Example:

response <- claudeR(prompt = list(list(role = "user", content = "What is the capital of France?")),
                    model = "claude-3-opus-20240229",
                    max_tokens = 50,
                    api_key = "your_api_key_here")

cat(response)
                    
Replace "your_api_key_here" with your Anthropic's API key. 
You can also set it as an environment variable using Sys.setenv(ANTHROPIC_API_KEY = {Your API KEY here})

# Parameters
The claudeR function accepts the following parameters:

* prompt: The text prompt you want Claude to complete.
* model: The model to use for the request (default: "claude-v1").
* max_tokens: The maximum number of tokens to generate before stopping.
* stop_sequences: A list of strings upon which to stop generating (default: "\n\nHuman: ").
* temperature: Amount of randomness injected into the response (default: 0.7).
* top_k: Only sample from the top K options for each subsequent token (default: -1).
* top_p: Nucleus sampling (default: -1).
* api_key: Your API key for authentication. If not provided, the function will try to fetch it from the ANTHROPIC_API_KEY environment variable.

# License
This package is released under the MIT License.
