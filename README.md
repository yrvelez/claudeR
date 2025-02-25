# claudeR: R Interface to Anthropic's Claude API

This package provides a comprehensive R interface to Anthropic's Claude AI models, including Claude 2 and the Claude 3 family. The latest version adds support for Claude 3.7's extended thinking capabilities, allowing for more thorough reasoning and complex problem-solving.

## Installation

You can install this package from GitHub using the devtools package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install claudeR
devtools::install_github("yrvelez/claudeR")
```

## Basic Usage

Load the package and set your API key:

```r
library(claudeR)

# Set your API key as an environment variable (recommended)
Sys.setenv(ANTHROPIC_API_KEY = "your_api_key_here")

# Or pass it directly in function calls
# api_key = "your_api_key_here"
```

### Claude 2 Example

```r
response <- claudeR(
  prompt = "What is the capital of France?",
  model = "claude-2",
  max_tokens = 50
)

cat(response)
```

### Claude 3 Example

```r
response <- claudeR(
  prompt = list(list(role = "user", content = "What is the capital of France?")),
  model = "claude-3-opus-20240229",
  max_tokens = 50
)

cat(response)
```

### Claude 3.7 with Extended Thinking

Claude 3.7 introduces an extended thinking capability that improves reasoning for complex problems:

```r
result <- claudeR(
  prompt = list(list(role = "user", content = "Solve this math problem: If f(x) = 2x² + 3x - 5, find all values of x where f(x) = 20")),
  model = "claude-3-7-sonnet-20250219",
  thinking = list(type = "enabled", budget_tokens = 2000),
  return_thinking = TRUE
)

# Access thinking and response separately
cat("Thinking process:\n", result$thinking, "\n\n")
cat("Final answer:\n", result$response)
```

## Function Parameters

| Parameter | Description |
|-----------|-------------|
| `prompt` | For Claude 2: A string with the prompt text. For Claude 3: A list of message objects with `role` and `content` fields. |
| `model` | The model to use. Default: `"claude-3-7-sonnet-20250219"`. |
| `max_tokens` | Maximum number of tokens to generate. Default: `100`. |
| `stop_sequences` | A list of strings upon which to stop generating. |
| `temperature` | Controls randomness (0-1). Default: `0.7`. Must be exactly `1` when using extended thinking. |
| `top_k` | Only sample from top K options for each token. Default: `-1`. Automatically disabled with extended thinking. |
| `top_p` | Controls diversity via nucleus sampling. Default: `-1`. Automatically disabled with extended thinking. |
| `api_key` | Your API key. If `NULL`, reads from `ANTHROPIC_API_KEY` environment variable. |
| `system_prompt` | Optional system instructions for Claude 3 models. |
| `thinking` | For Claude 3.7: A list with `type="enabled"` and `budget_tokens` to activate extended thinking. |
| `stream_thinking` | Whether to stream the response in real-time. Default: `TRUE`. |
| `return_thinking` | Whether to return thinking output. Default: `FALSE`. |

## Extended Thinking Mode

Claude 3.7 features an advanced extended thinking mode that helps the model solve complex reasoning problems more effectively. To use this feature:

1. Set `thinking = list(type = "enabled", budget_tokens = N)` where N is the token budget for thinking
2. Set `return_thinking = TRUE` to see the thinking process
3. When using extended thinking, the following constraints apply:
   - `temperature` must be set to exactly 1 (handled automatically)
   - `top_p` and `top_k` parameters are disabled (handled automatically)
   - `max_tokens` must be greater than `budget_tokens` (auto-adjusted if needed)

Example with formatted output:

```r
result <- claudeR(
  prompt = list(list(role = "user", content = "Solve this combinatorial problem: In how many ways can 8 people be seated at a round table, considering that rotations of the same arrangement are considered the same?")),
  model = "claude-3-7-sonnet-20250219",
  thinking = list(type = "enabled", budget_tokens = 3000),
  max_tokens = 4000,
  return_thinking = TRUE
)
```

This will display:
- Thinking process with a clear header
- Final response with its own header
- Return a list with both components accessible as `result$thinking` and `result$response`

## License

This package is released under the MIT License.