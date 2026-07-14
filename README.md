# claudeR: R Interface to Anthropic's Claude

**First Commit: April 29, 2023**

claudeR provides two ways to use Claude from R:

1. **API interface** (`claudeR`) for the Anthropic Messages API, including
   streaming, adaptive thinking, structured responses, and model discovery.
2. **CLI interface** (`claude_code`) for the
   [Claude Code](https://docs.anthropic.com/en/docs/claude-code) CLI, including
   file operations, code execution, and multi-turn agentic work.

The API interface defaults to Claude Sonnet 5 through the `"sonnet"` shortcut.

## Installation

```r
# Install from GitHub
devtools::install_github("yrvelez/claudeR")

# For CLI features, also install Claude Code
# npm install -g @anthropic-ai/claude-code
```

## Quick Start

```r
library(claudeR)
Sys.setenv(ANTHROPIC_API_KEY = "your_api_key_here")

# API: a string is automatically wrapped as a user message
claudeR("What makes an R function pure?", max_tokens = 200)

# CLI: agentic coding (requires the Claude Code CLI)
claude_code("List the files in the current directory")
```

## API Interface

### Basic Usage

```r
# Claude Sonnet 5 is the balanced default
response <- claudeR("Explain tibbles in R", max_tokens = 300)

# Friendly shortcuts select the current model in each family
response <- claudeR(
  "Review this R function for edge cases",
  model = "opus",
  max_tokens = 500
)

# Complete Messages API message lists are also accepted
response <- claudeR(
  prompt = list(
    list(role = "user", content = "My data contain missing values."),
    list(role = "assistant", content = "What would you like to estimate?"),
    list(role = "user", content = "A robust mean in R.")
  ),
  system_prompt = "You are an expert R programmer. Be concise.",
  max_tokens = 500
)
```

### Current Models and Shortcuts

| Model | Shortcut | API model ID | Best fit |
|-------|----------|--------------|----------|
| Claude Fable 5 | `"fable"` | `claude-fable-5` | Highest-capability, long-running work |
| Claude Opus 4.8 | `"opus"` | `claude-opus-4-8` | Complex agentic coding and knowledge work |
| Claude Sonnet 5 | `"sonnet"` | `claude-sonnet-5` | Balanced speed and capability; package default |
| Claude Haiku 4.5 | `"haiku"` | `claude-haiku-4-5-20251001` | Fast, cost-efficient tasks |

You may also supply any full model ID. Unknown IDs are passed through unchanged
so a newly released model can be used before claudeR is updated.

Starting with the Claude 4.6 generation, dateless IDs such as
`claude-opus-4-8` and `claude-sonnet-5` are canonical, pinned model versions;
they are not aliases that silently advance to a later release. The Haiku
shortcut deliberately resolves to the dated, pinned 4.5 ID. Anthropic also
accepts the convenience API alias `claude-haiku-4-5`.

Query the live Models API when model availability or capabilities matter:

```r
models <- claude_models(limit = 25)
model_ids <- vapply(models$data, `[[`, character(1), "id")

# Shortcuts are resolved when retrieving one model
opus <- claude_models("opus")
opus$id
opus$capabilities
```

### Adaptive Thinking and Effort

The current Fable, Opus, and Sonnet models use adaptive thinking rather than a
fixed thinking-token budget. `effort` is placed in `output_config$effort` for
you.

```r
result <- claudeR(
  "Review this algorithm and identify subtle failure modes.",
  model = "opus",
  thinking = list(type = "adaptive", display = "summarized"),
  effort = "high",
  return_thinking = TRUE,
  max_tokens = 4000
)

cat("Thinking summary:\n", result$thinking, "\n\n")
cat("Answer:\n", result$response)
```

Fable 5 has adaptive thinking always on. Do not send a manual budget or
`thinking = list(type = "disabled")`. Fable can return a classifier refusal as
a successful HTTP response, so use `return_response = TRUE` when the distinction
between an empty answer and a refusal matters:

```r
result <- claudeR(
  "Plan a long-running research workflow.",
  model = "fable",
  effort = "high",
  return_response = TRUE,
  max_tokens = 4000
)

if (identical(result$stop_reason, "refusal")) {
  detail <- result$stop_details$explanation
  if (is.null(detail)) detail <- "Claude declined the request."
  message(detail)
} else {
  cat(result$response)
}
```

Without `return_response = TRUE`, a refusal produces a warning and returns
`NULL`. Structured responses also retain content blocks, usage, model,
`stop_reason`, and `stop_details` for tool-use and other advanced workflows.

### Beta Headers and Fallbacks

Pass beta header names with `betas`; additional named Messages API fields pass
through `...`. For example, the current server-side fallback beta can retry a
Fable refusal with Opus 4.8:

```r
result <- claudeR(
  "Analyze this difficult task.",
  model = "fable",
  return_response = TRUE,
  betas = "server-side-fallback-2026-06-01",
  fallbacks = list(list(model = "claude-opus-4-8"))
)
```

Beta names and availability can change; check Anthropic's documentation before
depending on a beta in production. Nested passthrough fields such as
`fallbacks` require full API model IDs rather than claudeR shortcuts.

### Main API Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `prompt` | One string or a non-empty list of Messages API messages | required |
| `model` | Full model ID or claudeR shortcut | `"sonnet"` |
| `max_tokens` | Maximum output tokens | `1024` |
| `system_prompt` | Top-level system instructions | `NULL` |
| `thinking` | Adaptive-thinking configuration | `NULL` |
| `effort` | Reasoning effort, added to `output_config` | `NULL` |
| `stream` | Stream the response | `FALSE` |
| `return_thinking` | Return `thinking` and `response` strings | `FALSE` |
| `return_response` | Return the structured response and stop metadata | `FALSE` |
| `betas` | Anthropic beta header names | `NULL` |
| `...` | Additional named Messages API body fields | none |

Leave `temperature`, `top_k`, and `top_p` as `NULL` for current adaptive
models, which reject non-default sampling parameters. The historical
`stream_thinking` argument remains as a compatibility alias for `stream`.

## CLI Interface (Claude Code)

The CLI interface lets Claude read and write files, execute code, and perform
multi-step tasks.

### Setup

```r
claude_code_available(verbose = TRUE)
claude_code_config_print()
claude_code_config(timeout = 600)
```

### Basic Usage

```r
claude_code("What files are in the current directory?")

result <- claude_code(
  "List 3 R packages for visualization",
  output_format = "json"
)

claude_code(
  "Analyze data.csv",
  allowed_tools = c("Read", "Bash")
)
```

The `model` argument to `claude_code()` is passed verbatim to the installed
Claude Code CLI as `--model`. It does not use the API shortcut resolver above;
model aliases, IDs, access, and defaults are determined by your Claude Code
installation and configuration.

```r
claude_code("Review this repository", model = "opus")
```

### The Claude Pipe Operator

```r
mtcars %|c>%
  "Generate a figure with mpg on the x axis and wt on the y axis" %|c>%
  "Facet by cylinders" %|c>%
  "Maximize the data-ink ratio"
```

### Convenience Functions

```r
mtcars %|c>% "Describe this dataset and suggest visualizations"
claude_code_file("analysis.R", "Review this code for potential issues")
claude_code_review("script.R", focus = "performance")
claude_code_generate("function to calculate a rolling mean", language = "R")
claude_code_analyze(iris, "What patterns exist in this data?")
```

### Batch and Session Management

```r
results <- claude_code_batch(
  c("Explain ggplot2", "Explain dplyr", "Explain tidyr"),
  progress = TRUE
)

claude_code_sessions()
claude_code_session("Continue our analysis", session_id = "last")
claude_code_chat()
```

### Response Utilities

```r
response <- claude_code("Write an R function to parse JSON")
code <- claude_code_extract(response, language = "r")
data <- claude_code_parse(response)
claude_code_print(response)
```

## When to Use API vs CLI

| Use case | Recommended interface |
|----------|-----------------------|
| Direct Messages API request | `claudeR()` |
| Streaming or adaptive thinking | `claudeR()` |
| Model discovery and structured stop handling | `claudeR()` |
| File operations or code execution | `claude_code()` |
| Multi-step repository work | `claude_code()` |
| Interactive coding | `claude_code_chat()` |

## Further Resources

- [Anthropic Models overview](https://platform.claude.com/docs/en/about-claude/models/overview)
- [Model IDs and versioning](https://platform.claude.com/docs/en/about-claude/models/model-ids-and-versions)
- [Anthropic API documentation](https://platform.claude.com/docs/en/api/overview)
- [Claude Code documentation](https://docs.anthropic.com/en/docs/claude-code)

## License

MIT License
