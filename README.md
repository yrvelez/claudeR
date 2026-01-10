# claudeR: R Interface to Anthropic's Claude

**First Commit: April 29, 2023**

This package provides two ways to interact with Claude from R:

1. **API Interface** (`claudeR`) - Direct API calls for text completions with streaming and extended thinking support
2. **CLI Interface** (`claude_code`) - Integration with [Claude Code](https://docs.anthropic.com/en/docs/claude-code), Anthropic's agentic coding CLI for file operations, code execution, and multi-turn interactions

Supports Claude 2, Claude 3, Claude 3.7, and Claude 4.5 models (Sonnet, Opus, Haiku). Defaults to Claude 4.5 Sonnet.

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

# API: Simple completion
claudeR(
 prompt = list(list(role = "user", content = "What is R?")),
 max_tokens = 100
)

# CLI: Agentic coding (requires Claude Code CLI)
claude_code("List the files in the current directory")
```

---

## API Interface

### Basic Usage

```r
# Claude 4.5 Sonnet (default)
response <- claudeR(
 prompt = list(list(role = "user", content = "Explain tibbles in R")),
 max_tokens = 200
)

# Use a different model
response <- claudeR(
 prompt = list(list(role = "user", content = "Explain tibbles in R")),
 model = "claude-opus-4-5-20251101",
 max_tokens = 200
)

# With system prompt
response <- claudeR(
 prompt = list(list(role = "user", content = "Review this code: x <- c(1,2,NA); mean(x)")),
 system_prompt = "You are an expert R programmer. Be concise.",
 max_tokens = 300
)
```

### Extended Thinking

For complex reasoning tasks, enable extended thinking:

```r
result <- claudeR(
 prompt = list(list(role = "user", content = "Solve: If f(x) = 2x^2 + 3x - 5, find x where f(x) = 20")),
 thinking = list(type = "enabled", budget_tokens = 2000),
 return_thinking = TRUE
)

cat("Thinking:\n", result$thinking, "\n\n")
cat("Answer:\n", result$response)
```

### Model Options

| Model | ID | Use Case |
|-------|-----|----------|
| Claude 4.5 Opus | `claude-opus-4-5-20251101` | Most capable, complex reasoning |
| Claude 4.5 Sonnet | `claude-sonnet-4-5-20250929` | Balanced (default) |
| Claude 4.5 Haiku | `claude-haiku-4-5-20251001` | Fast, cost-efficient |
| Claude 3.7 Sonnet | `claude-3-7-sonnet-20250219` | Extended thinking |
| Claude 2 | `claude-2` | Legacy support |

### API Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `prompt` | String (Claude 2) or list of messages (Claude 3+) | required |
| `model` | Model identifier | `claude-sonnet-4-5-20250929` |
| `max_tokens` | Maximum tokens to generate | `100` |
| `temperature` | Randomness (0-1). Must be 1 with thinking. | `0.7` |
| `system_prompt` | System instructions (Claude 3+) | `NULL` |
| `thinking` | Extended thinking config | `NULL` |
| `return_thinking` | Return thinking in output | `FALSE` |
| `stream_thinking` | Stream response in real-time | `TRUE` |

---

## CLI Interface (Claude Code)

The CLI interface provides agentic capabilities—Claude can read/write files, execute code, and perform multi-step tasks.

### Setup

```r
# Check if Claude Code is installed
claude_code_available(verbose = TRUE)

# View configuration
claude_code_config_print()

# Configure settings
claude_code_config(timeout = 600)
```

### Basic Usage

```r
# Simple prompt
claude_code("What files are in the current directory?")

# With JSON output
result <- claude_code("List 3 R packages for visualization", output_format = "json")

# Restrict available tools
claude_code("Analyze data.csv", allowed_tools = c("Read", "Bash"))
```

### Convenience Functions

```r
# Pipe data to Claude
mtcars |> claude_code_pipe("Describe this dataset and suggest visualizations")

# Analyze a file
claude_code_file("analysis.R", "Review this code for potential issues")

# Code review with focus
claude_code_review("script.R", focus = "performance")

# Generate code
claude_code_generate("function to calculate rolling mean", language = "R")

# Analyze a data frame
claude_code_analyze(iris, "What patterns exist in this data?")
```

### Batch Processing

```r
prompts <- c(
 "Explain ggplot2",
 "Explain dplyr",
 "Explain tidyr"
)

results <- claude_code_batch(prompts, progress = TRUE)
```

### Session Management

```r
# List previous sessions
claude_code_sessions()

# Resume a session
claude_code_session("Continue our analysis", session_id = "last")
```

### Interactive Chat

```r
# Start an interactive chat (in R console)
claude_code_chat()
```

### Response Utilities

```r
response <- claude_code("Write an R function to parse JSON")

# Extract code blocks
code <- claude_code_extract(response, language = "r")

# Parse JSON from response
data <- claude_code_parse(response)

# Pretty print
claude_code_print(response)
```

### CLI Parameters

| Parameter | Description | Default |
|-----------|-------------|---------|
| `prompt` | The prompt to send | required |
| `output_format` | `"text"`, `"json"`, or `"stream-json"` | `"text"` |
| `model` | Model to use | config default |
| `max_turns` | Maximum agentic turns | `NULL` |
| `system_prompt` | Custom system prompt | `NULL` |
| `allowed_tools` | Tools Claude can use | all |
| `disallowed_tools` | Tools Claude cannot use | none |
| `permission_mode` | `"default"`, `"acceptEdits"`, `"bypassPermissions"` | `"default"` |
| `working_dir` | Working directory | `getwd()` |
| `timeout` | Timeout in seconds | `300` |

---

## When to Use API vs CLI

| Use Case | Recommended |
|----------|-------------|
| Simple text completion | API (`claudeR`) |
| Streaming responses | API (`claudeR`) |
| Extended thinking | API (`claudeR`) |
| File operations | CLI (`claude_code`) |
| Code execution | CLI (`claude_code`) |
| Multi-step tasks | CLI (`claude_code`) |
| Interactive coding | CLI (`claude_code_chat`) |

## License

MIT License
