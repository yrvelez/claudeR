# claudeR 0.2.0

* Added current Claude model shortcuts: `fable` (Claude Fable 5), `opus`
  (Claude Opus 4.8), `sonnet` (Claude Sonnet 5), and `haiku` (Claude Haiku 4.5).
  Claude Sonnet 5 is the new balanced default; arbitrary model IDs continue to
  pass through unchanged.
* Added `claude_models()` for live model and capability discovery through the
  Anthropic Models API.
* Added adaptive thinking and `output_config` effort support for current models.
* Added structured responses with model, usage, content blocks, stop reasons,
  and refusal details via `return_response = TRUE`.
* API and streaming failures now signal R errors instead of printing an error
  and returning `NULL`; refusal stop reasons remain inspectable as successful
  structured responses.
* Added beta-header support and a forward-compatible `...` request-body escape
  hatch for tools, fallbacks, structured output, and future Messages API fields.
* Fixed ignored `stop_sequences`, invalid default sampling parameters, message
  content-block loss, simplified JSON parsing, SSE parsing across CRLF chunk
  boundaries, and hidden streaming HTTP errors.
* Character prompts are now automatically converted to Messages API user turns.
  The retired Claude 2 Text Completions path now produces a migration error.
* Streaming is now opt-in. The `stream_thinking` name remains available for
  backward compatibility.
* Regenerated and repaired Claude Code documentation, completed missing imports,
  and moved internal CLI configuration out of the user's global environment.

# claudeR 0.1.0

## Initial CRAN Release

* First public release of claudeR package
* Support for Claude 2, Claude 3, and Claude 4 model families
* Extended thinking mode for step-by-step reasoning
* Real-time streaming of responses
* System prompt support for Claude 3+ models
* Comprehensive documentation and vignette
