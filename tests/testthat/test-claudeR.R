test_that("claudeR requires API key", {
  # Save current API key if it exists

  old_key <- Sys.getenv("ANTHROPIC_API_KEY")

  # Unset the environment variable

  Sys.unsetenv("ANTHROPIC_API_KEY")

  expect_error(
    claudeR(
      prompt = list(list(role = "user", content = "Hello")),
      api_key = NULL
    ),
    "Please provide an API key"
  )



  # Restore the API key
  if (old_key != "") {
    Sys.setenv(ANTHROPIC_API_KEY = old_key)
  }
})

test_that("claudeR validates prompt format for Claude 3/4 models", {
  # Claude 3/4 models require list format prompts
  expect_error(
    claudeR(
      prompt = "This is a string, not a list",
      model = "claude-sonnet-4-5-20250929",
      api_key = "fake-api-key-for-testing"
    ),
    "Claude-3 and newer models require the input in a list format"
  )
})

test_that("claudeR accepts valid list format prompt for Claude 3/4", {
  skip_if(
    Sys.getenv("ANTHROPIC_API_KEY") == "",
    "ANTHROPIC_API_KEY not set - skipping live API test"
  )

  # This test only runs when an API key is available
  # It validates that the function accepts proper list format
  response <- claudeR(
    prompt = list(list(role = "user", content = "Say 'test' and nothing else")),
    model = "claude-sonnet-4-5-20250929",
    max_tokens = 10,
    stream_thinking = FALSE
  )

  expect_type(response, "character")
  expect_true(nchar(response) > 0)
})

test_that("claudeR allows string prompt for Claude 2 models", {
  # Claude 2 models should accept string prompts without error on format validation
  # The API call will fail with a fake key, but we verify the prompt format check passes
  result <- tryCatch(
    claudeR(
      prompt = "This is a simple string prompt",
      model = "claude-2.1",
      api_key = "fake-api-key-for-testing"
    ),
    error = function(e) e$message
  )

 # Should NOT contain the prompt format error message
  expect_false(
    grepl("require the input in a list format", result, fixed = TRUE)
  )
})
