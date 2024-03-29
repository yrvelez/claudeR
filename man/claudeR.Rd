% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{claudeR}
\alias{claudeR}
\title{Interact with the Anthropics Claude API}
\usage{
claudeR(
  prompt,
  model = "claude-2",
  max_tokens,
  stop_sequences = "\\n\\nHuman: ",
  temperature = 0.7,
  top_k = -1,
  top_p = -1,
  api_key = NULL
)
}
\arguments{
\item{prompt}{The prompt you want Claude to complete.}

\item{model}{The model to use for the request.}

\item{max_tokens}{A maximum number of tokens to generate before stopping.}

\item{stop_sequences}{(Optional) A list of strings upon which to stop generating.}

\item{temperature}{(Optional) Amount of randomness injected into the response.}

\item{top_k}{(Optional) Only sample from the top K options for each subsequent token.}

\item{top_p}{(Optional) Does nucleus sampling.}

\item{api_key}{Your API key for authentication.}
}
\value{
The resulting completion up to and excluding the stop sequences.
}
\description{
Interact with the Anthropics Claude API
}
