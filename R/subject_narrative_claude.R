# Claude narrator for subject_narrative (optional, uses the ellmer package)
#
# The one place the narrative touches a language model. It is handed the derived
# facts text only, never raw records, and is told to summarise without adding
# anything. ellmer is a Suggests dependency; without it (or without an API key)
# this stops with a clear message and the deterministic facts are unaffected.

#' @noRd
subject_narrative_system_prompt <- function() {
  paste0(
    "You summarise one subject in a clinical trial for a reviewer, using only ",
    "the facts you are given.\n\n",
    "Rules:\n",
    "- Write 3 to 5 short sentences of plain English, no bullet points.\n",
    "- Use only the numbers, events, dates and categories in the facts. Never ",
    "add, infer or estimate a value that is not stated.\n",
    "- Do not diagnose, do not judge causality, and do not give medical advice ",
    "or recommendations. Describe only what the data records.\n",
    "- If something is not in the facts, do not mention it and do not guess.\n",
    "- Refer to the subject by their identifier, not as a patient by name."
  )
}

#' Narrate one subject's facts with Claude via ellmer
#'
#' Default narrator for [subject_narrative()]. Sends the derived facts text (not
#' the raw data) to Claude and returns a short plain-language summary. Requires
#' the `ellmer` package and an `ANTHROPIC_API_KEY`.
#'
#' @param facts_text The facts block from [subject_narrative_facts_text()].
#' @param facts The structured facts list (unused here; accepted so narrators
#'   share one signature).
#' @param model Claude model id (default `"claude-opus-5-5"`).
#' @param chat An optional pre-built ellmer chat object (mainly for testing).
#' @param ... Ignored.
#' @return A single character string: the narrative.
#' @export
subject_narrative_claude <- function(facts_text, facts = NULL,
                                     model = "claude-opus-5-5", chat = NULL, ...) {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required for subject_narrative_claude(). ",
         "Install it, or pass your own `narrate` function to subject_narrative().")
  }
  if (is.null(chat)) {
    chat <- ellmer::chat_anthropic(
      system_prompt = subject_narrative_system_prompt(), model = model)
  }
  as.character(chat$chat(facts_text))
}
