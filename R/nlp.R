RemoveURL <- function(text) gsub("https?://[[:graph:]]*", "<url>", text)

CleanText <- function(text) {
  text <- RemoveURL(text)
  text <- stringr::str_replace_all(text,"[^[:graph:]\n\r]", " ")
  gsub("(^ +)|( +$)", "", text)
}

ReplaceSpaces <- function(text) {
  gsub("[[:space:]]+", " ", text)
}

RemoveHashtags <- function(text) {
  gsub("#", " ", text)
}

TokenizeWithEmojis <- function(text) {
  emojis <- EmoticonFindeR::FindUnicodeEmojis(text)
  tokens <- text2vec::word_tokenizer(text)
  if (nrow(emojis)) {
    emojis <- emojis[, list(from=list(sort(c(1, start, start + 1))),
                            end=list(sort(c(start - 1, start, nchar(text))))),
                     by=list(id, text)]
    emojis[, text.split := stringi::stri_sub_all(text, from, end)]
    emojis[, tokens := lapply(text.split, function(text) {
      unlist(lapply(text, function(t) {
        if (t %in% EmoticonFindeR::emojis$char) {
          t
        } else if (t != "") {
          text2vec::word_tokenizer(t)
        }
      }))
    })]
    tokens[emojis$id] <- emojis$tokens
  }
  tokens
}

VoikkoStemTokens <- function(tokens, voikko=NULL) {
  if (is.null(voikko)) {
    libvoikko <- reticulate::import("libvoikko")
    voikko <- libvoikko$Voikko("fi-x-standard")
  }
  lapply(tokens, voikko$analyze)
}

#' @export
Preprocess <- function(text) {
  text %>% CleanText %>% RemoveHashtags %>% ReplaceSpaces %>% tolower
}

#' @export
ProcessFinnish <- function(text, ...) {
  UseMethod("ProcessFinnish", text)
}

#' @export
ProcessFinnish.character <- function(text, ids=NULL) {
  if (is.null(ids)) {
    ids <- 1:length(text)
  }
  text <- data.table(text=text, id=ids)
  ProcessFinnish(text)
}

#' @export
ProcessFinnish.data.table <- function(text) {
  text[, text.clean := Preprocess(text)]
  text[, tokens := TokenizeWithEmojis(text.clean)]
  voikko <- reticulate::import("libvoikko")$Voikko("fi-x-standard")
  text[, voikko := lapply(tokens, VoikkoStemTokens, voikko)]
  if (is.character(text$tokens)) {
    text[, tokens := list(list(tokens))]
    text[, voikko := list(list(voikko))]
  }
  text[, tokens.stemmed := mapply(function(tokens, voikko) {
    if (length(voikko)) {
      mapply(function(t, v) {
        tolower(if (length(v) == 0) t else v[[1]]$BASEFORM)
      }, tokens, voikko)
    } else tokens
  }, tokens, voikko, SIMPLIFY=FALSE)]
  text
}
