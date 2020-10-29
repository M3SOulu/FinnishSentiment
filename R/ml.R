#' @export
Features <- function(text, vocab, lexicons) {
  logging::loginfo("Computing DTM for %d text documents", nrow(text))
  dtm <- TextFeatures::MakeDTM(text$tokens.stemmed, text$id, vocab)
  logging::loginfo("Matching SS Lexicon for %d text documents", nrow(text))
  ss.lexicon <- MatchLexicon(text$tokens, lexicons$lexicon.ss.re,
                             "SSLexicon", text$id)
  logging::loginfo("Matching AFINN Lexicon for %d text documents", nrow(text))
  afinn.lexicon <- MatchLexicon(text$tokens.stemmed, lexicons$lexicon.afinn.re,
                                "AFINN", text$id)
  cbind(dtm, ss.lexicon, afinn.lexicon)
}

#' @export
Model <- function(text, ...) {
  UseMethod("Model", text)
}

#' @export
Model.character <- function(text, polarity, lexicons, ...) {
  text <- ProcessFinnish(text)
  text$polarity <- polarity
  Model(text, lexicons, ...)
}

#' @export
Model.list <- function(text, lexicons, ...) {
  vocab <- TextFeatures::MakeVocabulary(text$tokens.stemmed)
  features <- Features(text, vocab, lexicons)
  list(vocab=vocab, lexicons=lexicons,
       model=GlmnetCaret(features, text$polarity, ...))
}

#' @export
Model.data.frame <- Model.list

#' @export
GlmnetCaret <- function(data, response, repeats=1, allow.parallel=TRUE, seed=42) {
  control <- trainControl(method="repeatedcv",
                          number=10,
                          repeats=repeats,
                          returnResamp="all",
                          returnData=FALSE,
                          savePredictions="final",
                          classProbs=TRUE,
                          allowParallel=allow.parallel,
                          verboseIter=TRUE,
                          summaryFunction=multiClassSummary)
  grid <- expand.grid(alpha=1, lambda=seq(0.001, 0.1, by=0.001))
  if (!is.null(seed)) {
    set.seed(seed)
  }
  train(data, response, method="glmnet", trControl=control,
        weights=as.vector(length(response) / table(response)[response]),
        metric="AUC", tuneGrid=grid)
}

#' @export
FinnishSentiment <- function(text, ...) {
  UseMethod("FinnishSentiment", text)
}

#' @export
FinnishSentiment.character <- function(text, model, ids=NULL) {
  ProcessFinnish(text) %>% FinnishSentiment(model)
}

#' @export
FinnishSentiment.list <- function(text, model) {
  Features(text, model$vocab, model$lexicons) %>% FinnishSentiment(model)
}

#' @export
FinnishSentiment.data.frame <- FinnishSentiment.list

#' @export
FinnishSentiment.Matrix <- function(text, model) {
  predict(model$model, text)
}
