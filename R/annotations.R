#' @export
Vote1 <- function(polarity, annotator) {
  votes <- table(polarity)
  best <- which.max(votes)
  if (length(best) == 1) {
    which(polarity == names(best))[1]
  }
}

#' @export
Vote2 <- function(polarity, annotator) {
  votes <- table(polarity)
  best <- which.max(votes)
  if (length(best) == 1) {
    which(polarity == names(best))[1]
  } else if ("positive" %in% names(best) && !"negative" %in% names(best)) {
    which(polarity == "positive")[1]
  } else if ("negative" %in% names(best) && !"positive" %in% names(best)) {
    which(polarity == "negative")[1]
  }
}

#' @export
Vote3 <- function(polarity, annotator) {
  votes <- table(polarity)
  best <- which.max(votes)
  if (length(best) == 1) {
    which(polarity == names(best))[1]
  } else if ("neutral" %in% names(best)) {
    which(polarity == "neutral")[1]
  }
}

#' @export
FavorNeutral <- function(polarity, annotator) {
  if (length(unique(polarity)) == 1) {
    1
  } else if (any(polarity == "neutral")) {
    which(polarity == "neutral")[1]
  }
}

#' @export
FavorPolarity <- function(polarity, annotator) {
  if (length(unique(polarity)) == 1) {
    1
  } else if (any(polarity == "positive") & all(!polarity == "negative")) {
    which(polarity == "positive")[1]
  } else if (any(polarity == "negative") & all(!polarity == "positive")) {
    which(polarity == "negative")[1]
  } else if (any(polarity == "neutral")) {
    which(polarity == "neutral")[1]
  }
}

#' @export
PickAnnotation <- function(annotations, PickFUNC,
                           vars=c("annotator1", "annotator2", "annotator3")) {
  data <- melt(annotations[lang == "fi"],
               measure.vars=vars,
               variable.name="annotator", value.name="polarity")
  data <- data[!is.na(polarity)]
  data <- data[, .SD[PickFUNC(polarity, annotator)], by=status_id]
  data[, polarity2 := factor(polarity, levels=c("negative", "positive"))]
  data[, polarity := factor(polarity, levels=c("negative", "positive", "neutral"))]
  data
}
