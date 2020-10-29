Pattern2Regex <- function(pattern) {
  prefix <- grep("\\*$", pattern)
  if (length(prefix)) {
    no.prefix <- paste(pattern[-prefix], collapse="|")
    prefix <- paste(sub("\\*$", "", pattern[prefix]), collapse="|")
    if (nchar(prefix) && nchar(no.prefix)) sprintf("((^| )(%s))|((^| )%s($| ))", prefix, no.prefix)
    else if (nchar(prefix)) sprintf("(^| )(%s)", prefix)
  } else {
    sprintf("(^| )(%s)($| )", paste(pattern, collapse="|"))
  }
}

SentiStrengthLexicon <- function() {
  filename <- system.file("lexicons", "sentistrength.txt",
                          package="FinnishSentiment", mustWork=TRUE)
  lexicon <- fread(filename, col.names=c("pattern", "value"))
  lexicon <- unique(lexicon)[, {
    if (.N == 1) .SD
    else if (all(value > 0)) list(value=max(value))
    else if (all(value < 0)) list(value=min(value))
  }, by=pattern]
  lexicon.re.full <- lexicon[, list(regex=Pattern2Regex(pattern)), by=value]
  lexicon.re.full <- lexicon.re.full[order(value)]

  lexicon.re <- lexicon[abs(value) > 1, list(value=value / abs(value), pattern)]
  lexicon.re <- lexicon.re[, list(regex=Pattern2Regex(pattern)), by=value]
  lexicon.re <- lexicon.re[order(value)]

  list(lexicon.ss=lexicon, lexicon.ss.re=lexicon.re, lexicon.ss.re.full=lexicon.re.full)
}

AFINNLexicon <- function() {
  filename <- system.file("lexicons", "afinn.json",
                          package="FinnishSentiment", mustWork=TRUE)
  lexicon <- unlist(jsonlite::read_json(filename))
  lexicon <- data.table(pattern=names(lexicon), value=lexicon)[value != 0]
  lexicon.re.full <- lexicon[, list(regex=Pattern2Regex(pattern)), by=value]
  lexicon.re.full <- lexicon.re.full[order(value)]

  lexicon.re <- lexicon[, list(value=value / abs(value), pattern)]
  lexicon.re <- lexicon.re[, list(regex=Pattern2Regex(pattern)), by=value]
  lexicon.re <- lexicon.re[order(value)]

  list(lexicon.afinn=lexicon, lexicon.afinn.re=lexicon.re,
       lexicon.afinn.re.full=lexicon.re.full)
}

#' @export
Lexicons <- function() {
  c(SentiStrengthLexicon(), AFINNLexicon())
}

#' @export
MatchLexicon <- function(tokens, lexicon, lexicon.name=NULL, ids=NULL) {
  tokens <- sapply(tokens, paste, collapse=" ")
  Count <- function(regex) stringr::str_count(tokens, regex)
  res <- sapply(lexicon$regex, Count)
  colnames(res) <- lexicon$value
  if (!is.null(lexicon.name)) {
    colnames(res) <- paste(lexicon.name, colnames(res), sep="_")
  }
  if (!is.null(ids)) {
    rownames(res) <- as.character(ids)
  }
  res
}
