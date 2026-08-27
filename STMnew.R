library(stm)
processed <- textProcessor(AIRESTM$Abstract, metadata = AIRESTM)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta
set.seed(50)
stm.search <- searchK(documents = out$documents,
                      vocab = out$vocab,
                      K = 10:30,
                      init.type = "Spectral")
plot(stm.search)
processed <- prepDocuments(docs, vocab, meta)  # Replace with your data

k_seq <- c(5, 10, 15, 20, 25, 30)
models <- lapply(k_seq, function(k) {
  stm(documents = processed$documents,
      vocab = processed$vocab,
      K = k,
      data = processed$meta,
      max.em.its = 75,
      init.type = "Spectral")
})


