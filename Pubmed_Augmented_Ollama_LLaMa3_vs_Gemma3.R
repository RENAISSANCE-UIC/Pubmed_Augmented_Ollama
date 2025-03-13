pacman::p_load(
  ellmer,
  rentrez,
  tidyverse
)

withr::local_options(ellmer_timeout_s = 300) 

search_pubmed <- function(query){
  search_query <- entrez_search(db = "pubmed", 
                                term = query, 
                                retmax = 20)
  ids <- search_query$ids
  metadata <- entrez_summary(db = "pubmed", id = ids)
  fetched <- rentrez::entrez_fetch(db = "pubmed", id = ids,
                        rettype = "xml",
                        retmode = "",
                        parsed = TRUE) %>% parse_pubmed_xml() 
  if(length(ids) == 1) {
    abstracts <- fetched$abstract
  } else {
    abstracts <- lapply(fetched, function(x) x$abstract)
  }
  
  collate_abstracts <- function(abstract_list) {
    sapply(abstract_list, function(x) paste(x, collapse = " "))
  }
  
  top5 <- collate_abstracts(abstracts)[1:5] # Should return 5 but I might change this
  
  output <- mapply(
    function(id, abstract) paste0("PMID: ", id, ": ", abstract), ids[1:5], top5) %>% 
    paste(., collapse = " ")
  
  return(output)
}

# Self-hosted instances of LLaMa3 and Gemma3
# obtained via ollama pull

summarize_pubmed <- function(query){ 

  # Use llama3.2 as a summarizer
  pmb_model <- "llama3.2"
  
  pmb_system_prompt <- "You are a medical research assistant that searches Pubmed and
    reports a summary of your findings. Use the provided function. 
    1. Extract all PMID occurrences and report these as PMID: <<insert PMID here>>
    2. Extract the summaries and report these as PMID: <<summary>>
  "
  
  pmb_system_prompt  <- gsub("\\n", "", x= pmb_system_prompt )
  
  pubmed_bot <- chat_ollama(model=pmb_model, 
                            system_prompt= pmb_system_prompt )
  
  pubmed_bot$register_tool(tool(
    search_pubmed,
    "Performs a Pubmed search on input, 'query', and returns top 5 abstracts",
    query = type_string(
      "The search query to submit to Pubmed. No default.",
      required = TRUE
    )
  ))
  
  summary <- pubmed_bot$chat(query)
  
  return(summary)

}  

# Try a few models

system_prompt <- "You are a researcher who studies antimicrobial nanoparticles.
  Your focus is on metal and metal oxide nanoparticles (MMO-NPs).
  You want to find the best MMO-NP properties (such as 
  size, shape, composition, and surface chemistry)
  to use against against specific microorganisms.
  NO HALLUCINATIONS.
  Please report: 
    ** PMID: <<Insert PMID here>>
    ** Nanoparticle Size: <<Insert nanoparticle size here>>
    ** Nanoparticle Shape: <<Insert nanoparticle shape here>>
    ** Nanoparticle Composition: <<Insert nanoparticle composition here>>
    ** Nanoparticle Concentration: <<Insert [nanoparticle] here>> 
  Use the provided Pubmed Summary to help 
  guide your research and REPORT PMIDS. 
"
system_prompt <- gsub("\\n", "", x=system_prompt)

query <- "What type of nanoparticle should I use against MRSA?"
result <- search_pubmed(query)

# Baseline function (llama3.2 is small and quick)
summarize_pubmed(query)

# Llama3.1 model (faster than 3.3, can call tools)
model1 <- "llama3.1"

chat_llama31 <- chat_ollama(model=model1, 
                          system_prompt=system_prompt)

chat_llama31$register_tool(tool(
  search_pubmed,
  "Performs a Pubmed search on input, 'query', and returns top 5 abstracts",
  query = type_string(
    "The search query to submit to Pubmed. No default.",
    required = TRUE
  )
)) # llama3 accepts tool use

chat_llama31$chat(query)


# Gemma3 has extended 128k-token context window 

model2 <- "gemma3:27b"
chat_gemma <- chat_ollama(model=model2, 
                          system_prompt=system_prompt)

RAGmented <- 
  paste("Please answer the following question with the provided information:",
       query, result, collapse=" ") # gemma3 does NOT accept tool use

chat_gemma$chat(RAGmented)


# Llama3.3 is the largest model, takes the longest to provide output

model3 <- "llama3.3"

chat_llama33 <- chat_ollama(model=model3, 
                            system_prompt=system_prompt)

chat_llama33$chat(RAGmented)
