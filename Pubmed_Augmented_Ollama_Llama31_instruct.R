pacman::p_load(
  ellmer,
  rentrez,
  tidyverse
)

search_pubmed <- function(query){
  search_query <- entrez_search(db = "pubmed", 
                                term = query, 
                                retmax = 5)
  ids <- search_query$ids
  metadata <- entrez_summary(db = "pubmed", id = ids)
  fetched <- rentrez::entrez_fetch(db = "pubmed", id = ids,
                        rettype = "xml",
                        retmode = "",
                        parsed = TRUE) %>% parse_pubmed_xml() 
  
  abstracts <- lapply(fetched, function(x) x$abstract)
  
  collate_abstracts <- function(abstract_list) {
    sapply(abstract_list, function(x) paste(x, collapse = " "))
  }
  
  top5 <- collate_abstracts(abstracts)[1:5] # Should return 5 but I might change this
  
  output <- mapply(
    function(id, abstract) paste0("PMID: ", id, ": ", abstract), ids[1:5], top5) %>% 
    paste(., collapse = " ")
  
  return(output)
}

#search_pubmed("Gold nanoparticles")

# Self-hosted instance of LLaMa3.1_Instruct
# obtained via ollama pull
model <- "llama3.1:8b-instruct-q8_0"

system_prompt <- "You are a researcher who studies antimicrobial nanoparticles.
  Your focus is on metal and metal oxide nanoparticles (MMO-NPs).
  You want to find the best nanoparticle properties (such as 
  size, shape, composition, and surface chemistry)
  to use against against specific microorganisms.
  You can use Pubmed to help guide your research. 
  If you use Pubmed, PLEASE REPORT THE PMIDs.
"

system_prompt <- gsub("\\n", "", x=system_prompt)

chat_llama <- chat_ollama(model=model, 
                          system_prompt=system_prompt)

chat_llama$register_tool(tool(
  search_pubmed,
  "Performs a Pubmed search on input, 'query', and returns top 5 abstracts",
  query = type_string(
    "The search query to submit to Pubmed. No default.",
    required = TRUE
  )
))

# Testing
chat_llama$chat("What type of nanoparticle should I use against MRSA?")

chat_llama$chat("What type of nanoparticle should I use against Acinetobacter baumannii?")
