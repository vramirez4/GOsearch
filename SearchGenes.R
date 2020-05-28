## Updated 7/16/2019


##CHECK THAT YOU HAVE THE NECESSARY PACKAGES
required_packages = c("httr", "jsonlite", "xml2", "data.table")
for (i in required_packages) {
  if (i %in% rownames(installed.packages()) == FALSE) {
    install.packages(i)
  }
  else {
    NULL
  }
}

library(httr)
library(jsonlite)
library(xml2)
library(data.table)


##QUERY GENE ONTOLOGY DATABASE
## x specifies some vector of keywords to search i.e c("taste","perception")
## y specifies the maximum amount of Gene Ontology ID's to return
GETGO = function(x, y) {
  key_words = x
  num_entries = y
  search_terms = paste(key_words, collapse = "&")
  
  
  requestURL <-
    sprintf(
      "https://www.ebi.ac.uk/QuickGO/services/ontology/go/search?query=%s&limit=%s&page=1",
      search_terms,
      num_entries
    )
  r <- GET(requestURL, accept("application/json"))
  
  json <- toJSON(content(r))
  dat <- fromJSON(json)
  GOIDS = dat$results[dat$results$isObsolete == "FALSE", ]
  return(GOIDS)
}

##QUERY THE BIOMART USING OUR GO IDs
##INPUT IS THE OUTPUT FROM GETGO()
GetGenes = function(dat = "Output from GETGO()", annotation = "choose between 'GRCH37' or 'GRCH38' Default is GRCH37") {
  GOS = paste(dat$id)
  
  QUERY <- paste(GOS, collapse = ",")
  if (annotation == "GRCH38") {
    requestURL2 = paste(
      'http://uswest.ensembl.org/biomart/martservice?query=<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE Query><Query  virtualSchemaName = "default" formatter = "TSV" header = "0" uniqueRows = "0" count = "" datasetConfigVersion = "0.6" ><Dataset name = "hsapiens_gene_ensembl" interface = "default" ><Filter name = "go_parent_term" value = "',
      QUERY,
      '" /><Filter name = "with_ucsc" excluded = "0"/><Attribute name = "external_gene_name" /><Attribute name = "chromosome_name" /><Attribute name = "start_position" /><Attribute name = "end_position" /><Attribute name = "ensembl_gene_id" /><Attribute name = "ensembl_transcript_id" /><Attribute name = "transcript_start" /><Attribute name = "transcript_end" /><Attribute name = "transcript_length" /><Attribute name = "transcript_biotype" /><Attribute name = "strand" /><Attribute name = "description" /><Attribute name = "go_id" /><Attribute name = "definition_1006" /></Dataset></Query>',
      sep = "",
      collapse = ""
    )
  }
  else {
    requestURL2 = paste(
      'http://grch37.ensembl.org/biomart/martservice?query=<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE Query><Query  virtualSchemaName = "default" formatter = "TSV" header = "0" uniqueRows = "0" count = "" datasetConfigVersion = "0.6" ><Dataset name = "hsapiens_gene_ensembl" interface = "default" ><Filter name = "go_parent_term" value = "',
      QUERY,
      '" /><Filter name = "with_ucsc" excluded = "0"/><Attribute name = "external_gene_name" /><Attribute name = "chromosome_name" /><Attribute name = "start_position" /><Attribute name = "end_position" /><Attribute name = "ensembl_gene_id" /><Attribute name = "ensembl_transcript_id" /><Attribute name = "transcript_start" /><Attribute name = "transcript_end" /><Attribute name = "transcript_length" /><Attribute name = "transcript_biotype" /><Attribute name = "strand" /><Attribute name = "description" /><Attribute name = "go_id" /><Attribute name = "definition_1006" /></Dataset></Query>',
      sep = "",
      collapse = ""
    )
  }
  biomart <- GET(requestURL2, accept = "text/xml")
  stop_for_status(biomart)
  output = content(biomart, as = "text")
  resolve = paste(output, sep = "\t")
  martlist = data.table::fread(resolve, sep = "\t")
  colnames(martlist) = c(
    "gene_name",
    "chromosome",
    "start_position",
    "end_position",
    "ensembl_gene_id",
    "ensembl_transcript_id",
    "transcript_start",
    "transcript_end",
    "transcript_length",
    "transcript_biotype",
    "strand",
    "description",
    "go_id",
    "definition"
  )
  
  
  return(martlist)
}



##SAMPLE RUN
##dat = GETGO("taste", 25)
##GeneList = GetGenes(dat, "GRCH37")
