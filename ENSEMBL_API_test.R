# Load the required libraries
library(httr)
library(jsonlite)

get_gene_function_summary <- function(ensembl_id) {
  # Define the base URL of the Ensembl REST API
  base_url <- "https://rest.ensembl.org"
  
  # Define the endpoint for gene lookup
  endpoint <- paste0("/lookup/id/", ensembl_id)
  
  # Create the full URL for the API request
  url <- paste0(base_url, endpoint)
  
  tryCatch({
    # Send a GET request to the Ensembl API
    response <- GET(url, add_headers("Content-Type" = "application/json"))
    
    # Check if the request was successful
    if (http_type(response) == "application/json") {
      # Parse the JSON response
      data <- content(response, "parsed")
      
      # Extract the gene description
      gene_description <- data$description
      
      return(gene_description)
    } else {
      cat("Error: Unable to retrieve gene function summary for", ensembl_id, ". Status code:", http_status(response), "\n")
    }
  }, error = function(e) {
    cat("Error: Request to Ensembl API failed. ", e$message, "\n")
  })
}

# Replace this with your ENSEMBL ID
ensembl_id <- "ENSG00000157764"

gene_summary <- get_gene_function_summary(ensembl_id)
cat("Gene Function Summary for", ensembl_id, ":", gene_summary, "\n")
