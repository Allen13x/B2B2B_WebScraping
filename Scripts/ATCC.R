# Load necessary libraries
library(RSelenium)
library(httr)
library(rvest)
library(tidyverse)

# Pull and run the Selenium Docker container
shell('docker pull selenium/standalone-chrome:4.2.2')
shell('docker run -d -p 4445:4444 --shm-size 4g selenium/standalone-chrome:4.2.2')

# Initialize the remote driver
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

# Define the base URL and search parameters
base_url <- "your_base_url_here"
search_query <- "your_search_query_here"
results_per_page <- 20

# Construct the URL for the first page
url <- paste0(base_url, "#q=", URLencode(search_query), "&sort=relevancy&numberOfResults=", results_per_page)
remDr$navigate(url)
Sys.sleep(5)

# Find elements on the page
elements <- remDr$findElements(using = "css", value = ".product-search-listing-card__content")
elements2 <- remDr$findElements(using = 'css', value = '.product-search-listing-card__genomic-sequence')
maxIndex <- remDr$findElements(using = 'css', value = '.coveo-highlight')[[3]]$getElementText()
elements3 <- remDr$findElements('css', 'h3.product-search-listing-card__name')

# Initialize the result list
result <- list()

# Extract data from the first page
for (i in 1:length(elements)) {
	text_content <- str_split(elements[[i]]$findElement(using = "css", value = "p")$getElementText()[[1]], '\n')[[1]][c(1,2)]
	download_link <- elements2[[i]]$findElement(using = "css", value = "a.product-search-listing-card__genomic-sequence")$getElementAttribute("href")[[1]]
	char_link <- elements3[[i]]$findChildElements('css', 'a')[[1]]$getElementAttribute('href')[[1]]
	line <- c(text_content, download_link, char_link)
	names(line) <- c('Name', 'Code', 'Link', 'Characteristic_link')
	result[[i]] <- line
}

# Iterate through the following pages
while (TRUE) {
	n <- length(result)
	if (n == maxIndex) break
	
	url1 <- paste0(base_url, "#q=", URLencode(search_query), "&first=", n, "&sort=relevancy&numberOfResults=", results_per_page)
	remDr$navigate(url1)
	Sys.sleep(1)
	
	elements <- remDr$findElements(using = "css", value = ".product-search-listing-card__content")
	elements2 <- remDr$findElements(using = 'css', value = '.product-search-listing-card__genomic-sequence')
	elements3 <- remDr$findElements('css', 'h3.product-search-listing-card__name')
	
	if (length(elements) == 0) break
	
	for (i in 1:length(elements)) {
		Sys.sleep(1)
		text_content <- str_split(elements[[i]]$findElement(using = "css", value = "p")$getElementText()[[1]], '\n')[[1]][c(1,2)]
		download_link <- elements2[[i]]$findElement(using = "css", value = "a.product-search-listing-card__genomic-sequence")$getElementAttribute("href")[[1]]
		char_link <- elements3[[i]]$findChildElements('css', 'a')[[1]]$getElementAttribute('href')[[1]]
		line <- c(text_content, download_link, char_link)
		names(line) <- c('Name', 'Code', 'Link', 'Characteristic_link')
		count <- n + i
		result[[count]] <- line
		if (count == maxIndex) break
	}
}

# Combine results into a data frame and clean up the 'Code' column
result_df <- bind_rows(result) %>%
	mutate(Code = str_remove(Code, ' BSL.*'))



##### Genome Data ####

# Loop through each row in the result data frame
for (j in 1:nrow(result)) {
	url1 <- result$Link[j]
	remDr$navigate(url1)
	Sys.sleep(1)
	
	# Extract metadata from the page
	meta <- remDr$findElements(using = "css", value = ".metadata-page")
	s <- str_split(meta[[1]]$findElement(using = 'css', value = "td")$getElementText()[[1]], '\n')[[1]]
	
	# Extract and clean specific metadata fields
	metadata[[j]] <- c(
		'Date_Pub' = str_remove(s[grep('Published', s)], 'Date Published '),
		'Length' = str_remove_all(s[grep('Length ', s)], '[Length | nt|\\,]'),
		'Seq_tech' = s[grep('Sequencing Technology', s) + 1],
		'Contigs_number' = s[grep('Number of Contigs', s) + 1],
		'Level' = s[grep('Assembly Level', s) + 1],
		'N50' = str_remove_all(s[grep('N50', s)], 'N50 | nt|\\,'),
		'%GC' = str_remove_all(s[grep('%GC', s)], '%GC '),
		'Genotype' = str_remove_all(s[grep('Genotype', s)], 'Genotype| '),
		'Isolation' = str_remove_all(s[grep('Isolation', s)], 'Isolation '),
		'Tags' = s[grep('Tags', s) + 1],
		'BSL' = str_remove_all(s[grep('Biosafety Level', s)], '[A-z ]'),
		'Type_Strain' = str_remove_all(s[grep('Type Strain', s)], 'Type Strain '),
		'Antigenic_prop' = str_remove_all(s[grep('Antigenic Properties', s)], 'Antigenic Properties'),
		'CDS' = str_remove_all(s[grep('number of CDS', s)], 'Number of CDS |\\,'),
		'Hyp_prot' = str_remove_all(s[grep('Number of Hypothetical Proteins', s)], 'Number of Hypothetical Proteins |\\,'),
		'tRNA' = str_remove_all(s[grep('Number of tRNA', s)], 'Number of tRNA |\\,'),
		'rRNA_5s' = str_remove_all(s[grep('Number of 5s rRNA', s)], 'Number of 5s rRNA |\\,'),
		'rRNA_16s' = str_remove_all(s[grep('Number of 16s rRNA', s)], 'Number of 16s rRNA |\\,'),
		'rRNA_23s' = str_remove_all(s[grep('Number of 23s rRNA', s)], 'Number of 23s rRNA |\\,'),
		'ONT_reads' = str_remove_all(s[grep('Oxford Nanopore Read Count', s)], 'Oxford Nanopore Read Count |\\,'),
		'ONT_Qscore' = str_remove_all(s[grep('Oxford Nanopore Median Q Score', s) + 1], '\\,'),
		'ILL_reads' = str_remove_all(s[grep('Illumina Read Count ', s)], 'Illumina Read Count |\\,'),
		'Ill_Qscore' = str_remove_all(s[grep('Illumina Median Q Score', s)], 'Illumina Median Q Score |\\,'),
		'Ill_MeanCov' = str_remove_all(s[grep('Illumina Mean Coverage Depth ', s)], 'Illumina Mean Coverage Depth |\\,|×'),
		'Code' = result$Code[j]
	)
	
	print(j)
}

# Remove duplicate rows from the result data frame
result_sliced <- result %>% distinct()

# Join the result data frame with the metadata
result_1 <- left_join(result_sliced, bind_rows(metadata) %>% distinct())

#### Pheno Data ####

##### Extract MetaDATA ######

char <- list()
for (i in 1:nrow(result_1)) {
	print(i)
	
	url <- result_1$Characteristic_link[i]
	
	# Read the HTML content of the webpage
	page <- read_html(url)
	Sys.sleep(1)
	
	# Extract Characteristics
	dd <- page %>% 
		html_node("h3:contains('Characteristics')") %>% 
		html_nodes(xpath = "../..") %>% 
		html_nodes("dd") %>% 
		html_text() %>% 
		str_replace_all('\n', ';') %>% 
		str_remove_all(pattern = '\r') %>% 
		str_remove_all('^;\\s+|;\\s+$|(?<=;)\\s+') %>% 
		str_remove_all('^;|;$|;(?=;)')
	
	names(dd) <- page %>% 
		html_node("h3:contains('Characteristics')") %>% 
		html_nodes(xpath = "../..") %>% 
		html_nodes("dt") %>% 
		html_text()
	
	# Extract History
	hd <- page %>% 
		html_node("h3:contains('History')") %>% 
		html_nodes(xpath = "../..") %>% 
		html_nodes("dd") %>% 
		html_text() %>% 
		str_replace_all('\n', ';') %>% 
		str_remove_all(pattern = '\r') %>% 
		str_remove_all('^;\\s+|;\\s+$|(?<=;)\\s+')
	
	names(hd) <- page %>% 
		html_node("h3:contains('History')") %>% 
		html_nodes(xpath = "../..") %>% 
		html_nodes("dt") %>% 
		html_text()
	
	# Extract general data
	gd <- page %>% 
		html_node("dt:contains('Product category')") %>% 
		html_nodes(xpath = '../..') %>% 
		html_nodes('dd') %>% 
		html_text() %>% 
		str_replace_all('\n', ';') %>% 
		str_remove_all(pattern = '\r') %>% 
		str_remove_all('^;\\s+|;\\s+$|(?<=;)\\s+|\\s+$|^;')
	
	names(gd) <- page %>% 
		html_node("dt:contains('Product category')") %>% 
		html_nodes(xpath = '../..') %>% 
		html_nodes('dt') %>% 
		html_text()
	
	char[[i]] <- c(gd, dd, hd, c('Code' = result_1$Code[i]))
}

# Split drugs according to susceptibility testing
char_df <- bind_rows(char) %>% 
	mutate(`Susceptibility profile` = strsplit(`Susceptibility profile`, ';')) %>% 
	mutate(
		R = sapply(`Susceptibility profile`, function(x) {
			res_index <- grep("Resistant", x)
			next_index <- min(which(grepl("Intermediate|Susceptible", x)) - 1, length(x))
			if (length(res_index) > 0) paste(x[res_index:next_index], collapse = ";") else ""
		}),
		I = sapply(`Susceptibility profile`, function(x) {
			int_index <- grep("Intermediate", x)
			sus_index <- grep("Susceptible", x)
			if (length(int_index) > 0 && length(sus_index) > 0) paste(x[int_index:sus_index], collapse = ";") else ""
		}),
		S = sapply(`Susceptibility profile`, function(x) {
			sus_index <- grep("Susceptible", x)
			if (length(sus_index) > 0) paste(x[sus_index:length(x)], collapse = ";") else ""
		})
	) %>% 
	mutate(across(c('R', 'I', 'S'), function(x) {
		str_remove_all(x, 'Resistant;|Intermediate;|Susceptible;')
	})) %>% 
	select(-`Susceptibility profile`)

remDr$close()

### Save
left_join(result_1, char_df) %>% write_delim('atcc_extracted.tsv', delim = '\t')
