library(RSelenium)
library(httr)
library(rvest)
library(tidyverse)

# Pull and run the Docker container for Selenium
shell('docker pull selenium/standalone-chrome:4.2.2')
shell('docker run -d -p 4445:4444 --shm-size 4g selenium/standalone-chrome:4.2.2')

# Initialize the remote driver
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

# JavaScript to center elements on the viewport
center_script <- "
var viewPortHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
var elementTop = arguments[0].getBoundingClientRect().top;
window.scrollBy(0, elementTop-(viewPortHeight/2));
"

# Navigate to the URL
url <- 'https://ccug.se/collections/search?collection=antimicrobialresistance'
remDr$navigate(url)
remDr$screenshot(TRUE)
Sys.sleep(5)

data_CCUG <- list()

# Set the number of records to 100
in100 <- remDr$findElement(using = 'css selector', value = '#number_of_records > a:nth-child(4)')
remDr$executeScript(center_script, list(in100))
in100$clickElement()

# Loop through pages
for (j in 1:2) {
	elements <- remDr$findElements(using = "css selector", value = "#table_ccug_num")
	
	# Loop through each element
	for (i in seq_along(elements)) {
		elementz <- remDr$findElements(using = "css selector", value = "#table_ccug_num")
		remDr$executeScript(center_script, list(elementz[[i]]))
		elementz[[i]]$clickElement()
		
		name <- remDr$findElement(using = 'css selector', value = '#content > h1')$getElementText() %>% unlist()
		id <- str_remove_all(name, ' -.*')
		species <- str_remove_all(name, '.* - ')
		
		metadata <- remDr$findElements(using = 'css selector', value = '#content > table > tbody > tr > td')
		metc <- c()
		metcn <- c()
		current_header <- ""
		
		for (z in seq_along(metadata)) {
			text <- metadata[[z]]$getElementText() %>% unlist()
			
			if (str_detect(text, ':$')) {
				current_header <- text
				metcn <- c(metcn, current_header)
			} else {
				if (current_header %in% names(metc)) {
					metc[current_header] <- paste(metc[current_header], text, sep = "\t")
				} else {
					metc[current_header] <- text
				}
			}
		}
		
		metc['id'] <- id
		metc['species'] <- species
		v <- as.data.frame(t(metc))
		x <- i + (j - 1) * 100
		data_CCUG[[x]] <- v
		
		remDr$findElement(using = 'css selector', value = '#ov_buttons > a')$clickElement()
		print(paste0(x, ' ', name))
	}
	
	if (j == 2) break
	
	nextpage <- remDr$findElement(using = 'css selector', value = '#next > i')
	remDr$executeScript(center_script, list(nextpage))
	nextpage$clickElement()
}

remDr$screenshot(TRUE)

# Save the data to a CSV file
data_CCUG %>% bind_rows() %>% tibble() %>% relocate(id, species) %>% write_delim('Data_CCUG.csv', delim = ';', na = '')