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
url <- 'https://www.dsmz.de/collection/catalogue/microorganisms/special-groups-of-organisms/who-priority-pathogens-list'
remDr$navigate(url)
remDr$screenshot(TRUE)
Sys.sleep(5)

data_DSMZ <- list()

# Find all elements by the CSS selector
elements <- remDr$findElements(using = "css selector", value = "#c4158 > div > table > tbody > tr > td > a")

# Loop through each element
for (i in seq_along(elements)) {
	elementz <- remDr$findElements(using = "css selector", value = "#c4158 > div > table > tbody > tr > td > a")
	remDr$executeScript(center_script, list(elementz[[i]]))
	
	urln <- elementz[[i]]$getElementAttribute('href') %>% unlist()
	id <- elementz[[i]]$getElementText() %>% unlist()
	
	remDr$navigate(urln)
	
	species <- remDr$findElement(using = 'css selector', value = '#innerwrapper > div.maincol > div > div > h1')$getElementText() %>% unlist()
	metadata <- remDr$findElements(using = 'css selector', value = '#innerwrapper > div > div > div > div > div > div')
	
	metc <- c()
	current_header <- ""
	
	for (z in seq_along(metadata)) {
		text <- metadata[[z]]$getElementText() %>% unlist()
		
		if (str_detect(text, ':$')) {
			current_header <- text
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
	data_DSMZ[[i]] <- v
	
	remDr$navigate(url)
	print(paste0(i, ' ', species, '-', id))
}

remDr$screenshot(TRUE)

# Save the data to a CSV file
data_DSMZ %>% bind_rows() %>% tibble() %>% relocate(id, species) %>% write_delim('Data_DSMZ.csv', delim = ';', na = '')