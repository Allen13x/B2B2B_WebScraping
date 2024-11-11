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
url <- 'https://refs.wdcm.org/refs/browse/species'
remDr$navigate(url)
remDr$screenshot(TRUE)
Sys.sleep(5)

data_WDCM <- list()
g <- 0
j <- 1

for (p in 1:j) {
	if (p > 1) {
		arrow <- remDr$findElement(using = 'css selector', value = '#app > div > div.fd-browse > div.fd-browse-results-list.fd-species > div.fd-table-bottom > div > button.btn-next > i')
		remDr$executeScript(center_script, list(arrow))
		arrow$clickElement()
		Sys.sleep(1)
	}
}

while (TRUE) {
	elements <- remDr$findElements(using = "css selector", value = "#app > div > div.fd-browse > div.fd-browse-results-list.fd-species > div > ul > li > div.name > span")

	for (i in seq_along(elements)) {
		elementz <- remDr$findElements(using = "css selector", value = "#app > div > div.fd-browse > div.fd-browse-results-list.fd-species > div > ul > li > div.name > span")
		remDr$executeScript(center_script, list(elementz[[i]]))
		
		id <- elementz[[i]]$getElementText() %>% unlist()
		elementz[[i]]$clickElement()
		Sys.sleep(1)
		
		species <- paste0(
			remDr$findElement(using = 'css selector', value = '#app > div > div.fd-browse-detail > div.fd-browse-detail-base > ul > li > div.val > span > span.firstname')$getElementText() %>% unlist(),
			' ',
			remDr$findElement(using = 'css selector', value = '#app > div > div.fd-browse-detail > div.fd-browse-detail-base > ul > li > div.val > span > span.middlename')$getElementText() %>% unlist()
		)
		
		stids <- remDr$findElements(using = 'css selector', value = '#app > div > div.fd-browse-detail > div.fd-browse-detail-base > ul > li:nth-child(2) > div.val > span')
		stid <- lapply(stids, function(x) x$getElementText()) %>% unlist() %>% str_remove_all(pattern = ';') %>% str_c(collapse = ';')
		
		metc <- c(id = id, species = species, `Strain ID` = stid)
		v <- as.data.frame(t(metc))
		
		x <- i + g
		data_WDCM[[x]] <- v
		
		remDr$goBack()
		Sys.sleep(1)
		
		for (p in 1:j) {
			if (p > 1) {
				arrow <- remDr$findElement(using = 'css selector', value = '#app > div > div.fd-browse > div.fd-browse-results-list.fd-species > div.fd-table-bottom > div > button.btn-next > i')
				remDr$executeScript(center_script, list(arrow))
				arrow$clickElement()
				Sys.sleep(1)
			}
		}
		print(paste0(x, ' ', species, '-', id))
	}

	g <- g + length(elements)
	j <- j + 1
	
	arrow <- remDr$findElement(using = 'css selector', value = '#app > div > div.fd-browse > div.fd-browse-results-list.fd-species > div.fd-table-bottom > div > button.btn-next > i')
	remDr$executeScript(center_script, list(arrow))
	arrow$clickElement()
	Sys.sleep(1)
	
	if (j > 11) break
	print(paste0('Change page to ', j))
}

remDr$screenshot(TRUE)

data_WDCM %>% bind_rows() %>% tibble() %>% relocate(id, species) %>% distinct() %>% write_delim('Data_WDCM.csv', delim = ';', na = '')