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
url <- 'https://www.beiresources.org/AntimicrobialSearch.aspx'
remDr$navigate(url)
remDr$screenshot(TRUE)
Sys.sleep(5)

data_BEI <- list()

# Close the login popup
nologin <- remDr$findElements(using = 'css selector', value = '#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_XPNCartPopupControl_btnCancel')
nologin[[1]]$clickElement()

# Loop through pages
for (j in 1:50) {
	elements <- remDr$findElements(using = "css selector", value = "#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_rptResults > tbody > tr > td > table > tbody > tr > td > div > button")
	
	# Loop through each element
	for (i in seq_along(elements)) {
		remDr$executeScript(center_script, list(elements[[i]]))
		elements[[i]]$clickElement()
		
		dr <- tryCatch(remDr$findElement(using = 'css selector', value = paste0('#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_rptResults_blResistant_', i - 1)), error = function(e) NULL)
		ds <- tryCatch(remDr$findElement(using = 'css selector', value = paste0('#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_rptResults_blSusceptible_', i - 1)), error = function(e) NULL)
		names <- tryCatch(remDr$findElement(using = 'css selector', value = paste0('#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_rptResults_lblDescription_', i - 1, ' > i')), error = function(e) NULL)
		id <- tryCatch(remDr$findElement(using = 'css selector', value = paste0('#dnn_ctr14461_XPNBEI_AntimicrobialSearchResults_rptResults_hlATCCNum_', i - 1)), error = function(e) NULL)
		
		s <- c(
			'id' = if (is.null(id)) "" else id$getElementText() %>% unlist(),
			'species' = if (is.null(names)) "" else names$getElementText() %>% unlist(),
			'R' = if (is.null(dr)) "" else dr$getElementText() %>% unlist(),
			'S' = if (is.null(ds)) "" else ds$getElementText() %>% unlist()
		)
		
		v <- as.data.frame(t(s))
		x <- i + (j - 1) * 20
		data_BEI[[x]] <- v
	}
	
	if (j == 50) break
	
	j <- j + 1
	if ((j %% 10) == 1) j <- '...'
	
	page <- remDr$findElements(using = "link text", value = as.character(j))
	remDr$executeScript(center_script, list(page[[2]]))
	page[[2]]$clickElement()
	Sys.sleep(3)
	print(j)
}

remDr$screenshot(TRUE)

# Save the data to a CSV file
data_BEI %>% bind_rows() %>% tibble() %>% write_delim('Data_BEI.csv', delim = ';', na = '')