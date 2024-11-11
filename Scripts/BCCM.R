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

# Navigate to the URL
url <- 'https://bccm.belspo.be/page/gDNA-LMG'
remDr$navigate(url)

# Wait for the page to load
Sys.sleep(5)

# Find all elements by the CSS selector
elements <- remDr$findElements(using = "css selector", value = "#summary-content > mat-card > mat-card-content > perfect-scrollbar > div > div.ps-content > div > table > tbody > tr > td.mat-cell.cdk-cell.cdk-column-name.mat-column-name.ng-star-inserted > a > overflow-tooltip > div")

# Take a screenshot
remDr$screenshot(TRUE)

data_BCCM <- list()

# Loop through each element
for (i in seq_along(elements)) {
	Sys.sleep(1)
	
	# Scroll to the element if necessary
	if (i > 8) {
		remDr$executeScript("arguments[0].scrollIntoView(true);", list(elements[[i - 7]]))
	}
	
	Sys.sleep(1)
	elements[[i]]$clickElement()
	
	# Wait for the data to load
	Sys.sleep(2)
	
	# Extract data
	datalist <- remDr$findElements(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-body.ps.ps--active-y > details-visualizer > div > div > detailsgenerator > div > efielddetails > div > details-line > div > div > div > div')
	datalist2 <- remDr$findElements(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-body.ps.ps--active-y > details-visualizer > div > div > detailsgenerator > div > synlinkfielddetails > div > details-line > div > div > div > div > div > a')
	dataheader <- remDr$findElements(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-body.ps.ps--active-y > details-visualizer > div > div > detailsgenerator > div > efielddetails > div > details-line > div > div > mat-hint > overflow-tooltip > div')
	dataheader2 <- remDr$findElements(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-body.ps.ps--active-y > details-visualizer > div > div > detailsgenerator > div > synlinkfielddetails > div > details-line > div > div > mat-hint > overflow-tooltip > div')
	
	s <- c(
		lapply(datalist, function(x) { x$getElementText() }) %>% unlist(),
		lapply(datalist2, function(x) { x$getElementText() }) %>% unlist()
	)
	names(s) <- c(
		lapply(dataheader, function(x) { x$getElementText() }) %>% unlist(),
		lapply(dataheader2, function(x) { x$getElementText() }) %>% unlist()
	)
	
	if (is.null(s)) {
		closer <- remDr$findElement(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-header > a.header-action.action-close > mat-icon')
		closer$clickElement()
		print(i)
		next
	}
	
	v <- as.data.frame(t(s))
	
	closer <- remDr$findElement(using = 'css selector', value = 'body > div > div.dockmodal.popped-out.no-footer.resizable > div.dockmodal-header > a.header-action.action-close > mat-icon')
	closer$clickElement()
	
	data_BCCM[[i]] <- v
}

# Save the data to a CSV file
data_BCCM %>% bind_rows() %>% tibble() %>% write_delim('Data_BCCM.csv', delim = ';', na = '')