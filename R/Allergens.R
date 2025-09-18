

### Allergen DB
url.allergens = function() {
	fields = c("allergenname", "allergensource", "TaxSource",
		"TaxOrder", "foodallerg", "bioname");
	vals = c(rep("", 4), "all", "");
	fields = paste(fields, vals, sep="=", collapse = "&");
	url = "https://www.allergen.org/search.php?";
	url = paste0(url, fields, "&browse=Browse");
	return(url);
}
read.allergens.html = function(url = NULL, strip = NULL) {
	if(is.null(url)) {
		url = url.allergens();
	}
	doc = rvest::read_html(url);
	x   = doc |> rvest::html_element(xpath = "//table[@id='example']") |> rvest::html_table();
	print("Finished downloading.");
	# Date:
	names(x)[c(5, ncol(x))] = c("Route", "Updated");
	dt1 = as.POSIXlt(x$Created, tz = "GMT", format = "%Y-%m-%d");
	dt2 = as.POSIXlt(x$Updated, tz = "GMT", format = "%Y-%m-%d");
	x$Created = NULL; x$Updated = NULL;
	x$Created = as.Date(dt1);
	x$Updated = as.Date(dt2);
	if(! is.null(strip) && length(strip) > 0) {
		if(length(strip) > 1) {
			warning("Strip multiple tokens: NOT yet implemented!");
		}
		x$Title = gsub(strip, "", x$Title);
	}
	# URL / Link:
	yA = doc |> rvest::html_elements(xpath = "//table[@id='example']/tbody/tr/td/a");
	yT = rvest::html_text(yA, trim = TRUE);
	yH = rvest::html_attr(yA, "href");
	yH = data.frame(Allergen = yT, Url = yH);
	x  = merge(x, yH, by = "Allergen");
	return(x);
}

