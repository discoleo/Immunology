

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

### Links: GeneBank & UniProt
read.allergen.isotbl = function(x, start = 1, n = 100, verbose = TRUE) {
	if(is.null(x$idURL)) {
		x$idURL = sub("viewallergen.php?aid=", "", x$Url, fixed = TRUE);
	}
	if(verbose) div = ceiling(n / 16);
	n = n + start - 1;
	if(n > nrow(x)) n = nrow(x);
	lst = lapply(seq(start, n), function(id) {
		lst = read.allergen.isotbl0(x$idURL[id]);
		lst$ID   = id;
		lst$Name = x$Allergen[id];
		# Feedback: is NOT real-time;
		if(verbose) {
			idn = id %% div;
			if(idn == 0) cat(id, ", ", sep = "");
		}
		return(lst);
	});
	if(verbose) cat("OK\nFinished!\n");
	lst = do.call(rbind, lst);
	return(lst);
}
read.allergen.isotbl0 = function(id) {
	url = paste0("https://www.allergen.org/viewallergen.php?aid=", id);
	doc = rvest::read_html(url);
	x   = doc |> rvest::html_element(xpath = "//table[@id='isotable']");
	nms = c("Variants", "GBPr", "GBNc", "UniProt", "PDB");
	if(inherits(x, "xml_missing")) {
		x = data.frame(NA, NA, NA, NA, NA);
		names(x) = nms;
		return(x);
	}
	x = rvest::html_table(x);
	names(x) = nms;
	return(x);
}

### UniProt Table

read.uniprot.html = function(url, verbose = TRUE) {
	doc = rvest::read_html(url);
	x   = doc |> rvest::html_element(xpath = "//table[contains(@class, 'data-table')]") |>
		rvest::html_table(header = TRUE, convert = FALSE);
	# Columns:
	x   = x[, -c(1,3)]
	names(x)[c(2,3,4,5,6)] = c("EName", "PrName", "GeneName", "Len", "GeneID");
	# Length:
	x$Len = sub(" AA$", "", x$Len);
	x$Len = gsub(",", "", x$Len);
	isNum = grepl("^\\d+$", x$Len);
	if(all(isNum)) {
		x$Len = as.integer(x$Len);
	} else {
		if(verbose) {
			print(head(x$Len[! isNum]));
		}
	}
	invisible(x);
}
