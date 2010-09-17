extract.docs.chunk <- function (
	### modification of inlinedocs
	code, name.fun
){ res <- list()
	clines <- grep(prefix, code)
	if (length(grep("#", code[1]))) {
		res$title <- gsub("[^#]*#\\s*(.*)", "\\1", code[1], 
				perl = TRUE)
	}
	if (length(clines) > 0) {
		bounds <- which(diff(clines) != 1)
		starts <- c(1, bounds + 1)
		ends <- c(bounds, length(clines))
		for (i in seq_along(starts)) {
			start <- clines[starts[i]]
			end <- clines[ends[i]]
			lab <- if (end + 1 == length(code)) 
						"value"
					else if (start == 2) 
						"description"
					else if (0 == length(grep("^\\s*#", code[start - 1], perl = TRUE))) {
						arg <- gsub("^[ \t(,]*", "", code[start - 1])	#twutz: strip leading white spaces and brackets and ,
						arg <- gsub("^([^=,]*)[=,].*", "\\1", arg)		
						arg <- gsub("^(.*)[ \t]+$*", "\\1", arg)	#twutz: remove trailing whitespaces		
						arg <- gsub("...", "\\dots", arg, fix = TRUE)
						paste("item{", arg, "}", sep = "")
					}
					else {
						next
					}
			res[[lab]] <- decomment(code[start:end])
		}
	}
	arg.pat <- paste("^[^=,#]*?([\\w\\.]+)\\s*([=,].*|\\)\\s*)?", 
			"<<\\s*(\\S.*?)\\s*$", sep = "##")
	skeleton.fields <- c("alias", "details", "keyword", "references", 
			"author", "note", "seealso", "value", "title", "description", 
			"describe", "end","examples")	#twutz: added examples
	extra.regexp <- paste("^\\s*##(", paste(skeleton.fields, 
					collapse = "|"), ")<<\\s*(.*)$", sep = "")
	cont.re <- "^\\s*##\\s*"
	in.describe <- 0
	first.describe <- FALSE
	k <- 1
	in.chunk <- FALSE
	end.chunk <- function(field, payload) {
		if ("alias" == field) {
			payload <- gsub("\\n+", "\\}\n\\\\alias\\{", 
					payload, perl = TRUE)
			chunk.sep <- "}\n\\alias{"
		}
		else if ("keyword" == field) {
			payload <- gsub("\\s+", "\\}\n\\\\keyword\\{", 
					payload, perl = TRUE)
			chunk.sep <- "}\n\\keyword{"
		}
		else if ("title" == field) {
			chunk.sep <- " "
		}
		else if ("description" == field) {
			chunk.sep <- "\n"
		}
		else if ("examples" == field) {	#twutz
			chunk.sep <- "\n"
		}
		else {
			chunk.sep <- "\n\n"
		}
		chunk.res <- NULL
		if (0 == length(grep("^\\s*$", payload, perl = TRUE))) 
			chunk.res <- if (is.null(res[[field]])) 
						payload
					else paste(res[[field]], payload, sep = chunk.sep)
		invisible(chunk.res)
	}
	while (k <= length(code)) {
		line <- code[k]
		if (0 < length(grep(extra.regexp, line, perl = TRUE))) {
			new.field <- gsub(extra.regexp, "\\1", line, 
					perl = TRUE)
			new.contents <- gsub(extra.regexp, "\\2", line, 
					perl = TRUE)
			if ("describe" == new.field) {
				if (first.describe) {
					stop("consecutive ##describe<< at line", 
							k, "in", name.fun)
				}
				else {
					if (nzchar(new.contents)) {
						if (is.null(payload) || 0 == nzchar(payload)) {
							payload <- new.contents
						}
						else {
							payload <- paste(payload, new.contents, 
									sep = "\n\n")
						}
					}
					first.describe <- TRUE
				}
			}
			else if ("end" == new.field) {
				if (in.describe > 0) {
					if ("value" == cur.field && 1 == in.describe) {
						payload <- paste(payload, "}", sep = "")
					}
					else {
						payload <- paste(payload, "}\n}", sep = "")
					}
					in.describe <- in.describe - 1
				}
				else {
					warning("mismatched ##end<< at line ", k, 
							" in ", name.fun)
				}
				if (nzchar(new.contents)) {
					if (nzchar(payload)) {
						payload <- paste(payload, new.contents, 
								sep = "\n")
					}
					else {
						payload <- new.contents
					}
				}
			}
			else {
				if (0 < in.describe) {
					if ("value" != cur.field) {
						payload <- paste(payload, "}", sep = "")
					}
					while (in.describe > 0) {
						payload <- paste(payload, "}", sep = "\n")
						in.describe <- in.describe - 1
					}
				}
				if (in.chunk) 
					res[[cur.field]] <- end.chunk(cur.field, 
							payload)
				in.chunk <- TRUE
				cur.field <- new.field
				payload <- new.contents
				if ("value" == new.field) {
					first.describe <- TRUE
				}
			}
		}
		else if (in.chunk && 0 < length(grep(cont.re, line, 
						perl = TRUE))) {
			if (0 == length(grep(prefix, line, perl = TRUE))) {
				stripped <- gsub(cont.re, "", line, perl = TRUE)
				#twutz: add inlcude directive to insert body of function
#				if( 0 < length(grep(fbody.re,stripped),perl=TRUE)){
#					fbody.re <- "^includebody\\(([^\\)]*)\\)\\s*$"
#					#gsub(fbody.re,"\\1","includebody(example.myfunction)   ",perl=TRUE)
#					#stripped="includebody(example.traceback.curr)\n"
#					fname=gsub(fbody.re,"\\1",stripped, perl=TRUE)
#					body <- as.character(body(fname))
#					stripped=gsub(stripBrackets.re,"",body,perl=TRUE)
#				}
				if (nzchar(payload)) {
					payload <- paste(payload, stripped, sep = "\n")
				}
				else {
					payload <- stripped
				}
			}
		}
		else if (0 < length(grep(arg.pat, line, perl = TRUE))) {
			not.describe <- (0 == in.describe && !first.describe)
			if (in.chunk && not.describe) {
				res[[cur.field]] <- end.chunk(cur.field, payload)
			}
			comment <- gsub(arg.pat, "\\3", line, perl = TRUE)
			arg <- gsub(arg.pat, "\\\\item\\{\\1\\}", line, 
					perl = TRUE)
			in.chunk <- TRUE
			if (not.describe) {
				cur.field <- gsub("...", "\\dots", arg, fix = TRUE)
				payload <- comment
			}
			else {
				if (first.describe) {
					if ("value" == cur.field) {
						payload <- paste(payload, "\n", arg, "{", 
								sep = "")
					}
					else {
						payload <- paste(payload, "\\describe{\n", 
								arg, "{", sep = "")
					}
					first.describe <- FALSE
					in.describe <- in.describe + 1
				}
				else {
					payload <- paste(payload, "}\n", arg, "{", 
							sep = "")
				}
				if (nzchar(comment)) {
					payload <- paste(payload, comment, sep = "")
				}
			}
		}
		else if (in.chunk) {
			if (0 == in.describe && !first.describe) {
				res[[cur.field]] <- end.chunk(cur.field, payload)
				in.chunk <- FALSE
				cur.field <- NULL
				payload <- NULL
			}
		}
		k <- k + 1
	}
	if (0 < in.describe) {
		if ("value" != cur.field) {
			payload <- paste(payload, "}", sep = "")
		}
		while (in.describe > 0) {
			payload <- paste(payload, "}", sep = "\n")
			in.describe <- in.describe - 1
		}
	}
	if (in.chunk) 
		res[[cur.field]] <- end.chunk(cur.field, payload)
	res
}
#mtrace(extract.docs.chunk)
