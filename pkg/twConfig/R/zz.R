.tmp.f <- function(){}      # overwrite other tmp functions for R CMD Check


# from twMisc
.fileExt <- function (filenames) 
{
    ifelse(regexpr("\\.", filenames) != -1, sub("(^.*[.])([^.]*)$", 
                    "\\2", filenames, perl = TRUE), "")
}
