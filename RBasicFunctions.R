print.functions <- function(){

	cat("Standard errors, error bars, rmsd etc:\n",sep="")
	cat("--------------------------------------\n",sep="")
	cat("se(x) - standard error\n",sep="")
	cat("rmsd(x) - root mean squared deviation\n",sep="")
	cat("errbar(x,y,error,color=black) - plot error bars on (x,y)\n",sep="")
	cat("runmean(x,window) - running average of x with window, returns same length as x, with smoothed end points\n",sep="")
	cat("Misc.:\n",sep="")
	cat("--------\n",sep="")
	cat("C.Var(x) - calc coefficient of variation around the mean\n",sep="")
	cat("rm.levels(factor) - remove non-used levels from factor\n",sep="")
	cat("h(x,...) - shortcut for head(x,...), see ?head\n",sep="")
	cat("last(x) - get last element of vector, list, data.frame, etc.\n",sep="")
	cat("format.hrs.min.sec(seconds) - return hrs:min:sec or min:sec if sec < 3600\n",sep="")
	cat("describe(x) - an alternative to summary of numeric vector or list\n",sep="")
	cat("instant_pkgs(c(pkg)) - instant packages for multiple package install\n",sep="")
	cat("ndf(df) - Returns names(df) in single column, numbered matrix format", sep = "")
	cat("macopen/o - Open find to current directory", sep = "")
	cat("gcn(df,num) - return column name given index num", sep = "")
	cat("gci(df,quoted) - return column index given quoted colname", sep = "")
	}


## Do you want to automatically convert strings to factor variables in a data.frame?
## WARNING!!! This makes your code less portable/reproducible.
options(stringsAsFactors=FALSE)


## Returns names(df) in single column, numbered matrix format.
ndf <- function(df) matrix(names(df))


## Open Finder to the current directory on mac
macopen <- function(...) if(Sys.info()[1]=="Darwin") system("open .")
o       <- function(...) if(Sys.info()[1]=="Darwin") system("open .")



## Standard errors, error bars, rmsd etc:
##########################################################################################


rmsd <- function(data,model){
	sqrt(mean((data - model)^2))
}


se <- function(x){
	sd(x)/sqrt(length(x))
}




runmean <- function(x,window){
	require(zoo)
	ori <- x
	new <- rollmean(x,window,na.pad=T)
	new[is.na(new)] <- ori[is.na(new)]
	new <- smoothEnds(new,window)
	new
}

## Misc
##########################################################################################

# GET STARS-----------------
get_stars = function(p) {
  stars = findInterval( p, c( 0, 0.001, 0.01, 0.05, 0.1))
  codes = c( "***" , "**", "*", ".", " ")
  codes[stars]
}
options( scipen = 1)

get_range = function(p) {
  range = findInterval( p, c( 0, 0.001, 0.01, 0.05, 0.1))
  codes = c( "<.001" , "<.01", "<.05", "<.10", " ")
  codes[range]
}
options( scipen = 1)

get_range.tex = function(p) {
  range = findInterval( p, c( 0, 0.001, 0.01, 0.05, 0.1))
  codes = c( "001" , "01", "05", "10", "10")
  codes[range]
}
options( scipen = 1)


#GET COLNAME AND INDEX NAME
adf <- function(x) {as.data.frame(x)}
gci <- function(df, quoted) {
  z = which(colnames(df)==eval(quote(quoted)))
  return(z)
}
gcn <- function (df, num) {
  z = colnames(df)[num]
  return(z)
}


 instant_pkgs <- function(pkgs) {
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }

	if (length(pkgs_miss) == 0) {
	message("\n ...Packages were already installed!\n")
	}

pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }


attached <- search()
    attached_pkgs <- attached[grepl("package", attached)]
    need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]

    if (length(need_to_attach) > 0) {
      for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
    }

	if (length(need_to_attach) == 0) {
	message("\n ...Packages were already loaded!\n")
	}
}
