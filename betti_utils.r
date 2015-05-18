##########  Atlantic AMO ##################################################


read_AMO <- function(file) {

 ## Download and process AMO Data File ###############
  require("reshape")
   a_file <-file
  ## Find number of rows in the file
    (a_rows <- length(readLines(a_file)))

  ## Read file as  char vector, one line per row, Exclude first row 
    a_lines <- readLines(a_file, n=a_rows)
    num_a <- a_rows - 4
    a_lines_2 <- a_lines[2:num_a]

  ##Convert the character vector to a dataframe using fixed width format (fwf)
    a_df <- read.table(
     textConnection(a_lines_2), header=F, skip=0, colClasses = "numeric")
    closeAllConnections()
    names(a_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  ## Convert from wide format to long format
    dfa <- melt(a_df, id.var="Year", variable_name="Month")
    names(dfa) <- c("yr", "mon", "AMO")

  ## dfa$Month is factor, Convert to month number & calc yr_frac
    amo_mo_num <- unclass(dfa$mon)
    amo_mo_frac <- as.numeric((amo_mo_num-0.5)/12)
    yr_frac <- as.numeric(dfa$yr) + amo_mo_frac
   dfa <- data.frame(yr_frac,  dfa)
   dfa <- subset(dfa, dfa$AMO> -90)
   dfa <- dfa[order(dfa$yr_frac),]
   dfa <- subset(dfa, dfa$yr_frac > 1856)
   dfa <- dfa[,c(1,4)]
}

func_AMO <- function() {

 ## Download and process AMO Data File ###############
  require("reshape")
  a_link <- "http://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data"
  a_file <-    c("amo_latest.txt")
  download.file(a_link, a_file)

  ## Find number of rows in the file
    (a_rows <- length(readLines(a_file)))

  ## Read file as  char vector, one line per row, Exclude first row 
    a_lines <- readLines(a_file, n=a_rows)
    num_a <- a_rows - 4
    a_lines_2 <- a_lines[2:num_a]

  ##Convert the character vector to a dataframe using fixed width format (fwf)
    a_df <- read.table(
     textConnection(a_lines_2), header=F, skip=0, colClasses = "numeric")
    closeAllConnections()
    names(a_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  ## Convert from wide format to long format
    dfa <- melt(a_df, id.var="Year", variable_name="Month")
    names(dfa) <- c("yr", "mon", "AMO")

  ## dfa$Month is factor, Convert to month number & calc yr_frac
    amo_mo_num <- unclass(dfa$mon)
    amo_mo_frac <- as.numeric((amo_mo_num-0.5)/12)
    yr_frac <- as.numeric(dfa$yr) + amo_mo_frac
   dfa <- data.frame(yr_frac,  dfa)
   dfa <- subset(dfa, dfa$AMO> -90)
   dfa <- dfa[order(dfa$yr_frac),]
   dfa <- subset(dfa, dfa$yr_frac > 1856)
   dfa <- dfa[,c(1,4)]
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
####  ann_avg function ##############################################
# Calculate Annual Average From Monthly Time Series

    ann_avg<-function(x) {
	n<-length(x)
	m<-n-12*floor(n/12)
	if(m>0) x<-c(x, rep(NA,12-m))
	years<-length(x)/12
	x<-array(x,dim=c(12,years))
	annavg<-apply(x,2,mean,na.rm=T)
	return(annavg)
	}
	
##################################################################################
####  snap function ##############################################
      ## Function to provide data.frame snapshot
      # 1st 4 rows; middle 5 rows; last 4 rows

  snap <- function(x) {
  nx <- nrow(x)
  mid <- as.integer(nx/2)
  p_seq <- c(seq(1,4,1), seq(mid-2,mid+2,1), seq(nx-3,nx,1))
  print(x[p_seq,])
     }

##################################################################################
####  date to yr_frac function ##############################################

 func_dt_2_yf <- function(dt){  
   ## converts dt(as.Date) to yr_frac)
   yr <- as.numeric(format(dt, format="%Y"))
   mo <- as.numeric(format(dt, format="%m"))
   dy <- as.numeric(format(dt, format="%d"))
   yr_frac <- as.numeric(yr + (mo-1)/12 + (dy/30)/12)
   return(yr_frac)}


##################################################################################
####  year & month number to yr_frac function ##################################

func_yr_mn_2_yf <- function(yr, mo){  
   ## converts yr & mo number to yr_frac)
   yr_frac <- as.numeric(yr + (mo-0.5)/12)
return(yr_frac)}

##################################################################################



####  yr_mn function ##############################################
  ## to get yr_mn from yr_frac
  # y_f is yr_frac vector 
   func_yr_mn <- function(y_f) {  
    yr <- as.integer(y_f) 
    ## Each month is 1/12 or 0.083 of calandar year
    inc <- 1/12
    mo <- ceiling((y_f-yr)/(inc)) 
    mo_char <-  formatC(mo,width=2,flag='0') 
    yr_mn <- as.numeric(as.character(paste(yr, mo_char, sep="")  ))
    return(yr_mn)
 }
##################################################################################

mon_name <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
###############################################################################################
##########  Jones' Gibraltar-Iceland NAO index ################################################
 
func_NAO <- function() {
  ## updated 11/12/10 
   require("reshape")
  link <-   "http://climexp.knmi.nl/data/inao.dat"
  in_data <- read.csv(link,
             sep = "", dec=".", as.is = T,header=F,
             colClasses=rep("numeric",13),
             comment.char = "#", na.strings = c("*", "-","-99.9", "-999.9000","-999"))
  names(in_data) = c("Yr","Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug",
                         "Sept", "Oct", "Nov", "Dec")
 # Use reshape - melt function to convert from wide format to long format
 ## Specify id var, in this case year
 ### Specify measure variable as either names or col numbers: in this case use col numbers
  NAO<- melt(in_data, id.var=c("Yr"), measure.var=c(2:13))
  names(NAO) <- c("yr", "mon", "NAO_jones")
  yr_frac <- as.numeric(NAO$yr + (as.numeric(NAO$mon)-0.5)/12 )
  NAO<- data.frame(yr_frac, NAO)
  NAO<- NAO[order(NAO$yr_frac),]
  NAO<- NAO[,c(1,4)]
  ## source file shows full year of months, need to remove ross for future months
   cur_dt <- Sys.Date()
   cur_yr <- as.numeric(format(cur_dt, format="%Y"))
   prev_mo <- as.numeric(format(cur_dt, format="%m")) -1
   max_yr_frac <- as.numeric(cur_yr + (prev_mo-0.5)/12 )
   NAO<- subset(NAO, yr_frac <= max_yr_frac) 
   return(NAO)
}

##########  MEI ENSO index ################################################
 
func_MEI <- function() {
  ## updated 11/12/10 
   require("reshape")
  link <-   "http://climexp.knmi.nl/data/imei.dat"
  in_data <- read.csv(link,
             sep = "", dec=".", as.is = T,header=F,
             colClasses=rep("numeric",13),
             comment.char = "#", na.strings = c("*", "-","-99.9", "-999.9000","-999","-999.9"))
  names(in_data) = c("Yr","Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug",
                         "Sept", "Oct", "Nov", "Dec")
 # Use reshape - melt function to convert from wide format to long format
 ## Specify id var, in this case year
 ### Specify measure variable as either names or col numbers: in this case use col numbers
  MEI<- melt(in_data, id.var=c("Yr"), measure.var=c(2:13))
  names(MEI) <- c("yr", "mon", "MEI_jones")
  yr_frac <- as.numeric(MEI$yr + (as.numeric(MEI$mon)-0.5)/12 )
  MEI<- data.frame(yr_frac, MEI)
  MEI<- MEI[order(MEI$yr_frac),]
  MEI<- MEI[,c(1,4)]
  ## source file shows full year of months, need to remove ross for future months
   cur_dt <- Sys.Date()
   cur_yr <- as.numeric(format(cur_dt, format="%Y"))
   prev_mo <- as.numeric(format(cur_dt, format="%m")) -1
   max_yr_frac <- as.numeric(cur_yr + (prev_mo-0.5)/12 )
   MEI<- subset(MEI, yr_frac <= max_yr_frac) 
   return(MEI)
}

# sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", 
              # "b", "a", "a", "b", "b", "b", "a")

# gexf_PCT8 <- igraph.to.gexf(g_PCT8)



# predict(mcCCRC, newdata = c("H","H"), n.ahead = 5)







# rle_consec <- function(x)
# {
    # if (!is.vector(x) && !is.list(x))
        # stop("'x' must be an atomic vector")
    # n <- length(x)
    # if (n == 0L)
    # return(structure(list(lengths = integer(), values = x),
             # class = "rle_consec"))
    # y <- x[-1L] != x[-n] + 1
    # i <- c(which(y | is.na(y)), n)
    # structure(list(lengths = diff(c(0L, i)), values = x[i]),
              # class = "rle_consec")
# }

# Function to calculate first-order Markov transition matrix.
# Each *row* corresponds to a single run of the Markov chain

# trans.matrix <- function(X, prob=T)
# {
    # tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
    # if(prob) tt <- tt / rowSums(tt)
    # tt
# }

# parse_date_time(x, c("%y%m%d", "%y%m%d %H%M"))

# difftime(syrrupan$Started,syrrupan$dos1,units="days")
