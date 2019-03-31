
# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
risk_dates = c(
  "1987-10-01",
  "1994-02-01",
  "1997-07-01",
  "1998-08-01",
  "1998-10-01",
  "2000-07-01",
  "2001-09-01")
risk_labels = c(
  "Black Monday",
  "Bond Crash",
  "Asian Crisis",
  "Russian Crisis",
  "LTCM",
  "Tech Bubble",
  "Sept 11")


# Event lists - FOR BEST RESULTS, KEEP THESE DATES IN ORDER
cbs_dates = c(
  "2008-11-25",
  "2009-03-18",
  "2010-11-03",
  "2011-09-21",
  "2012-06-20",
  "2012-09-13",
  "2012-12-12",
  "2013-05-22",
  "2013-06-19",
  "2013-09-18",
  "2014-08-29",
  "2015-12-16",
  "2016-02-11" ,
  "2016-11-08",
  "2016-12-14")

cbs_labels = c(
  "Lending $800 Bil",
  "purchase LT bonds",
  "purchase $600 Bil bonds",
  "Lower consumer loans",
  "Operation Twist",
  "launch QE3",
  "buy more bonds",
  "plan to taper",
  "Taper Tantrum",
  "No tapering" ,
  "QE3 End",
  "Fed Rate Hike-25bp",
  "Janet give-up",
  "Trump Win-Reflation Hope",
  "Fed Rate Hike to 50bp")


# 1. November 25, 2008
# Fed unveils $800 billion plan to bolster lending, housing

# 2. March 18, 2009
# Federal Reserve to buy $300 billion in longer-term Treasury bonds


# 3. November 3, 2010
# Federal Reserve to buy $600 billion in bonds


# 4. September 21, 2011
# Federal Reserve moves to lower interest rates on consumer loans
# with a $400 billion debt-swap program

# 5. June 20, 2012
# Fed extends 'Operation Twist'
# The Fed says it will extend its holdings of long-term government bonds by $267
# billion in another effort to bring down borrowing costs.

# 6. September 13, 2012
# Fed to launch QE3 by buying mortgage securities
# By an 11-to-1 vote, the Federal Reserve decides to launch a third round of
# open-ended bond purchases ??? so-called QE3 ??? saying it will buy $40 billion of
# agency mortgage-backed securities per month.

# 7. December 12, 2012
# Fed to buy more bonds as it sets jobless target

# 8.January 14, 2013
# Bernanke downplays inflation risk of QE3
# Federal Reserve Chairman Ben Bernanke plays down the fears of some more
# hawkish central bankers and investors that the Fed's aggressive
# bond-buying program will lead to higher inflation.
# I don't believe significant inflation is going to be the
# result of any of this, Bernanke says in a speech at the University of Michigan.

# 8.May 22, 2013
# Bernanke tells Congress step down in QE could come soon

# 9. June 19, 2013 - Taper Tantrum
# Bernanke says the central bank may scale back its bond purchases this year,
# depending on the economic outlook

# September 18, 2013
# Fed decides not to taper



has_internet <- function(){
  !is.null(curl::nslookup("r-project.org", error = FALSE))
}

if( has_internet() == TRUE){
  
  
  # These are start and end dates, formatted as xts ranges.
  ## http://www.nber.org-cycles.html
  getSymbols("USREC",src="FRED")
  start <- index(USREC[which(diff(USREC$USREC)==1)])
  end   <- index(USREC[which(diff(USREC$USREC)==-1)-1])
  recession_df <- data.frame(start=start, end=end[-1])
  # recession.df <- subset(reccesion_df, start >= min(as.Date(index(factors))))
 
  
  recession_df <- recession_df %>%
    mutate_at(vars(c("start", "end")), funs(as.Date))
  
  write.csv(recession_df,"raw_data/recession_df.csv")
  
} else {
  
  recession_df <- read.csv("raw_data/recession_df.csv")
}



cbs_df <- data.frame(date=as.Date(cbs_dates), event= cbs_labels)
