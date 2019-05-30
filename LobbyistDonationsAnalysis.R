### SEARCHING FOR LOBBYIST DONATIONS TO PRESIDENTIAL CANDIDATES
### By Casey Tolan
### An analysis comparing a list of registered California and San Francisco lobbyists to campaign donors to presidential candidates
### Leads to a list of possible lobbyist-donor matches that needs to be checked and verified manually
### Used for data analysis for San Jose Mercury News article about Kamala Harris: http://mercurynews.com/2019/05/30/california-democratic-convention-presidential-primary-joe-biden/

# ------------------------------------------
# PRE-ANALYSIS: scraping SF lobbyist website to get database of registered SF lobbyists. 
# I downloaded the three html pages myself and read them into R, as website didn't allow direct scraping:
#     webpage <- read_html("data/sflobbyists1.html")
#     lobbyistNames <- html_nodes(webpage, "div h4")
#     sflobbyists <- as.data.frame(html_text(lobbyistNames))
#     webpage <- read_html("data/sflobbyists2.html")
#     lobbyistNames <- html_nodes(webpage, "div h4")
#     sflobbyists <- rbind(sflobbyists,as.data.frame(html_text(lobbyistNames)))
#     webpage <- read_html("data/sflobbyists3.html")
#     lobbyistNames <- html_nodes(webpage, "div h4")
#     sflobbyists <- rbind(sflobbyists,as.data.frame(html_text(lobbyistNames)))
# Then I cleaned the results in Excel to get a final database of SF lobbyists -- SFlobbyists.csv
# I obtained the list of all current registered CA lobbyists in a database from CA Secretary of State's office 

# --------------------------------------------
# GETTING STARTED
rm(list = ls()) # clear existing variables
fecReport <- read.csv("data/HarrisQ1.csv",stringsAsFactors=F) # Get FEC report

# Filter out individual donors
indivContrib <- filter(fecReport, FormType == "SA17A", MemoCode != "X" ) # Only individual donors -- FEC recommends this methodology

# Deal with refunds -- add them in to contribution database as negative
refunds <-filter(fecReport, FormType == "SB28A", MemoCode != "X" ) # Refunds
refundsNegative <- mutate(refunds,ContribAmount=ContribAmount * -1)
donationsWithRefunds = rbind(indivContrib,refundsNegative) # The main contribution + refund database we will work with

# Just get the variables we care about
donationsWithRefunds <- select(donationsWithRefunds,FirstName,LastName,Prefix,Suffix,Employer,Occupation,City,State,Address1,Address2,Zip,ContribDate,ContribAmount,AggregateAmount,X)

# ---------------------------------------------------
# CHECK FOR CALIFORNIA LOBBYISTS
# Import the database of all currently registered CA lobbyists 
CAlobbyists <- read.csv("data/CAlobbyists.csv",stringsAsFactors = F)
checkCAlobbyists <- donationsWithRefunds[0,]   # Data frame for lobbyist-donor matches

# Check for matches between Harris donors and registered CA lobbyists
pb <- txtProgressBar(min = 1, max = nrow(donationsWithRefunds) , style = 3) # visualizing progress bar
for (row in 1:nrow(donationsWithRefunds)) { #nrow(donationsWithRefunds)) {
  
  donorFirst = toupper(donationsWithRefunds$FirstName[row])
  donorLast = toupper(donationsWithRefunds$LastName[row])
  
  lobbyistMatches = filter(CAlobbyists,grepl(donorFirst,FirstName)&grepl(donorLast,LastName))
  
  if (nrow(lobbyistMatches) > 0) { # if there is a match with a lobbyist record
      checkCAlobbyists <- rbind(checkCAlobbyists,donationsWithRefunds[row,]) # Save this row in checkCAlobbyists dataframe
  }

  setTxtProgressBar(pb,row) # to visualize progress bar
}

# ---------------------------------------------------
# CHECK FOR SAN FRANCISCO LOBBYISTS
# Import the database of all currently registered SF lobbyists -- scraped using code above
SFlobbyists <- read.csv("data/SFlobbyists.csv",stringsAsFactors = F)
checkSFlobbyists <- donationsWithRefunds[0,]   # Data frame for lobbyist-donor matches

# Check for matches between Harris donors and registered SF lobbyists
pb <- txtProgressBar(min = 1, max = nrow(donationsWithRefunds) , style = 3) # visualizing progress bar
for (row in 1:nrow(donationsWithRefunds)) { #nrow(donationsWithRefunds)) {

  donorFirst = toupper(donationsWithRefunds$FirstName[row])
  donorLast = toupper(donationsWithRefunds$LastName[row])

  lobbyistMatches = filter(SFlobbyists,grepl(donorFirst,toupper(FirstName))&grepl(donorLast,toupper(LastName)))

  if (nrow(lobbyistMatches) > 0) { # if there is a match with a lobbyist record
    checkSFlobbyists <- rbind(checkSFlobbyists,donationsWithRefunds[row,]) # Save this row in checkSFlobbyists dataframe
  }

  setTxtProgressBar(pb,row) # to visualize progress bar
}

# The final results will have plenty of false positives -- it's important to go through manually and weed them out
# It would be possible to also match by donor employer, but that's unreliable because they often describe their employer in different ways

write.csv(checkCAlobbyists, "output/possible Harris CA lobbyist donors.csv")
write.csv(checkSFlobbyists, "output/possible Harris SF lobbyist donors.csv")