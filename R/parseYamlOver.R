##########################################################################################
# Designed and developed by Tinniam V Ganesh
# Date : 2 May 2020
# Function: parseYamlOver
# This function converts a given yaml file to a dataframe delivery by delivery.
#
###########################################################################################
#' @title
#' Parse yaml file and convert to dataframe
#'
#' @description
#' This function parses the yaml file and converts it into a data frame. This is an internal function and
#' is used by convertAllYaml2RDataframes() & convertYaml2RDataframe()
#'
#' @usage
#' parseYamlOver(match,s,ateam,delivery,meta)
#'
#' @param match
#' The dataframe of the match
#'
#' @param s
#' The string with the delivery
#'
#' @param ateam
#' The team
#'
#'
#' @param delivery
#' The delivery of the over
#'
#' @param meta
#' The meta information of the match
#'
#' @return overs
#' The dataframe of overs
#'
#' @references
#' \url{https://cricsheet.org/}\cr
#' \url{https://gigadom.in/}\cr
#' \url{https://github.com/tvganesh/yorkrData/}
#'
#'
#' @author
#' Tinniam V Ganesh
#'
#' @note
#' Maintainer: Tinniam V Ganesh \email{tvganesh.85@gmail.com}
#'
#' @examples
#' \dontrun{
#' # Parse the yaml over
#' }
#'
#' @seealso
#' \code{\link{getBatsmanDetails}}\cr
#' \code{\link{getBowlerWicketDetails}}\cr
#' \code{\link{batsmanDismissals}}\cr
#' \code{\link{getTeamBattingDetails}}\cr
#'
#'
#'
parseYamlOver <- function(match,s,ateam,delivery,meta) {
    team=ball=totalRuns=rnames=batsman=bowler=nonStriker=NULL
    byes=legbyes=noballs=wides=nonBoundary=penalty=runs=NULL
    extras=wicketFielder=wicketKind=wicketPlayerOut=NULL
    gt9=FALSE

    # Create an empty data frame
    overs <- data.frame(ball=character(),team=character(),batsman=character(),
                        bowler=character(),nonStriker=character(),byes=character(),
                        legbyes=character(), noballs=character(), wides=character(),
                        nonBoundary=character(), penalty=character(),
                        runs=character(),extras=character(),totalRuns=character(),
                        wicketFielder=character(), wicketKind=character(),
                        wicketPlayerOut=character(),replacementIn=factor(),
                        replacementOut=character(),replacementReason=character(),replacementRole=character(),
                        date=character(),
                        matchType=character(),
                        overs=character(),venue=character(),team1=character(),team2=character(),
                        winner=character(),result=character())
    # Loop through all deliveries one by one.
    for(i in 1:length(delivery)){
        #cat("i=",i,"\n") # Debug

        # Filter rows based on the delivery(ball) as overset
        # Note if an over has more than 10 deliveries then the deliveries are
        # 1st.0,1.batsman, 1st.0.2.batsman,..., 1st.0.9.batsman, 1st.0.1.batsman.1,
        # 1st.0.1.batsman.2 and so on.
        #filter(match,grepl("1st.0.1.\\D*$",rnames))
        #1      1st.0.1.batsman Shahzaib Hasan
        #2       1st.0.1.bowler      JA Morkel
        #3  1st.0.1.non_striker   Imran Farhat
        #4 1st.0.1.runs.batsman              1
        #5  1st.0.1.runs.extras              0
        #6   1st.0.1.runs.total              1
        #filter(match,grepl("1st.0.1.\\D*.\\d{1}$",rnames))
        #1      1st.0.1.batsman.1   Imran Farhat
        #2       1st.0.1.bowler.1      JA Morkel
        #3  1st.0.1.non_striker.1 Shahzaib Hasan
        #4 1st.0.1.runs.batsman.1              1
        #5  1st.0.1.runs.extras.1              0
        #6   1st.0.1.runs.total.1              1
        # Assume in the worst case there are 15 deliveries
        # Compute delivery
        del <- i %%  15 # Assuming max of 15 deliveries

        # For deliveries 1-9
        if((del >=1) && (del<= 9)){
            pattern = paste(s[i],"\\D*$",sep="")
        } else if(del > 9){ # deliveries 10-15
            # Find increment above 9
            gt9=TRUE
            inc <- del -9
            # Use pattern with suffix .1,.2,.3 etc
            pattern = paste(s[i],"\\D*.","[",inc,"]$",sep="")
        }
        overset <- filter(match,grepl(pattern,rnames))

        #Transpose
        over <-as.data.frame(t(overset))
        # Set column names from 1st row
        names(over) <- lapply(over[1, ], as.character)
        # Remove 1st row
        over <- over[-1, ]
        names(over)=gsub(s[i],"",names(over))

        #If the over had more than 9 balls then the suffix *.1,*.2 have to be removed also
        if(gt9){
            val = paste(".",inc,sep="")
            names(over)=gsub(val,"",names(over))
        }


        #Check the number of deliveries in the over
        d <- dim(over)
        if(d[2] == 0){
            next
        } else if(d[2] >=10){
            print("Greater than equal to 10 cols!")
            #print(d)
            #print(names(over))
            #break
        }

        #Replace extras. with "" for the extras before doing diff - Added 27 Oct 2021
        names(over)=gsub("extras\\.","",names(over))
        cols<-names(over)
        cols1=c("batsman","bowler","non_striker","byes","legbyes","noballs","wides","nonBoundary","penalty",
                "runs.batsman","runs.extras","runs.total","wicket.fielders","wicket.kind","wicket.player_out",
                "replacements.role.in", "replacements.role.out","replacements.role.reason","replacements.role.role")

        # Get the missing columns
        a <- setdiff(cols1,cols)
        if(length(a) != (length(cols1) - length(cols))){
            break
        }

        # Create a dataframe with the missing columns
        over1=data.frame(rbind(a))
        # Set column names
        names(over1) <- lapply(over1[1, ], as.character)
        over1 <- over1[-1, ]
        over1 <- data.frame(lapply(over1, as.character), stringsAsFactors=FALSE)

        over1[1,] <- rep(0,times=length(a))
        newover <- cbind(over,over1)
        newover$ball=gsub("\\\\.","",s[i])
        newover$team = ateam
        newover <- cbind(newover,meta)

        # Convert all columns to character
        newover <- data.frame(lapply(newover, as.character), stringsAsFactors=FALSE)
        # Stack the overs
        overs <- rbind(overs,newover)

    }

    overs

}
