## subset of points more than N m from each other
## code that is ran is at the bottom


## import the data
## data MUST be a distance matrix between points with a header of point names and a column of point names that are identical
card_distance_pivot = read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Point and Dist Data/card_waypoints_all_utm_distance_pivot.csv")
rownames(card_distance_pivot) = card_distance_pivot[,1] ## assign the row names
card_distance_pivot = card_distance_pivot[,-1] ## remove the original row names
colnames(card_distance_pivot) = rownames(card_distance_pivot) ## assign the column names

## function to create a subset of the points that are above a minimum distance threshhold
## begin function
points.subset = function(data, N = 100, DropMode = "rand") { 
  # data is the distance matrix uploaded
  # N is the threshhold distance value
  # modes possible under DropMode = "rand" "mode" "min"
  # Dropmode chooses whether you want to drop the points randomly (rand),
  # by whichever has the highest mode and THEN randomly (mode),
  # or by whichever has the lowest minimum (min) 

  # print("start") ## prints to show start of function - used for debugging
  starttime = Sys.time() ## stores the system time 
  
  ## set up blank data frames and lists for later use
  card_dist_reduced = data ## set the distance matrix ready to be reduced
  not_pair = data.frame()
  not_pair_rev = data.frame()
  not_pair_both = data.frame()
  okay = c()
  okay_pair = data.frame()
  not = c()
  badlist = c()
  minlist = c()
  replist = c()
  ## begin while/repeat function
  while (min(card_dist_reduced,na.rm=TRUE)<N) {
  ## while the minimum distance on the distance matrix is less than the threshhold, repeat
    repeat {
      #print("repeat") ## used for debugging
      ## reset all the variables used
      not_pair_rev = data.frame()
      not_pair_both = data.frame()
      okay = c() ## list of pairs of points that have a distnace above the threshhold
      not = c() ## list of pairs of points that have a distance below the threshhold
      ## okay and not are formatted as a list of strings with the format "point i, point j"
      okay_pair = data.frame() ## dataframe of pairs of points that have a distance above the threshhold
      not_pair = data.frame() ## dataframe of pairs of points that have a distance below the threshhold
      ## okay_pair and not_pair are dataframes with two columns 
      ## where column 1 is the point i in a pair and column 2 is the point j in a pair
      #j = c() ## debugging
      #i = c() ## debugging
      #k = c() ## debugging
      #print("first for loop") ## debugging
      
      ## begin for loops to determine the distances between point i and point j
      ## for each row i and each column j
      for (i in 1:length(card_dist_reduced)) {
        for (j in 1:length(card_dist_reduced)) {
          if (as.numeric(rownames(card_dist_reduced[i,])) >= as.numeric(colnames(card_dist_reduced[j]))) {
            ## if the numbers in i and j are equal to each other
            ## which indicates that the distance comparison is between the same point
            ## do nothing
            okay = c(okay)
            not = c(not)
          }
          else if (card_dist_reduced[i,j] >= N) {
            ## if the distance between points i and j is greater than or equal to 100
            ## add the concatenated pair of point i and j to the okay list
            okay = c(okay,paste(as.numeric(rownames(card_dist_reduced[i,])),",",as.numeric(colnames(card_dist_reduced[j])),sep=""))
            ## add the pair of points to the two columns of the okay_pair dataframe
            okay_pair = rbind(okay_pair,c(as.numeric(rownames(card_dist_reduced[i,])),as.numeric(colnames(card_dist_reduced[j]))))
          }
          else if (card_dist_reduced[i,j] < N) {
            ## if the distance between points i and j is less than 100
            ## add the concatenated pair of point i and j to the not list
            not = c(not,paste(as.numeric(rownames(card_dist_reduced[i,])),",",as.numeric(colnames(card_dist_reduced[j])),sep=""))
            ## ad the pair of points to the two columns of the not_pair dataframe
            not_pair = rbind(not_pair,c(as.numeric(rownames(card_dist_reduced[i,])),as.numeric(colnames(card_dist_reduced[j]))))
          }
          else {
            print("ERROR")
            ## if this for loop fails print an error
          }
        }
      }
      
      ## make all of the sets of points on the okay_pair and not_pair unique
      okay_pair = unique(okay_pair)
      not_pair = unique(not_pair)
      if (length(not_pair) == 0) {
        break()
        print("no more pairs")
        ## if the length of not_pair is zero, break the repeat
        ## this indicates that there are no pairs that are below the threshhold distance
      }

      ## finding the frequencies to determine the number of pairs points are in
      ## this is applicable to the "mode" part of the function
      #print("finding the frequencies") ## debugging
      
      ## renaming columns for merging purposes
      unname(not_pair)
      colnames(not_pair) =c("one","two")
      
      ## reverse the not_pair dataframe so that points are stored as j,i not i,j
      ## this is so that we can extract which points are in which pairs
      ## and which points are in pairs most frequently
      not_pair_rev = as.data.frame(cbind(not_pair[,2],not_pair[,1]))
      unname(not_pair_rev)
      colnames(not_pair_rev) =c("one","two")
      
      ## merge not_pair with not_pair_rev 
      ## this merges pairs i,j and j,i so that i and j are both listed in one column
      not_pair_both = rbind(not_pair,not_pair_rev)
      
      ## sort the list so that points are in numerical order
      ## and putting this list into a usable data frame to populate frequencies/modes
      ## this also gives us the number of times a number appears in the table via the table() function
      sorted = as.data.frame(sort(table(not_pair_both[1]),decreasing=TRUE))
      sorted = cbind(as.numeric(rownames(sorted)),sorted)
      sorted = as.data.frame(sorted)
      colnames(sorted) = c("Point","Freq")
      rownames(sorted) = seq(1:length(sorted[,1]))
      print(c("length sorted",length(sorted[,1])))
      
      ## running the loop to find the modes
      #print("mode loop") ## debugging
      repeats = c() ## repeats is the points that have a mode more than 1
      if ((as.numeric(sorted$Freq[1])) != (as.numeric(sorted$Freq[2]))) {
        repeats = 1
      } else {
        for (k in 1:length(sorted[,1])) {
          if ((as.numeric(sorted$Freq[1])) == (as.numeric(sorted$Freq[k])))  {
            repeats = k
          } else {
            repeats = repeats
          }
        }
      }
      print(c("repeats",repeats)) ## prints the list of points that are repeats
      replist = c(replist,repeats) ## adds the number of repeats to a list to track the number of repeats
      if (DropMode == "mode") {
        ## removing the modes
        #print("mode removal")
        bad = as.numeric(sorted$Point[1:repeats]) ## bad is a list of the repeated points
        if (repeats != 1) {
          bad = sample(bad,1)
          print("rand")
          ## if there are any numbers that have a mode greater than 1, randomly select from them
          ## and add that point to the bad list
        } else {
          bad = bad
          ## otherwise leave the badlist as is
        }
      } else if (DropMode == "rand") {
        bad = sample(as.numeric(sorted$Point),1)
        ## if Dropmode is set to random, randomly sample one of the points
        ## and add that to the bad list
      } else if (DropMode == "min") {
        mintable = data.frame()
        ## if dropmode is set to min, remove one of the points in the pair
        ## that has the minimum distance between them
        ## set up a table of the minimum distances between points
        for (i in 1:length(sorted$Point)){
          minnum = which(names(card_dist_reduced)==sorted$Point[i])
          mintable[i,1] = names(card_dist_reduced[minnum])
          mintable[i,2] = min(card_dist_reduced[minnum],na.rm=TRUE)
        }
        ## add one of the points to the bad list
        colnames(mintable) = c("Point","Min")
        ## if the minimum distance is the same between the first point and the second point
        ## randomly sample between those two points and add to the bad list
        ## otherwise just take the first point and add to the bad list
        mintable = mintable[order(mintable$Min),]
        if (mintable$Min[1] == mintable$Min[2]) {
          bad = as.numeric(sample(c(mintable$Point[1],mintable$Point[2]),1))
        } else {
          bad = as.numeric(mintable$Point[1])
        }
      } else {
        print("Invalid Type")
        ## if DropMode is given and invalid type, throw this error
      }
      ## print the point that is designated as bad
      ## this point will be removed
      print(c("bad",bad))
      ## get the column number of the bad point
      badnum = which(names(card_dist_reduced)==bad)
      print(c("badnum",badnum))
      ## remove the column that is associated with the bad point
      ## remove the row that is associated with the bad point
      card_dist_reduced = card_dist_reduced[,-badnum]
      card_dist_reduced = card_dist_reduced[-badnum,]
      ## print the points that still remain
      ## print the minimum distance between the points that are left on the table
      print(names(card_dist_reduced))
      print(c("min",min(card_dist_reduced,na.rm=TRUE)))
      ## add the bad point to a list of points showing which points were removed when
      badlist = c(badlist,as.numeric(bad))
      ## add the minimum to a list showing what the minimum was after each rep
      minlist = c(minlist,as.numeric(min(card_dist_reduced,na.rm=TRUE)))
    } 
  }
  fully_reduced = names(card_dist_reduced) ## list of the points that are still in the reduced data set
  print("FULLY REDUCED") ## prints that the loop is complete
  print(fully_reduced) ## prints the list of points that are in the reduced data set
  ## plot to check progress
  par(mfrow=c(3,1))
  plot(badlist) ## plot, in order, the numbers that are removed
  plot(minlist) ## plot, in order, the minimum distance on the matrix
  plot(replist) ## plot, in order, the number of repeats of the modes
  endtime = Sys.time() ## stores the system time
  result = list(removed=badlist,modes=replist,minimums=minlist,subset=fully_reduced)
  ## returns the result, so name$removed gives the badlist, name$modes gives the modes, name$minimums gives the minimums
  ## and most importantly name$subset gives the list of the fully reduced points
  return(result)
  print("start time",starttime,"end time",endtime,"elapsed",(endtime-starttime)) ## tells how much time elapsed
}
## end function

## function to keep multiple subsets of points that are above the threshhold
## that are also the longest length
## begin function
keep.longest.list = function(dat,reps=100,n=100,Drop="rand") {
  ## this function will keep the longest lists
  ## dat must be a dataframe distance matrix
  ## reps is the number of lists to iterate through
  ## n is the distance threshhold
  ## Drop is the same as DropMode and must be either "rand" "mode" or "min"

  ## rename variables for use passing to points.subset
  data1 = dat
  N1 = n
  DropMode1 = Drop
  
  ## set up blank lists and data frames for later
  best = data.frame()
  bestlist = c()
  length=c()
  startbest = Sys.time() ## store the start time of all the reps
  for (i in 1:reps) {
    ## for a number of iterations of length "reps"
    print(c("round",i,"of",reps)) ## print the round of reps
    x = points.subset(data=data1,N=N1,DropMode=DropMode1) ## run the points.subset function
    if (length(x$subset) > max(bestlist)) {
      ## if the points.subset function returns a reduced points list with length greater than the original list of points
      ## keep that reduced points list in a dataframe
      best = x$subset
      length = c(length,length(best)) ## add the length of the longest list to a running tally
      bestlist = c(bestlist,as.numeric(length(x$subset))) ## adds the length of the output of the function to a running tally
    } else if (length(x$subset) < max(bestlist)) {
      ## if the function returns a reduced list with length less than the original list of points
      ## do not keep the newly returned list, output the same list
      best = best
      length = c(length,length(best)) ## add the length of the longest list to a running tally
      bestlist = c(bestlist,as.numeric(length(x$subset))) ## adds the length of the output of the function to a running tally
    } else if (length(x$subset) == max(bestlist)) {
      ## if the function returns a list with length exactly the same as the original list
      ## concatenate the old and new lists together and keep both
      best = cbind(best,x$subset)
      length = c(length,length(best)) ## add the length of the longest list to a running tally
      bestlist = c(bestlist,as.numeric(length(x$subset))) ## adds the length of the output of the function to a running tally
    } 
    ## plot the lists to keep track of the functions
    par(mfrow=c(2,1))
    plot(bestlist)
    plot(length)
  } 
  endbest = Sys.time()
  result = list(best1=best,bestlist1=bestlist,length1=length)
  return(result)
  print(c("start",startbest,"end",endbest,"elapsed",endbest-startbest))
}
## end function

## function to keep all subsets of points above threshhold
keep.all.lists = function(dat,reps=100,n=100,Drop="rand") {
  ## this function will keep all lists, not just longest
  ## dat must be a dataframe distance matrix
  ## reps is the number of lists to iterate through
  ## n is the distance threshhold
  ## Drop is the same as DropMode and must be either "rand" "mode" or "min"
  
  ## rename variables for use passing to points.subset
  data1 = dat
  N1 = n
  DropMode1 = Drop
  
  ## set up blank lists and data frames for later
  best = c()
  bestlist = c()
  length=c()
  startbest = Sys.time() ## store the start time of all the reps
  for (i in 1:reps) {
    ## for a number of iterations of length "reps"
    print(c("round",i,"of",reps)) ## print the round of reps
    x = points.subset(data=data1,N=N1,DropMode=DropMode1) ## run the points.subset function
    ## keep all reps in a matrix, populating with NA if needed
    best[[i]] = x$subset
    #best = list(best,x$subset)
    length = c(length,length(best)) ## add the length of the longest list to a running tally
    bestlist = c(bestlist,as.numeric(length(x$subset))) ## adds the length of the output of the function to a running tally
    } 
    ## plot the lists to keep track of the functions
    par(mfrow=c(2,1))
    plot(bestlist)
    plot(length)
  endbest = Sys.time()
  result = list(best1=best,bestlist1=bestlist,length1=length)
  return(result)
  print(c("start",startbest,"end",endbest,"elapsed",endbest-startbest))
  print("DONE")
}
## end function

## function to cull lists down to minimum value
## begin function
## lists must be output from keep.all.lists
cull.lists=function(lists) {
  ## lists is output of keep.all.lists
  x = c()
  min = min(lists$bestlist1) ## establishes the minimum value to cull to
  reps = length(lists$bestlist1) ## establishes the number of lists to rep through
  for (i in 1:reps) {
    x[[i]] = sort(sample(x=lists$best1[[i]],size=min))
    ## populates a new list, randomly samples from old list
    ## new list is length of the minumum value
  }
  result = list(culled=x)
  return(result)
}
## end function

## function to remove non-unique lists
## begin function
## lists must be output from keep.all.lists or cull.lists
remove.dup.lists = function(lists) {
  ## list is output of cull.lists or keep.all.lists
  x = c()
  x = lists[-which(duplicated(testlist))]
  ## removes any list in which the list is not unique
  result = list(nodups=x)
  return(result)
}
## end function




## code to be run
all_100 = keep.all.lists(card_distance_pivot,reps=100,n=100,Drop="rand")
all_100_nodup = remove.dup.lists(all_100)
all_100_nodup_cull = cull.lists=(all_100_nodup)

all_150 = keep.all.lists(card_distance_pivot,reps=100,n=150,Drop="rand")
all_150_nodup = remove.dup.lists(all_100)
all_150_nodup_cull = cull.lists=(all_100_nodup)

all_200 = keep.all.lists(card_distance_pivot,reps=100,n=200,Drop="rand")
all_200_nodup = remove.dup.lists(all_100)
all_200_nodup_cull = cull.lists=(all_100_nodup)