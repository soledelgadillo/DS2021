search_for_filenames_containing_multiple_patterns_and_output_file_info <-
        function(directory, search.strings) {
                
        # directory is an absolute path (full path) or a path relative to the R
        # working directory to the folder to be searched. If want to search the
        # R working directory itself, can set a directory = "."
        # or could set directory to be the full path to the R working directory.
        
        # search.strings is a character string vector
        
        # Return a data frame of the file names (not including folders) in 
        # directory that contain ALL the entries of the search.strings vector
        # somewhere in their file name, the search will be case insensitive
        # (treats lower case and upper case letters as the same)
        
        # Use R's list.files function to list all the files (file names) in directory
        # that contain search.strings[1] in their name, then eliminate names of
        # folders, then use R's grep function to find out, for each file name, 
        # whether or not each character string in search.strings appears in the
        # file name.
        # For the files whose names contain all the character strings in search.strings,
        # use R's file.info function to get the file size and last modification time.
                
        # The output data frame will contain the file size and the last modification
        # time for each file that has each member of search.strings somewhere in
        # its file name.
        
        # We will get the file names without the folder path leading to the files
        # included in the name.
        
        #check that the search.strings is non-empty character vector
                
        num_strings <- length(search.strings)
        if(num_strings < 1) stop("no entries in search.strings")
        if(!is.character(search.strings)) stop("search.strings is not a character vector")
        
        # We will return a data frame whose first column contains the files in
        # directory that contain each character string in the search.strings 
        # vector somewhere in their name, and the third column will be the file  
        # size (in bytes).
                
        # The first step is to get all the file names in directory that contain
        # the first string in search.strings somewhere in their file name; do
        # not include the path to the file in the file name
                
        filenames <- list.files(directory, pattern = search.strings[1],
                                        full.names = FALSE, ignore.case = TRUE)
        
        # If at any point there are no files then return a message that there 
        # are none
                
        if(length(filenames) == 0) {
                print("no files contain all the search strings")
                return("no files contain all the search strings")
        }
                
        # Eliminate names of any folders (directories) that are in the filenames
        # character vector, paste(directory, "/", filenames, sep = "") will get the 
        # filenames including either the relative path from the R working directory
        # or the absolute path (depending on what directory is), use these names 
        # in the R dir.exists function to check for folder (directory) names in 
        # filenames
        filenames <- filenames[!dir.exists(paste(directory, "/", filenames, 
                                                        sep = ""))]
        # exclude directory names
                
        if(length(filenames) == 0) {
                print("no files contain all the search strings")
                return("no files contain all the search strings")
        }
                
        # if there is more than 1 search string, search over the rest of them
        if(num_strings > 1) {
                for (k in 2 : num_strings) {
                        filenames <- grep(search.strings[k], filenames,
                                          ignore.case = TRUE, value = TRUE)
                        # grep returns the subset of filenames that contain
                        # search.strings[k] in the file name, if filenames is an 
                        # empty character vector, grep will just return an empty
                        # character vector
                }
        }
        num_files <- length(filenames)
        if(num_files == 0) {
                print("no files contain all the search strings")
                return("no files contain all the search strings")
        }
        
        # If got to here, at least 1 file has all the character strings in 
        # search.strings in its names, so get the information on these file(s)
        # into a data frame.
        
        ## Get the data frame to be output
        
        colnamesdf <- c("file.name", "modif.date", "size.in.bytes")
        fname <- character(0)
        fmtime <- character(0)
        fsize <- numeric(0)
        
        for (k in 1:num_files) {
                
                file_info <- file_info <- file.info(paste(directory, "/", 
                                filenames[k], sep = ""))
                
                fname <- c(fname, filenames[k])
                fmtime <- c(fmtime, as.character(file_info$mtime))
                fsize <- c(fsize, as.character(file_info$size))
                
        }
        
        df <- data.frame(fname, fmtime, fsize, stringsAsFactors = FALSE)
        colnames(df) <- colnamesdf
        
        ##df <- data.frame()
        ##colnamesdf <- c("file.name", "modif.date", "size.in.bytes")
        
        ##for (k in 1:num_file) {
        ##        file_info <- file.info(paste(directory, "/", 
        ##                                     filenames[k], sep = ""))
                # needed to include a path to the file so file.info can locate it
        ##        dfk <- data.frame(filenames[k], as.character(file_info$mtime, 
        ##                          file_info$size), stringsAsFactors = FALSE)
        ##        colnames(dfk) <- colnamesdf
        
        ##        df <- rbind(df, dfk) #the rbind command stacks dfk below df
        ##}

        # Get the information into a character matrix, the will convert it to a 
        # data frame.
                
        #file.matrix <- matrix("0", nrow = num_files, ncol = 3)
        #dfcolnames <- c("file.name", "modif.date", "size.in.bytes")
                
        #for (k in 1:num_files) {
        #        file_info <- file.info(paste(directory, "/", 
        #                                     filenames[k], sep = ""))
        # needed to include the path to the file so file.info can locate it
        #        file.matrix[k, 1] <- filenames[k]
        #        file.matrix[k, 2] <- as.character(file_info$mtime)
        #        file.matrix[k, 3] <- as.character(file_info$size)
        #}
        #df <- data.frame(file.matrix, stringsAsFactors = FALSE)
        #colnames(df) <- dfcolnames
        
        ## Finished getting the data frame to be output
                
        # Write the data frame out to a tab delimited text file called 
        # scrlisting.txt in directory (i.e., in the folder specified by 
        # the argument directory this function was called with).
        
        output_filename <- paste(directory, "/", "scrlisting.txt", sep = "")
                
        # One can rename this "scratch file" as desired after viewing it
        # (best viewed in Excel). 
        
        write.table(df, file = output_filename, append = FALSE,
                    quote = FALSE, sep = "\t", row.names = FALSE, 
                    col.names = TRUE)
        
        # This call to write.table will write out a data frame as one would
        # usually want; it specifies the column separator to be a tab.
                
        return(df)
                
}