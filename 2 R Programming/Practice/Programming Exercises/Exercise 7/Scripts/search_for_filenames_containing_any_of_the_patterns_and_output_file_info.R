search_for_filenames_containing_any_of_the_patterns_and_output_file_info <-
        function(directory, search.strings) {
        # directory is an absolute path(full path) or a path relative to the R 
        # working directory folder to be searched. If want to search the R working
        # directory itself, can set directory = "." with the line of code:
        # directory <- "." or could set directory to be the full path to the R 
        # working directory
        
        # search.strings is a character string vector
        
        # Return a data frame of the file names (not including folders) in
        # directory that contain ANY of the entries of the search.strings vector
        # somewhere in their file name, or at the beginning of the file name, or
        # at the end of the file name, if so specified).
        # The search will be case insensitive (treats lower case and upper case 
        # letters as the same.
        
        # The first step is to initialize the filenames vector:
        # filenames <- character(0)
                
        # In a for loop, use R's list.files function to list all the files (file
        # names) matching entries of search.strings, one by one, eliminating 
        # names of folders, and appending them to filenames
        
        # Use the unique function to elimitane duplicates (keep only one copy of
        # each file name)
        
        # For the files whose names contain any of the character strings in
        # search strings, use R's file.info function to get the file size and 
        # last modification time as in the previous function.
        
        # The output data frame will contain the file size and the last 
        # modification time for each file that has any member of search.strings
        # somewhere in its file name (or at the beginning or at the end of the
        # file name (or at the beginning or at the end of the file name, if so
        # specified)
        
        # We will get the file names without the folder path leading to the
        # files included in the name
         
        # check that search.strings is a non-empty character vector
 
        num_strings <- length(search.strings)
        if(num_strings < 1) stop("no entries in search.strings")
        if(!is.character(search.strings)) 
                stop("search.strings is not a character vector")
        
        # We will return a data frame whose first column contains the files in 
        # directory that contain any character string in the search.strings vector 
        # somewhere in their name. The second column will contain the last time 
        # (and date) the file was modified, and the third column will be the file
        # size (in bytes)
        
        # The first step is to initialize the filenames vector
        
        filenames <- character(0)
        
        # Then, in a for loop, for each entry S of search.strings, use list.files
        # to get the vector V the file names in directory that contain S in their
        # file name and append V to filenames (after eliminating any folder names).
        
        # Do not include the path to the file in the file name.
        
        # To eliminate names of any folders (directories) that ar in V: 
        # do paste(directory, "/", V, sep = "") to get the file names including
        # either the relative path from the R working directory or the absolute
        # path (depending on what directory is); use these names in the R
        # dir.exists to check for folder (directory) names in filenames
        
        for (k in 1: num_strings) {
                V <- list.files(directory, pattern = search.strings[k],
                                full.names = FALSE, ignore.case = TRUE)
                # exclude directory names from V (we need to do this since we 
                # are "adding" the file names in V to the filenames and want 
                # only the file names, not folder names. If V is empty
                # (character(0)) then skip this
                
                if (length(V) > 0) {
                        V <- V[!dir.exists(paste(directory, "/", V, sep = ""))]
                        filenames <- c(filanames, V)
                }
                
        num_files <- length(filenames)
        if (num_files == 0) {
                        print("no files contain any of the search strings")
                        return("no files contain any of the search strings")
                }
        }
        
        filenames <- unique(filenames)
        
        # If got to here, at least 1 file has a character string in search.strings
        # in its name, so get the information on these file(s) into a data frame
        
        ## Get the data frame to be output
        
        colnamesdf <- c("file.name", "modif.date", "size.in.bytes")
        fname <- character(0)
        fmtime <- character(0)
        fsize <- numeric(0)
        
        for (k in 1:num_files) {
                
                file_info <- file.info(paste(directory, "/", filenames[k], sep = ""))
                
                fname <- c(fname, filenames[k])
                fmtime <- c(fmtime, as.character(file_info$mtime))
                fsize <- c(fsize, as.character(file_info$size))
                
        }
        
        df <- data.frame(fname, fmtime, fsize, stringsAsFactors = FALSE)
        colnames(df) <- colnamesdf
        
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