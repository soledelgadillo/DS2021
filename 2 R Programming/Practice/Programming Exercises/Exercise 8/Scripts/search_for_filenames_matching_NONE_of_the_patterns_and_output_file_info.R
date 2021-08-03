search_for_filenames_matching_NONE_of_the_patterns_and_output_file_info <-
        function(directory, search.strings) {

        # directory is an absolute path(full path) or a path relative to the R 
        # working directory folder to be searched. If want to search the R working
        # directory itself, can set directory = "." with the line of code:
        # directory <- "." or could set directory to be the full path to the R 
        # working directory
        
        # search.strings is a character string vector
        
        # Return a vector of the file names (not including folders) in directory
        # that contain NONE of the entries of the search.strings vector in their
        # file name, the search will be case insensitive (treats lower case and 
        # upper case letters as the same).
        # Since this is mainly an exercise in using the %in% function, just
        # return the vector of file names(and don't bother with getting the last
        # modification data or file size).
        
        # Use R's list.files function to list all the files (file names) in 
        # directory then eliminate names of folders; put the result in Vall
        
        # Then use the previous function
        # search_for_filenames_matching_any_of the patterns_and_output_file_info
        # to find all the files in directory that match some entry in search.strings
        # and put the vector of these file names in Vany
        
        # Then eliminate from Vall any entries that are in Vany using the %in% 
        # function and local negation (!)
        
        # We will get the file names without the folder path leading to the files
        # included in the name.
        
        # Check that search.strings is a non-empty character vector
        
        num_strings <- length(search.strings)
        if(num_strings < 1) stop("no entries in search.strings")
        if(!is.character(search.strings)) 
                stop("search.strings is not a character vector")
        
        Vall <- list.files(directory, full.names = FALSE, ignore.case = TRUE)
        num_files <- length(Vall)
        if (num_files == 0) {
                print("no files in directory")
                return("no files in directory")
        }
        
        # exclude directory names from Vall
        Vall <- Vall[!dir.exists(paste(directory, "/", Vall, sep = ""))]
        num_files <- length(Vall)
        if (num_files == 0) {
                print("no files in directory")
                return("no files in directory")
        }
        
        # now get Vany
        df.Vany <- search_for_filenames_containing_any_of_the_patterns_and_output_file_info(directory, search.strings)
        
        # nee to handle the cae that NO file names were a match for any of the
        # search stringsin which  case df.Vany isthe character string: 
        # "no files contain any of the search strings" rather than a data frame
        
        if (class(df.Vany) = "character") {
                Vany <- character(0)
                } else {
                Vany <- df.Vany$file.name
                }
        # When programming, one should always be sure the function handles 
        # "extreme cases", here that would be when NONE of the file names in 
        # directory match some entry in search.strings, and when ALL of the file
        # names match search.strings
        
        # eliminate names in Vany from Vall
        
        Vlogical <- !(Vall %in% Vany) 
        # we want entries in Vall that are NOT in Vany no need to use !
        filenames >- Vall[Vlogical] 
        # the desired file names (names NOT in Vany)
        
        num_files <- length(filenames)
        if (num_files == 0) {
                print("all files contain an entry of search strings")
                return("all files contain an entry of search strings")
        }
        
        # If got to here, at least 1 file has no entry of search.strings in its 
        # name 
        
        return(filenames) 
        # here we are just returning the file nimae without other information on them
}