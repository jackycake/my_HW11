copy_dir <- function(dirname, prefix)
{
  #get current directory name
  my_wd<-getwd()
  my_basename_wd <- basename(my_wd)
  
  #rename directory name to add prefix
  #and set new directory as a system environment
  Sys.setenv(banana=paste(prefix, my_basename_wd,sep=""))
  
  #make new directory
  system("mkdir $banana")
  
  #get files in current directory
  files<-list.files()
  file1<-files[1]
  file2<-files[2]
  
  #create new filenames and set as a system environment
  Sys.setenv(apple = paste(prefix, files[1],sep=""))
  Sys.setenv(orange = paste(prefix, files[2],sep=""))
  
  #copy file information to newly created files
  system("cp file1 $apple")
  system("cp file2 $orange")
  
  #move new files to new directory
  system ("mv $apple $banana")
  system("mv $orange $banana")
  
}

copy_dir2 <- function(dirname,prefix)
{
  my_wd<-getwd()
  basename_wd<-basename(my_wd)
  dir.create(paste(prefix, basename_wd, sep = ""))
  directory <- list.dirs(path = my_wd, full.names = TRUE, recursive = TRUE)
  directory1 <- directory[58]

  files<-list.files()
  file1<-files[1]
  file2<-files[2]
  
  file.copy(files, directory1)
  setwd(directory1)
  new_file1 <- (paste(prefix, file1, sep = ""))
  new_file2 <- (paste(prefix, file2, sep = ""))
  file.rename(file1, new_file1) 
  file.rename(file2, new_file2)
}
