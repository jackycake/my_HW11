copy_dir<-function(dirname, prefix)
{
  my_wd<-getwd()

  #rename directory name
  my_basename_wd <- basename(my_wd)
  new_dir_name1<-paste(prefix, my_basename_wd ,sep="")
  #make new directory
  system("mkdir new_dir_name1")
  
  #rename files within directory
  files<-list.files()
  file1<-files[1]
  file2<-files[2]
  
  system ("cp file1 new_dir_name1")
  system("cp file2 new_dir_name1")
  
  setwd("new_dir_name1/")  
  files<-list.files()
  file1<-files[1]
  file2<-files[2]
  new_filename_1<-paste(prefix, files[1],sep="")
  new_filename_2<-paste(prefix, files[2],sep="")
  system("mv file1 new_filename_1")
  system("mv file2 new_filename_2")
}

copy_dir2<-function(dirname,prefix)
{
  my_wd<-getwd()
  basename_wd<-basename(my_wd)
  new_dir<-dir.create(paste(prefix, basename_wd, sep = ""))

  files<-list.files()
  file1<-files[1]
  file2<-files[2]

  file.copy(files, "plus_jacklyn_problem1/")
  
  setwd("plus_jacklyn_problem1/")
  new_file1 <- (paste(prefix, file1, sep = ""))
  new_file2 <- (paste(prefix, file2, sep = ""))
  file.rename(file1, new_file1) 
  file.rename(file2, new_file2)
}

