run_cpt<-function (x_predictors,y_predicting,dir_output){
  cmd <- "@echo off

  (
  echo 611
  echo 1
  echo %X_PREDICTORS%
  echo 90
  echo -90
  echo 0
  echo 359
  echo 1
  echo 5
  echo 2
  echo %Y_PREDICTING%
  echo 90
  echo -90
  echo 0
  echo 359
  echo 1
  echo 5
  echo 1
  echo 3
  echo 112
  echo %RESULT_GI%
  echo 311
  echo 451
  echo 452
  echo 454
  echo 455
  echo 111
  echo 501
  echo %RESULT_PROB%
  echo 0
  ) | CPT_batch.exe"
  
  cmd<-gsub("%X_PREDICTORS%",x_predictors,cmd)
  cmd<-gsub("%Y_PREDICTING%",y_predicting,cmd)
  cmd<-gsub("%RESULT_GI%",paste0(dir_output,"/GI.txt"),cmd)
  cmd<-gsub("%RESULT_PROB%",paste0(dir_output,"/prob.txt"),cmd)
  cmd<-gsub("/","\\",cmd)
  write(cmd,"D:/ToBackup/Google Docs/USAID/Installs/cpt/text.bat")
  system(cmd, ignore.stdout = T, show.output.on.console = F)
}

run_cpt("D:/ToBackup/Google Docs/USAID/Installs/cpt/DEF_J.tsv","D:/ToBackup/Google Docs/USAID/Installs/cpt/dep/precip_cordoba.txt","D:/ToBackup/Google Docs/USAID/Installs/cpt/result")
