run_cpt<-function (dep, trim, x_predictors, x_modes,y_predicting,  y_modes, cca_modes, dir_output, transformation){
  cmd <- "@echo off

  (
  echo 611
  echo 545
  echo 1
  echo %X_PREDICTORS%
  echo 1
  echo %X_Modes%
  echo 2
  echo %Y_PREDICTING%
  echo 90
  echo -90
  echo 0
  echo 359
  echo 1
  echo %Y_Modes%
  echo 1
  echo %CCA_Modes%
  echo 9
  echo 1
  echo 554
  echo 2
  echo %Transformation%
  echo 112
  echo %RESULT_GI%
  echo 311
  echo 451
  echo 452
  echo 454
  echo 455
  echo 413
  echo 1
  echo %Pearson%
  echo 413
  echo 3
  echo %2AFC%
  echo 111
  echo 401
  echo %Canonical%
  echo 501
  echo %RESULT_PROB%
  echo 0
  echo 0
  ) | CPT_batch.exe"
  
  cmd<-gsub("%X_PREDICTORS%",x_predictors,cmd)
  cmd<-gsub("%X_Modes%",x_modes,cmd)
  cmd<-gsub("%Y_PREDICTING%",y_predicting,cmd)
  cmd<-gsub("%Y_Modes%",y_modes,cmd)
  cmd<-gsub("%CCA_Modes%",cca_modes,cmd)
  cmd<-gsub("%RESULT_GI%",paste0(dir_output,"/GoodnessIndex_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%Pearson%",paste0(dir_output,"/Pearsons_correlation_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%2AFC%",paste0(dir_output,"/k_2AFC_Score_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%Canonical%",paste0(dir_output,"/CanonicalCorrelations_",dep, "_", trim,".txt"),cmd)
  cmd<-gsub("%RESULT_PROB%",paste0(dir_output,"/ForecastProbabilities_",dep, "_", trim,".txt"),cmd)
  
  
  if(transformation == "no"){
    cmd<-gsub("%Transformation%"," ",cmd)} else if(transformation != "no"){
      cmd<-gsub("%Transformation%","541",cmd)}
  
  #cmd <- gsub("/","\", cmd)
  write(cmd,"C:/Users/AESQUIVEL/Desktop/outputs/text.bat")
  #system(cmd, ignore.stdout = T, show.output.on.console = F)
  system("C:/Users/AESQUIVEL/Desktop/outputs/text.bat", ignore.stdout = T, show.output.on.console = F)
}






run_cpt(dep = "cordoba", trim = "no", x_predictors = "C:/Users/AESQUIVEL/Desktop/s.txt",
          x_modes = 5, y_predicting = "C:/Users/AESQUIVEL/Google Drive/Exp_2/dep/precip_cordoba.txt",
          y_modes = 5, cca_modes = 3, dir_output = "C:/Users/AESQUIVEL/Desktop/outputs", transformation = "no")


