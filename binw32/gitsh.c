/* gitsh for msys  */
/* 主要解决 emacs与git合作时，提交日志中含中文，乱码问题 */
/* gcc -o gitsh.exe gitsh.c */
/* (when (equal system-type 'windows-nt) (setq vc-git-program (expand-file-name "~/.emacs.d/bin/gitsh.exe"))) */
#include <stdio.h>
#include <stdlib.h>
// #include <direct.h>
#include <string.h>
// .git/COMMIT_EDITMSG
int get_commit_editmsg_path(char** Path){
  char*  tempPath = getenv("TEMP");
  char* path=malloc(strlen(tempPath)+strlen("\\COMMIT_EDITMSG")+1);
  strcpy(path,tempPath);
  strcat(path,"\\COMMIT_EDITMSG");
  /* printf ("%s\n",path); */
  *Path= path;
  return 0;
}
int main(int argc, char* argv[])
{
  int i;
  char* cmd=(char*)malloc(1024*1024*10)  ;        /* 10k */
  char* buf=(char*)malloc(1024*1024*10)  ;        /* 10k */
  char* Path=NULL;
  sprintf(cmd,"%s","git");
  FILE *msgF;
  /* FILE *log; */
  /* log= fopen( "d:/tmp/log.txt", "a+"); */

  if(argc>2&& strcmp(argv[1],"commit")==0&&strcmp(argv[2],"-m")==0){
    get_commit_editmsg_path(&Path);

    msgF= fopen( Path, "w+");
    fprintf(msgF,"%s",argv[3]); /* -m "msg"   ,log msg  */
    fclose(msgF);

    sprintf(buf,"%s commit --file=\"%s\" ",cmd,Path);
    memcpy(cmd,buf,strlen(buf));

    for (i = 4; i < argc; i++){ // i = 4 , skip "-m" "msg"
      sprintf(buf,"%s \"%s\" ",cmd,argv[i]);
      memcpy(cmd,buf,strlen(buf));
    }
  }else{
    for (i = 1; i < argc; i++){
      sprintf(buf,"%s \"%s\" ",cmd,argv[i]);
      memcpy(cmd,buf,strlen(buf));
    }
  }
  system(cmd );
  /* fprintf(log,"%s\n",cmd); */
  if(Path!=NULL){
    free(Path);
  }
  free(cmd);
  free(buf);

  return 0;
}
