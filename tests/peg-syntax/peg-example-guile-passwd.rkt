#lang peg
passwd <-- entry* !. ;
entry <-- login C pass C uid C gid C nameORcomment C homedir C shell NL* ;
login <-- text ;
pass <-- text ;
uid <-- [0-9]* ;
gid <-- [0-9]* ;
nameORcomment <-- text ;
homedir <-- path ;
shell <-- path ;
path <-- (SLASH pathELEMENT)* ;
pathELEMENT <-- (!NL !C  !'/' .)* ;
text <- (!NL !C  .)* ;
C < ':' ;
NL < [\n] ;
SLASH < '/' ;
