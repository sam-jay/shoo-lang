#!/bin/bash

#justin's latex script
#cd src/
cd test/
TARGET='../latex_compiler.txt'

echo "" > $TARGET

declare -a FILES
#FILES=("shoo.ml" "scanner.mll" "ast.ml" "parser.mly" "sast.ml" "semant.ml" "lift.ml" "codegen.ml" "builtins.c")
FILES=("*")

for f in ${FILES[@]}
do
   echo $f
   echo '\subsubsection{'"$f"'}' >> $TARGET
   if [ ${f: -5} == ".shoo" ]
   then
    echo '\begin{mdframed}[hidealllines=true,backgroundcolor=blue!10]' >> $TARGET
    echo '\begin{lstlisting}'  >> $TARGET  
    cat $f >> $TARGET
   else
    echo '\begin{mdframed}[hidealllines=true,backgroundcolor=green!10]' >> $TARGET
    echo '\begin{lstlisting}'  >> $TARGET  
    cat $f >> $TARGET
   fi
   
   echo '\end{lstlisting}' >> $TARGET
   echo '\end{mdframed}' >> $TARGET
done


