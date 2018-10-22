#!/bin/sh

TARGET='../latex_compiler.txt'

echo "" > $TARGET

declare -a FILES=("grapl.ml" "scanner.mll" "ast.ml" "parser.mly" "sast.ml" "semant.ml" "hindley_milner.ml" "lift.ml" "codegen.ml" "graph.c")


for f in ${FILES[@]}
do
   echo $f
   echo '\\subsection*{'"$f"'}' >> $TARGET
   echo '\\begin{mdframed}[hidealllines=true,backgroundcolor=blue!20]' >> $TARGET
   echo '\\begin{lstlisting}'  >> $TARGET  
   cat $f >> $TARGET
   echo '\\end{lstlisting}' >> $TARGET
   echo '\\end{mdframed}' >> $TARGET
done


