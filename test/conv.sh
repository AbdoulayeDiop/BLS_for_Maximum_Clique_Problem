#!/bin/bash
if (($# != 1))
then
    echo "nombre d'argument incorrect">&2
    echo "utilisation : $0 nom_du_fichier_dimacs_a_convertir"
    exit 1
fi

if [[ ! -e $1 ]]
then
    echo "$1 n'est pas un fichier">&2
    exit 1
fi

cat $1 > dimacs.out

while read ligne
do
    if [[ $ligne =~ ^p ]]
    then
        read _ _ nbVertex _ <<< $ligne
        echo $nbVertex
        for ((i=1;i<=$nbVertex;i++))
        do
            weigth=$(($i%200+1)) 
            echo n $i $weigth >> dimacs.out
        done
    fi
done < $1

