#/bin/bash
IP=127.0.0.1

# TODO: unnecessary sudo problem process
sudo *

if [ "$1" = "build" ]; then
    dir=$(pwd)
    cd $2
    if [ "$3" = "origin" ]; then
        ghc -threaded -O2 -rtsopts Main_origin.hs
    elif [ "$3" = "simple" ]; then
        ghc -threaded -O2 -rtsopts Main_simple.hs $dir/../SimpleMS.hs
    fi
    cd $dir
elif [ "$1" = "clear" ]; then
    rm $2/*.dyn_o
    rm $2/*.hi
    rm $2/*.o
    rm $2/*.dyn_hi
    rm $2/Main_origin
    rm $2/Main_simple
elif [ "$1" = "allclear" ]; then
    rm *.dyn_o
    rm *.hi
    rm *.o
    rm *.dyn_hi
    rm */*.dyn_o
    rm */*.hi
    rm */*.o
    rm */*.dyn_hi
    rm */Main_origin
    rm */Main_simple
elif [ "$1" = "run" ]; then

##### Set Input File Name
    if [ "$2" = "cntPrime" ]; then
        #input=cntPrime.4.txt
        input=cntPrime.1000.txt
        #input=cntPrime.10000.txt
    elif [ "$2" = "kmeans" ]; then
        input=points.bin
    elif [ "$2" = "plag" ]; then
        input="/10/ java"
        #input="/150/ java"
    elif [ "$2" = "sudoku" ]; then
        input=sudoku17.1000.txt
        #input=sudoku17.16000.txt
        #input=sudoku17.49151.txt
    elif [ "$2" = "wordCount" ]; then
        input=mascWordList.txt
    fi
        
#### Run Master
    if [ "$4" = "master" ]; then
        sudo ./$2/Main_$3 master $IP 99 ./$2/input/$input

#### Run Slave
    elif [ "$4" = "slave" ]; then
        echo "Running slave"
        slaveNum=$5
        for i in $(seq ${slaveNum})
        do
            let port=200+$i
            sudo ./$2/Main_$3 slave $IP $port &
            pid[$i]=$!
            echo make pid[${pid[$i]}]
        done
        echo "------Running complete-------"
        read -n 1 -p "Kill Slave"
        for i in $(seq ${slaveNum})
        do
            sudo kill ${pid[$i]}
            echo kill pid[${pid[$i]}]
        done
    fi
fi
