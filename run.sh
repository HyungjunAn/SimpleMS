#/bin/bash
IP=127.0.0.1
sudo *
if [ "$1" = "build" ]; then
    ghc -threaded -O2 -rtsopts Sequential.hs
    ghc -threaded -O2 -rtsopts EvalMonad.hs
    ghc -threaded -O2 -rtsopts CloudHaskell.hs
    ghc -threaded -O2 -rtsopts UsingSimpleMS.hs
elif [ "$1" = "remove" ]; then
    rm *.hi
    rm *.dyn_o
    rm *.dyn_hi
    rm *.o
    rm Sequential
    rm EvalMonad
    rm CloudHaskell
    rm UsingSimpleMS
elif [ "$1" = "master" ]; then
    #sudo ./$2 master $IP 99 +RTS $3
    sudo ./$2 master +RTS $3
elif [ "$1" = "slave" ]; then
    echo "Running slave"
    input=$3
    for i in $(seq ${input})
    do
        let port=100+$i
        #sudo ./$2 slave $IP $port &
        sudo ./$2 slave &
        pid[$i]=$!
        sleep 1
        echo make pid[${pid[$i]}]
    done
    echo "------Running complete-------"
    read -n 1 -p "Kill Slave"
    for i in $(seq ${input})
    do
        sudo kill ${pid[$i]}
        echo kill pid[${pid[$i]}]
    done
fi
