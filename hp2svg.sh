cabal run bigrel -- --attribute-count 10 --tuple-count 10000 -d "x:=x" +RTS -h > /dev/null
./.cabal-sandbox/bin/hp2pretty bigrel.hp
