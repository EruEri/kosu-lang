FORMAT="let commit_hash"

HASH=$(git describe --always --abbrev=7)
if [ "$?" -eq "0" ]; then
        echo "$FORMAT = Some \"$HASH\"" > kosuHash.ml
    else
        echo "$FORMAT = None" 
fi 