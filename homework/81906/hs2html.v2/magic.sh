grep rhs spaghetti.tree | sed 's/^[ ]*//' | sed 's/rhs: (//' | tr -d [,]- | tr -s ' ' | awk -F' ' '{ if ( $1 == "exp_do" || $1 == "exp_cond" || $1 == "exp_let_in") print($2, $3, $4, $5) }' > coordinatesOfBlocks
./markBlocks
flex hs2html.flex
gcc lex.yy.c
./a.out spaghetti.hs-foldable > spaghetti.html
rm lex.yy.c a.out coordinatesOfBlocks spaghetti.hs-foldable
