IN="bla@some.com;john@home.com"
ADDR1=""
ADDR2=""
read ADDR1 ADDR2 <<<$(IFS=";"; echo $IN)