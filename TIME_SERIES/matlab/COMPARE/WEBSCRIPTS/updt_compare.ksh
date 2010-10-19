#!/bin/ksh
#set -x
DIRNAME=$1
FIGNAME=$2

fgrep -q -e "$1" log/compare.log

if [ $? != 0 ] ; then
 echo $1 >> log/compare.log
 # add some html lines to index.html describing the directories just added:
 cd tmp
cat << eof >> log
Comparison between <a href="${DIRNAME}/TIME_SERIES">$FIGNAME</a>
<br>&nbsp;
eof
cat header >! index.html
cat log >> index.html
cat coda  >> index.html

\cp index.html  ../web/DRAKKAR/COMPARE/
fi
