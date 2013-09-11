#!/bin/bash

CMD=$1
shift

case $CMD in
  merge)
	scala multiParses.scala train 1 "$@" 
	scala multiParses.scala test 1 "$@"

	scala multiParses.scala train 2 "$@"
	scala multiParses.scala test 2 "$@"

	scala multiParses.scala train 3 "$@"
	scala multiParses.scala test 3 "$@"
   ;;

  classify)
	scala classify.scala "$@"
   ;;

  remove)
	rm -r out-train/* "$@"
	rm -r out-test/* "$@"
	rm -r multiOut-train/* "$@"
	rm -r multiOut-test/* "$@"
   ;;

esac
exit 0


