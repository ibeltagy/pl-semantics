#71, 72 (equality) comp
#103 infinite loop
for pair in {104..1500}
do
	echo "### Pair $pair"
	date
	java -cp ./target/classes:`cat cp.txt` psl.$pair
done


