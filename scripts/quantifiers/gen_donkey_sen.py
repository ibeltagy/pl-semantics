#!/usr/bin/env python

import argparse
import sys
import difflib
import operator

def main():
	Qs = ["some", "all", "no", "not all"]
	v = "eat"
	lhss = ["man", "hungry man"]
	rhss = ["food", "delicious food"]
	direction = ["down", "up"]
	rules = [
		#"some-some-down",
		"some-some-up",
		#"some-all-down",
		#"some-all-up",
		#"some-no-down",
		#"some-no-up",
		#"some-not all-down",
		#"some-not all-up",
		#"all-some-down",	#<<--- existance problem (should be true in case of full blown existance)
		"all-some-up",
		"all-all-down",
		#"all-all-up",
		#"all-no-down",
		#"all-no-up",
		#"all-not all-down",
		#"all-not all-up",
		#"no-some-down",
		#"no-some-up",
		#"no-all-down",
		#"no-all-up",
		"no-no-down",
		#"no-no-up",
		#"no-not all-down",	#<<--- existance problem (should be true in case of full blown existance)
		#"no-not all-up",	#<<--- existance problem (should be true in case of full blown existance)
		#"not all-some-down",
		#"not all-some-up",
		#"not all-all-down",
		#"not all-all-up",
		#"not all-no-down",
		#"not all-no-up",
		#"not all-not all-down",
		"not all-not all-up"]

	for q11 in Qs:
		for q12 in Qs:
			for q21 in Qs:
				for q22 in Qs:
					for lhs in [0, 1]:
						for rhs in [0, 1]:
							infer1 = q11 + "-" + q21 + "-" + direction[lhs]
							infer2 = q12 + "-" + q22 + "-" + direction[rhs]
							entail = infer1 in rules and infer2 in rules
							entail = False
							firstPlace = ""
							secondPlace = ""
							firstQ = ""
							secondQ = ""
							if (q11 == q21):
								firstQ = q11
							if (q11 == "all" and q21 == "some"):
								firstQ = "some"
							if (q12 == q22):
								secondQ = q12
							if (q12 == "all" and q22 == "some"):
								secondQ = "some"
							if firstQ != "" and secondQ != "":
								if firstQ == "all" or firstQ =="no":
									firstPlace = "down"
								else:
									firstPlace = "up"
#								if firstQ == "no" or firstQ =="not all":
#									print "flip"
								if (firstQ == "no" or firstQ =="not all") and (secondQ == "all" or secondQ =="no"):
#									print "up 1"
									secondPlace = "up"
								elif (firstQ == "all" or firstQ =="some") and (secondQ == "some" or secondQ =="not all"):
									#print "up 2"
									secondPlace = "up"
								else:
#									print "down"
									secondPlace = "down"
								if firstPlace == direction[lhs] and secondPlace == direction[rhs]:
#									print firstPlace + "  " + secondPlace
									entail = True
									
#							print str(entail)
							print q11 + " "+ lhss[lhs] + " "+ v + " " + q12 + " "+ rhss[rhs] + "\t" + q21 + " "+ lhss[(lhs+1)%2] + " "+ v +" "+ q22+ " "+ rhss[(rhs+1)%2] + "\t" + str(entail)  #+ "  " + direction[lhs] + "  " + direction[rhs]
	for q11 in Qs:
		for q12 in Qs:
			print q11 + " "+ lhss[lhs] + " "+ v + " " + q12 + " "+ rhss[rhs]
	for q11 in Qs:
		for q12 in Qs:
			for lhs in [0, 1]:
				print q11 + " "+ lhss[lhs] + "\t" + q12 + " "+ lhss[(lhs+1)%2]
	for r in rules:
		print r
main()
