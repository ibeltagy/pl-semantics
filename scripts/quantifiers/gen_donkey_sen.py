#!/usr/bin/env python

import argparse
import sys
import difflib
import operator

def simplify (d1, pos1, d2, pos2, pos3):
	if (d1 == "no" or d1 == "not all"):
		d1 = evaluateQ(d1)
		d2 = flip(d2)
#		pos1 = flip(pos1)
	if (d2 == "no" or d2 == "not all"):
		d2 = evaluateQ(d2)
#		pos2 = flip(pos2)
		pos3 = flip(pos3)
#	print d1 + " "+ pos1 + " " + d2 + " " + pos2 + " " + pos3
	return (d1, pos1, d2, pos2, pos3)

def flip(sign):
	if sign == "+":
		return "-"
	elif sign == "-":
		return "+"
	elif sign == "no":
		return "some"
	elif sign == "all":
		return "not all"
	elif sign == "some":
		return "no"
	elif sign == "not all":
		return "all"
	else:
		raise

def evaluateQ (q):
	if q == "no":
		return "all"
	elif q == "not all":
		return "some"
	else:
		raise

def isEntail (q, polarity, direction):
	if(q == "all"):
		polarity = flip(polarity)
	if(polarity == "-" and direction == 0):
		return True
	if(polarity == "+" and direction == 1):
		return True
	return False


def main():
	Qs = ["some", "all", "no", "not all"]
	v = "eat"
	lhss = ["man", "hungry man"]
	rhss = ["food", "delicious food"]
	direction = ["down", "up"]

	for q11 in Qs:
		for q12 in Qs:
			for q21 in Qs:
				for q22 in Qs:
					for lhs in [0, 1]:
						for rhs in [0, 1]:
							(ed1, epos1, ed2, epos2, epos3) = simplify(q11, "+", q12, "+", "+")
							(qd1, qpos1, qd2, qpos2, qpos3) = simplify(q21, "+", q22, "+", "+")
							entail = True
							if (epos1 != qpos1):
								entail = False
							if (epos2 != qpos2):
								entail = False
							if (epos3 != qpos3):
								entail = False

							if (ed1 == "some" and qd1 == "all"):
								entail = False
							elif (ed1 == "all" and qd1 == "some"):
								ed1 = "some"

							if (ed2 == "some" and qd2 == "all"):
								entail = False
							elif (ed2 == "all" and qd2 == "some"):
								ed2 = "some"

							if ( not isEntail (ed1, epos1, lhs)):
								entail = False
							if ( not isEntail (ed2, epos2, rhs)):
								entail = False

							entailResult = 0
							if (entail):
								entailResult = 1
							print q11 + " "+ lhss[lhs] + " "+ v + " " + q12 + " "+ rhss[rhs] + "\t" + q21 + " "+ lhss[(lhs+1)%2] + " "+ v +" "+ q22+ " "+ rhss[(rhs+1)%2] + "\t" + str(entailResult)  
main()
