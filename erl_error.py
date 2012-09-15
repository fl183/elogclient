#!/bin/env python


while True:
	line = raw_input("error: \n")
	values = line.split(',')
	s = ''
	for value in values:
		ivalue = int(value)
		s += chr(ivalue)
	print s
