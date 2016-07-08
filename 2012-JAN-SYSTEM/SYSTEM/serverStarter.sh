#!/bin/bash


while [ "forever" ]
do
	echo "The parsing server server is starting"
	
	clisp  -E UTF-8 ./serverParser-new.lisp  .// 
	Sleep 1
	
done
