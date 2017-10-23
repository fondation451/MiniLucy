#!/bin/bash

shopt -s nullglob

# Test script for lus2rs

option=$1
compiler=minilucy
score=0
max=0
verbose=0

echo -n "Test de $compiler"

echo
echo

compile () {
if [[ $verbose != 0 ]]; then
  echo Compiling $2 $3
  ./$compiler $2 $3;
else
  ./$compiler $2 $3 > /dev/null 2>&1;
fi;
}

part1 () {

score=0
max=0
ext=$1

echo "Part 1: $ext syntactic analysis"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"0")
	echo
	echo "FAIL on "$f" (should have been rejected)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "COMPILER FAIL on "$f"";
    esac
done
echo

# les bons
echo -n "Good cases "
for f in tests/syntax/good/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --parse-only $f;
    case $? in
	"1")
	echo
	echo "FAIL on "$f" (should have been accepted)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
  echo "COMPILER FAIL on "$f"";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Part 1 ($ext): $score/$max : $percent%"; }

part2 () {

score=0
max=0
ext=$1

echo "Part 2: clock analysis"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --clock $f;
    case $? in
	"0")
	echo
	echo "FAIL on "$f" (should have been rejected)";;
	"1") score=`expr $score + 1`;;
	*)
	echo
	echo "COMPILER FAIL on "$f"";
    esac
done
echo

# les bons
echo -n "Good cases "
for f in tests/syntax/good/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --clock $f;
    case $? in
	"1")
	echo
	echo "FAIL on "$f" (should have been accepted)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
  echo "COMPILER FAIL on "$f"";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Part 2: $score/$max : $percent%"; }





test () {
  ext=$1
  part1 $ext;
  part2 $ext;
}


case $option in
    "-v" )
      verbose=1;;
    *    )
      verbose=0;;
esac

test lus

echo
echo

test elus

echo
