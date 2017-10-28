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
  echo Compiling $1 $2
  ./$compiler $1 $2;
else
  ./$compiler $1 $2 > /dev/null 2>&1;
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
    out=$?;
    case $out in
	"1")
	echo
	echo "FAIL on "$f" (should have been accepted)";;
	"0") score=`expr $score + 1`;;
	*)
	echo
  echo $out
  echo "COMPILER FAIL on "$f"";;
    esac
done
echo

percent=`expr 100 \* $score / $max`;

echo -n "Part 1 ($ext): $score/$max : $percent%"
echo
echo;}

part2 () {

score=0
max=0
ext=$1

echo "Part 2: Typing"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --type-only $f;
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
    compile --type-only $f;
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

echo -n "Part 2: $score/$max : $percent%"
echo
echo; }

part3 () {

score=0
max=0
ext=$1

echo "Part 3: clock analysis"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --clock-only $f;
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
    compile --clock-only $f;
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

echo -n "Part 3: $score/$max : $percent%"
echo
echo; }


part4 () {

score=0
max=0
ext=$1

echo "Part 4: Normalization"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --normalize-only $f;
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
    compile --normalize-only $f;
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

echo -n "Part 4: $score/$max : $percent%"
echo
echo; }









test () {
  ext=$1
  part1 $ext;
  part2 $ext;
  part3 $ext;
  part4 $ext;
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
