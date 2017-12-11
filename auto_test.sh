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


part5 () {

score=0
max=0
ext=$1

echo "Part 5: Scheduling"

echo -n "Bad cases "
for f in tests/syntax/bad/*.$ext; do
    echo -n ".";
    max=`expr $max + 1`;
    compile --schedule-only $f;
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
    compile --schedule-only $f;
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

echo -n "Part 5: $score/$max : $percent%"
echo
echo; }


part6 () {

ext=$1

score_comp=0
score_out=0
score_test=0
max=0

echo
echo "Part 6: Code Generation"

timeout="why3-cpulimit 30 0 -h"

for f in tests/syntax/good/*.$ext; do
  echo -n "."
  c_file=tests/syntax/good/`basename $f .$ext`.c
  rm -f $c_file
  expected=tests/syntax/good//`basename $f .$ext`.lout
  input_f=tests/syntax/good/`basename $f .$ext`.lin
  max=`expr $max + 1`;
  if compile $f; then
    rm -f out
    score_comp=`expr $score_comp + 1`;
    if gcc $c_file && eval "./a.out $input_f > out"; then
      score_out=`expr $score_out + 1`;
      if cmp --quiet out $expected; then
        score_test=`expr $score_test + 1`;
      else
        echo
        echo "FAIL : wrong output $f"
      fi
    else
      echo
      echo "FAIL : generated code doesn't work $f"
    fi
  else
    echo
    echo "FAIL : code generation $f (should succeed)"
  fi
done
echo

echo "Part 6:";
percent=`expr 100 \* $score_comp / $max`;
echo "Code Generation : $score_comp/$max : $percent%";
percent=`expr 100 \* $score_out / $max`;
echo "Code Execution : $score_out/$max : $percent%";
percent=`expr 100 \* $score_test / $max`;
echo "Code Behaviour : $score_test/$max : $percent%";}


























test () {
  ext=$1
  part1 $ext;
  part2 $ext;
  part3 $ext;
  part4 $ext;
  part5 $ext;
  part6 $ext;
}

case $option in
    "-v"    )
      verbose=1;;
    "-lus"  )
      test_lus=1
      test_elus=0;;
    "-elus" )
      test_lus=0
      test_elus=1;;
    "-both" )
      test_lus=1
      test_elus=1;;
    *       )
      verbose=0
      test_lus=1
      test_elus=0;;
esac

if [[ $test_lus != 0 ]]; then
  test lus
  echo
fi

if [[ $test_elus != 0 ]]; then
  test elus
  echo
fi
