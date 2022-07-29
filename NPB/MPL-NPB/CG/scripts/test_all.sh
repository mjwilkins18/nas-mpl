#!/bin/bash

echo "W"
for i in {0..5}; do
	./run W | tail -n 5
done
echo "S"
for i in {0..5}; do
	./run S | tail -n 5
done
echo "A"
for i in {0..5}; do
	./run A | tail -n 5
done
echo "B"
for i in {0..5}; do
	./run B | tail -n 5
done
echo "C"
for i in {0..5}; do
	./run C | tail -n 5
done
