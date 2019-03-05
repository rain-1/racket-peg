#lang peg

number <--- [0-9]+;
sum <--- number '+' number;
