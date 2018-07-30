#lang peg

number <-- value:[0-9]+ ;
sum <-- v1:number ~'+' v2:number ;
