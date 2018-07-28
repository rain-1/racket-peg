#lang peg


number <-- [0-9]+ ;
file <-- number (~',' number)* ;
