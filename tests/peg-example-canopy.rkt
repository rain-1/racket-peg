#lang peg

url       <--  scheme '://' host pathname search hash? ;
scheme    <--  'http' 's'? ;
host      <--  hostname port? ;
hostname  <--  segment ('.' segment)* ;
segment   <--  [a-z0-9-]+ ;
port      <--  ':' [0-9]+ ;
pathname  <--  '/' [^ ?]* ;
search    <--  ('?' [^ #]*)? ;
hash      <--  '#' [^ ]* ;
