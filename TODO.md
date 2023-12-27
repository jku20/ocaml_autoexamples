# TODOS
### Rewrite the frontend
Currently it is very broken. It shouldn't just work with a single function. Not flexible enough. It should work like rustdoc's documentation testing.

More precisly, it should by going through the parsetrees of both `.ml` and `.mli` files and find (item comments)[https://v2.ocaml.org/manual/doccomments.html#ss%3Aitem-comments]. For each of these item comments it should look for the text:
```
\```test
<some ocaml code>
\```
```
It then takes the ocaml code and runs it and captures the output to create:
```
\```test
<some ocaml code>
output------------------------------------------------------------------------
<print output if it exists, if it doesn't this entire section doesn't exist>
\```
```

### Write the backend
First implement a way to generate small but reasonable tests which compose well. This should be like property based testing fuzzers but which care more about being readable and small and less about being good tests.

### Write the middlend
Write a printer function which will create a nice table. It should take in a function to "test" and a "generator" and output the string which is a nice table (or maybe print it out might be better, but yeah, both).

## Priority List
backend -> middlend -> frontend
