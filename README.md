# Mathlogic Homework
## Building
`cabal build` should do it

Alternatively, `ghc Main.hs -O2` - see Build dependencies for details

### Build dependencies
`base >=4.8 && <4.9`
`attoparsec >=0.13 && <0.14`

## Task 1 : Proof correctness checker

### Invocation
* `<path>/Main` -- interactive - stdin / stdout, check after each statement, don't forget head!
* `<path>/Main <input>` -- output to stdout
* `<path>/Main <input> <output>`

## Task 2 : Deduction theorem application / Formal Arithmetic annotator

### Invocation
* `Main <action> [input=stdin] [output=stdout]` -- action is either "a" for annotation or "t" for transformation
* Transform is fail-fast, Annotate is persistent

#### Use cases:
* `Main a in.txt out.txt` -- passtrough from in to out, annotating proof (prints an error summary)
* `Main a in.txt | grep "Не"` -- from in to stdout, filter to only unproven lines
* `Main t in.txt | Main a | grep "Не"` -- get from in, transform, annotate, filter unproven


## Task 3 : (a+1)\*(a+1)=a\*a+2\*a+1

### Invocation
* `Main [input=stdin] [output=stdout]`
* Uses prebuilt proof of target statement @a, then substitutes a for 0'''..''
* `i.e. ./dist/build/task3/task3 <<<"21" | ../task2/dist/build/PredicateDeduction/PredicateDeduction a`
