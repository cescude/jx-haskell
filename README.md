Another implementation of jx in Haskell this time. A couple things:

* Haven't written Haskell in ~15 years, looks like GHC is all that's left
  anymore?
* Wanted to see what I can get away with without dipping into packages,
  bytestrings, and GHC extensions
* Bad JSON will fail with a matching error (this is just an afternoon project)

Interesting that it's implemented like a recursive decent parser, but, due to
laziness, the runtime behaves more like the char-at-a-time processor I wrote
for `jx-dlang`. 

Quick comparison of three versions against a 181M test file:

* this one processes it in 11.07s
* dlang version is 8.11s
* zig version is 14.43s
* nodejs version crashes after 40.9s

Interesting note, testing w/ `>/dev/null` is significantly faster for the zig
version than testing for `|tail -5`. Since real usage of these programs is to
actually *view* the results, I think perf testing against `| tail -5` gives
a nice medium (ie., terminal pushback is minimal, but the program itself
doesn't try to skip things).
