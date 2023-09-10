Another implementation of jx in Haskell this time. A couple things:

* Haven't written Haskell in ~15 years, looks like GHC is all that's left
  anymore?
* Wanted to see what I can get away with without dipping into packages,
  bytestrings, and GHC extensions
* Bad JSON will fail with a matching error (this is just an afternoon project)

Interesting that it's implemented like a recursive decent parser, but, due to
laziness, the runtime behaves more like the char-at-a-time processor I wrote
for `jx-dlang`. 

Quick comparison of three versions against a 181M test file (dumping stdout to
/dev/null, to eliminate that aspect):

* this one processes it in 13s
* dlang version is 7.1s
* zig version is 6.9s
* nodejs version crashes after 40.9s
