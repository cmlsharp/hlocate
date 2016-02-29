##hlocate

An attempt at implementing the locate utility in Haskell.

### Status
Bare-bones functionality
#### Done:
* Relatively performant querying (somewhat slower than mlocate)
* Serialization of data structure (database is roughly 2/3 the size of mlocate's on my system)
* Basic option parsing 
* Config file parsing
* Pruning

#### To-Do (in no particular order):
* Regex supprt (regex-posix probably)
* Implement the rest of locate's features
* "Merge" when updating as mlocate does to vastly reduce update times
* Better error handling
