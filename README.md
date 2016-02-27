##hlocate

An attempt at implementing the locate utility in Haskell.

### Status
Bare-bones functionality
#### Done:
* Performant searching and querying (database takes significantly less space than mlocate as well, even with no pruning support yet)
* Serializing data structure
* Basic option parsing 

#### To-Do (in no particular order):
* Pruning
* Config file
* Regex supprt (regex-posix probably)
* Implement the rest of locate's features
* "Merge" when updating as mlocate does to vastly reduce update times
* Better error handling
