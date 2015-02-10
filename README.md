## Experimental (API will change)

## SortedSet, SortedMap, HashSet, HashMap for Ur/Web

based on Arne Anderson Tree as listed in [wikipedia](https://en.wikipedia.org/wiki/AA_tree).

with tests on inserts, membership after deletes, and AATree properties.

The Haskell version I made first
has been tested with QuickCheck and passes tests for all AATree properties

#### to build it 

```bash
export C_INCLUDE_PATH=/path-to-your-urweb-installation/include
export LIBRARY_PATH=/path-to-your-urweb-installation/lib

# C file used in lib/lib_hashable/src/Hashable for hashTree, hashSet, hashMap
cd lib/lib_bits/src/c
gcc -c Bits.c
cd ../../../..

urweb aatree_test_v2

# execution
./aatree_test_v2.exe -p 8081 &   # -p <server port>

browser http://localhost:8081/

# when done, if the server has been started in background
killall -TERM aatree_test_v2.exe
```

Repeating page retrieval makes the test use different input random data.

--------------------

urweb unordHashTree_test

test as above.

--------------------

#### previous test design got random ints reading from /dev/urandom in Random.c

```bash

cd test/util/c
gcc -c Random.c
cd ../../..

urweb aatree_test_v1
```




