## SortedSet, SortedMap for Ur/Web

based on AATree [(wikipedia version)](https://en.wikipedia.org/wiki/AA_tree).

with tests on inserts, membership after deletes, and AATree properties.

The Haskell version I made first
has been tested with QuickCheck and passes tests for all AATree properties

#### to build it 

```bash
urweb aatree_test_v2

# execution
./aatree_test_v2.exe -p 8081 &   # -p <server port>

browser http://localhost:8081/

# when done, if the server has been started in background
killall -TERM aatree_test_v2.exe
```

Repeating page retrieval makes the test use different input random data.

--------------------

#### previous design got random ints reading from /dev/urandom in Random.c

```bash
export C_INCLUDE_PATH=/path-to-your-urweb-installation/include
export LIBRARY_PATH=/path-to-your-urweb-installation/lib

cd test/util/c
gcc -c Random.c
cd ../../..

urweb aatree_test_v1
```




