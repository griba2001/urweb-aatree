## SortedSet, SortedMap for Ur/Web

based on AATree [(wikipedia version)](https://en.wikipedia.org/wiki/AA_tree).

with tests on inserts, deletes, membership and AATree properties,

with ramdom list generator based on reading system /dev/urandom.

The Haskell version I made first
has been tested with QuickCheck and passes tests for all AATree properties

### to build it (because there is an FFI module)

```bash
export C_INCLUDE_PATH=/your-path-to-your-urweb-installation/include
export LIBRARY_PATH=/your-path-to-your-urweb-installation/lib

cd test/util/c
gcc -c Random.c
cd ../../..

urweb aatree_test

# execution
./aatree_test.exe -p 8081    # -p &lt;server port&gt;

browser localhost:8081/
```





