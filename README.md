## SortedMap, SortedSet, HashedEqMap, HashedEqSet functors for Ur/Web

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

urweb aatree_test_v3

# execution
./aatree_test_v3.exe -p 8081 &   # -p <server port>

browser http://localhost:8081/

# when done, if the server has been started in background
killall -TERM aatree_test_v3.exe
```

Repeating page retrieval makes the test use different input random data.

--------------------

urweb unordHashTree_test

test as above.

---------------------

####Use

#####Instanciating an IntSortedSet

```ocaml

open Set
open SetOps

structure IntItem = struct
                      type item = int
                      val ord_item = ord_int
                    end

structure IntSortedSet = Set.SortedSet( IntItem)

structure IntSortedSetOps = SetOps.MkSetOps (IntSortedSet)
```

#####Instanciating an StringHashedSet


```ocaml
open Set
open SetOps

structure StringItem = struct
                      type item = string
                      val eq_item = eq_string
                      val hashable_item = Hashable.hashable_string
                    end


structure StringHashedSet = Set.HashedEqSet( StringItem)

structure StringHashedSetOps = SetOps.MkSetOps (StringHashedSet)
```

#####Instanciating an Int * String SortedMap

```ocaml

open Map
open MapOps

structure IntXStringPair = struct
                      type key = int
                      type item = string
                      val ord_key = ord_int
                    end

structure IntXStringSortedMap = Map.SortedMap( IntXStringPair)

structure IntXStringSortedMapOps = MapOps.MkMapOps (IntXStringSortedMap)
```

#####Instanciating an String * Int HashedMap

```ocaml

open Map
open MapOps
open Hashable

structure StringXIntPair = struct
                      type key = string
                      type item = int
                      val eq_key = eq_string
                      val hashable_key = hashable_string
                    end

structure StringXIntHashMap = Map.HashedEqMap( StringXIntPair)

structure StringXIntHashMapOps = MapOps.MkMapOps (StringXIntHashMap)
```
