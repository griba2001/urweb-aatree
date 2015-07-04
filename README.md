### Sorted Map, Sorted Set, Unordered HashMap, Unordered HashSet functors for Ur/Web

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

```bash
urweb unordHashTree_test

# test as above.

# ** an intermediate data structure for hashMap buckets

urweb listMap_test

# test as above.
```

---------------------

####Considerations

The functor structure makes possible to factor out the input type requirements for an instance.

I was thinking the haskell construction

```haskell
instance (Ord item) => SortedSet set item
  where ...
```

Equivalent in Ur/Web:

```ocaml
functor MkSortedSet(Q: sig con item :: Type
                       val ord_item: ord item
                     end) = struct ... end
```

####Example:


```ocaml
structure IntSortedSet = Set.MkSortedSet( struct
                                type item = int
                                val ord_item = ord_int
                         end)

structure IntSetOps = SetOps.MkSetOps (IntSortedSet)

val mySortedSet = IntSortedSet.fromList (3 :: 1 :: 2 :: [])



structure StringHashedSet = Set.MkUnordHashSet( struct
                      type item = string
                      val eq_item = eq_string
                      val hashable_item = Hashable.hashable_string
                    end)

structure StringSetOps = SetOps.MkSetOps (StringHashedSet)

val myHashedSet = StringHashedSet.fromList ("ab" :: "cd" :: [])

structure IntKeyedSortedMap = Map.MkSortedMap( struct
                      type key = int
                      val ord_key = ord_int
                    end)

structure IntKMapOps = MapOps.MkMapOps (IntKeyedSortedMap)

val mySortedMap = IntKeyedSortedMap.fromList( (1, "ab") :: (2, "cd") :: [])

structure StringKeyedHashMap = Map.MkUnordHashMap( struct
                      type key = string
                      val eq_key = eq_string
                      val hashable_key = Hashable.hashable_string
                    end)

structure StringKMapOps = MapOps.MkMapOps (StringKeyedHashMap)

val myHashedMap = StringKeyedHashMap.fromList( ("ab", 1) :: ("cd", 2) :: [])

fun main () : transaction page = return <xml>
<body>
        <p>{[mySortedSet]}</p>
        <p>{[myHashedSet]}</p>
        <p/>
        <p>{[mySortedMap]}</p>
        <p>{[myHashedMap]}</p>
</body>
</xml>

```
Last changes:

Removed from functors input the unconstrained types (the item from Maps).
Added fromList / toList to ...Set, ...Map
