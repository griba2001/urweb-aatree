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
