
#include <bits/wordsize.h>
#include <urweb/urweb.h>

uw_Basis_int uw_Bits_wordSize() {
   return __WORDSIZE ;
}

uw_Basis_int uw_Bits_andb(uw_Basis_int x, uw_Basis_int y) {
             return x & y ;
}

uw_Basis_int uw_Bits_orb(uw_Basis_int x, uw_Basis_int y) {
             return x | y ;
}

uw_Basis_int uw_Bits_xorb(uw_Basis_int x, uw_Basis_int y) {
             return x ^ y ;
}

uw_Basis_int uw_Bits_notb(uw_Basis_int x) {
             return ~x ;
}

uw_Basis_int uw_Bits_fromFloat( uw_Basis_float x) {
        return (uw_Basis_int) x ;
}

