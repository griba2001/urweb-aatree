
#include <bits/wordsize.h>
#include <urweb/urweb.h>

uw_Basis_int uw_Bits_wordSize(uw_context ctx) {
   return __WORDSIZE ;
}

uw_Basis_int uw_Bits_andb(uw_context ctx, uw_Basis_int x, uw_Basis_int y) {
             return x & y ;
}

uw_Basis_int uw_Bits_orb(uw_context ctx, uw_Basis_int x, uw_Basis_int y) {
             return x | y ;
}

uw_Basis_int uw_Bits_xorb(uw_context ctx, uw_Basis_int x, uw_Basis_int y) {
             return x ^ y ;
}

uw_Basis_int uw_Bits_notb(uw_context ctx, uw_Basis_int x) {
             return ~x ;
}

uw_Basis_int uw_Bits_floatAsWord(uw_context ctx, uw_Basis_float x) {
        return (uw_Basis_int) x ;
}

