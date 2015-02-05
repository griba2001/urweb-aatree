#ifndef BITS_H
#define BITS_H

#include <urweb/urweb.h>

uw_Basis_int uw_Bits_wordSize();

uw_Basis_int uw_Bits_andb(uw_Basis_int x, uw_Basis_int y);

uw_Basis_int uw_Bits_orb(uw_Basis_int x, uw_Basis_int y);

uw_Basis_int uw_Bits_xorb(uw_Basis_int x, uw_Basis_int y);

uw_Basis_int uw_Bits_notb(uw_Basis_int x);

uw_Basis_int uw_Bits_fromFloat( uw_Basis_float x);

#endif
