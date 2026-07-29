#include "fricas"

MyOrder : OrderedSet == add {

    Rep == Integer;

    import from Rep;

    (a : %) = (b : %) : Boolean == rep(a) = rep(b);

    (a : %) < (b : %) : Boolean == rep(a) < rep(b);

    coerce(a : %) : OutputForm == coerce(rep(a));

}
