Normalization by evaluation in general, is to break up the process of producing a normal form into
distinct steps.

1. We first "evaluate" the code into a new representation which does not admit any beta reductions
and simultaneously tag every part of the term that might need eta expansion. This representation is
the "semantic domain". Next we "quote" the term from the semantic domain back into the syntax and
perform the tagged eta expansions.