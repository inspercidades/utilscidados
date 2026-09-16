# make_short_names() errors when the namespace is exhausted

    Code
      make_short_names(rep("a", 11), max_length = 2)
    Condition
      Error in `make_short_names()`:
      ! Cannot create 11 unique names with a 2-byte limit.

