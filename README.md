# Active QuickCheck2

A Clojure port of the original Haskell QuickCheck, integrated with
[Active Data](https://github.com/active-group/active-data).

(We looked at
[ClojureCheck](https://bitbucket.org/kotarak/clojurecheck),
[`clojure.test.generative`](https://github.com/clojure/test.generative),
but neither seems faithful to the original, particularly concerning
the reproducibility of test runs, and a set of generator combinators
that includes random generation of functions.
[`test.check`](https://github.com/clojure/test.check) is going down
the right path, but it's lacking some features we want, and is moving
too slow for our purposes.

This library, however, is a straighforward port of the Haskell code
from John Hughes's original paper.
## License

Copyright © 2024 Active Group GmbH

Distributed under the Eclipse Public License, the same as Clojure.
