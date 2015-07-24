# clj.sigsub

A Clojure library designed to ... well, that part is up to you.

## Usage

FIXME

Dependent signals must be dereferenced immedeately upon function call.
Lazily evaluated signals will not build dependencies properly.

Does not depend on b:
(signal/make-derived (fn [] (map #(+ @b x) [0 1 2])))

To fix:
(signal/make-derived (fn [] (doall (map #(+ @b x) [0 1 2]))))
