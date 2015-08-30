### ioref-stable

`IORef` sadly doesn't provide an `Ord` instance which makes it very
inefficient in some cases nice you can't put it in a `Set` or a
`Map`. This package provides an `IORefStable` types which pairs the
`IORef` with a `Unique` to allow ordering.
