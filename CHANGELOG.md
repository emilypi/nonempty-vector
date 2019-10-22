# Revision history for nonempty-vector

## 0.2.0.0

* Remove naughty `Generic`, and `Alternative` instances as they can construct empty `NonEmptyVector`s

* Handwritten `Read` and `Read1` instances with safe cons

* Added `uncons`, `unsnoc`, `replicate1`, `generate1`, `iterateN1`, `unsafeCreate`, `unsafeCreateT`, `unfoldr1`, `unfoldr1N`, `unfoldr1M`, `unfoldr1NM`,

* Added `unsafeFromList`, `unsafeFromVector`, and `fromNonEmptyN1`

## 0.1.0.0

* Remove `MonadFail` instance for the sake of backcompat with LTS < 13
* Drop Cabal version down to 2.0

## 0.0.1.1 -- 2019-10-20

* Export `toMVector` and `fromMVector`
* clean up docs

## 0.0.1.0 -- 2019-10-20

* First version. Released on an unsuspecting world.
