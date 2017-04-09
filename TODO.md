# TODO

## Documentation
* Add examples of more complex behaviour
  to the README

## Future work
* Implement a real-time backend
* Implement time-varying transformations:
  ```
  type TVBoidTransformation = Time -> BoidTransformation
  ```
  This, in combination with `avoid`, should be sufficient
  to implement things like "predators" etc.
* Implement space-varying transgformations:
  ```
  type SVBoidTransformation = Vec -> BoidTransformation
  ```
  This could be useful to e.g. "lay out paths" and similar
  things.
* Combine `SVBoidTransformation` and `TVBoidTransformation`
  in interesting ways.
