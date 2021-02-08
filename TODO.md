# List of things to do
## Third official release
These things are features and fixes I'd like to get completed for the third major release (v0.3.0) of Curly.
- Standard library
- Module system
- Bindings with C
- Tagged system for side effects
- Fix memory leaks
- Improved debug function so that outputs line number and the call to debug (example: `[main.curly:1] (debug 2) = 2`)
- Remove hacky `putch` function and replace it with an actual C function
- See github issues

## Fourth official release:
These things are features and fixes I'd like to get completed for the fourth major release (v0.4.0) of Curly.
- Product types
- Span (`**`) for product types
- Overridable operators
- Associated functions

## Future releases
These things are features and fixes I'd like to get completed in some point in the far future. Although these features are desired, they are not a priority due to either their complexity or dependencies. These are not in any particular order, and will be moved into an appropriate release when decided upon. These features, however, will be necessary to implement before an official v1.0.0.
- Intersection types
- Generics
- Mutability
- Laziness
- Lists
- Span (`**`) for lists
- `@map` (example:
```
@map
increment x: Int = x + 1
debug (increment 1 2) # [2, 3]
```
- Strings (which are basically just lists of chars)
- `?`/`!` operators
- Function composition operator
- Iterators
- For loops
- Memoization with `@memoize`

## Things in the far future
These things are goals that will take months, if not years, to even start due to either their complexity or dependencies.
- Package manager (written in Curly)
- Bootstraped compiler
- Optimisations
- Debugger

