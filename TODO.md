# List of things to do
## Bug fixes and features for 0.2.1
- Real sum types (not union types) (this means making the `a: Int` syntax work)
- Improved debug function that outputs line number and the call to debug
- `@debug` annotation that calls debug on the last value
- New syntax for match expressions, specifically:
```
match 2: Int | Float | Bool | (Int -> Int)
to i: Int => debug i
to Float => 0 # $ is no longer valid
# implement this when generics are a thing:
to o: '_ => debug o
```

## Third official release
These things are features and fixes I'd like to get completed for the third major release (v0.3.0) of Curly.
- Standard library
- Module system
- Bindings with C
- Tagged system for side effects
- Fix memory leaks
- See github issues

## Fourth official release:
These things are features and fixes I'd like to get completed for the fourth major release (v0.4.0) of Curly.
- Product types
- Span
- Overridable operators
- Associated functions

## Future releases
These things are features and fixes I'd like to get completed in some point in the far future. Although these features are desired, they are not a priority due to either their complexity or dependencies. These are not in any particular order, and will be moved into an appropriate release when decided upon. These features, however, will be necessary to implement before an official v1.0.0.
- Intersection types
- Generics
- Mutability
- Laziness
- Lists
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

