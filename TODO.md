# List of things to do
## Bug fixes and features for 0.2.1
- Real sum types (not union types) (this means making the `a: Int` syntax work and making `Int | Int` error instead of reduce)
- Improved debug function that outputs line number and the call to debug
- `--debug-lines` flag that takes in a comma separated list and inserts `debug`s after them
- New syntax for match expressions

## Third official release
These things are features and fixes I'd like to get completed for the third major release (v0.3.0) of Curly.
- Standard library
- Module system
- Bindings with C
- Overridable operators
- Fix memory leaks
- See github issues

## Fourth official release:
These things are features and fixes I'd like to get completed for the fourth major release (v0.4.0) of Curly.
- Product types
- Span

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
- Proper thought out system for IO

## Things in the far future
These things are goals that will take months, if not years, to even start due to either their complexity or dependencies.
- Package manager (written in Curly)
- Bootstraped compiler
- Optimisations
- Debugger

