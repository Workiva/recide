# Exception Sanitization Tools

Recide provides a collection of tools to acquire a ["sanitized"](https://github.com/Workiva/recide/blob/master/java-src/recide/sanex/ISanitized.java) version of a exception that should be considered safe to log (but may not be useful as a result).

### `recide.sanex.Utils`

This class contains a handful of static utility methods:

#### `getCurrentSanitizationLevel()`

Equivalent to deref'ing `recide.sanex/*sanitization-level*`.

#### `createSuppressionMap(...)`

Creates an IPersistentMap with the appropriate keywords corresponding to the boolean args.

#### `sanitize(Throwable)`, `sanitize(Throwable, IPersistentMap)`

Shortcut to Clojure IFn `recide.sanex/sanitize`.
