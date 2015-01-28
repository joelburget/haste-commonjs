haste-commonjs
==============

Easily use CommonJS modules from Haste.

Example
-------

```haskell
foreign import commonjs "require('adder')" adder :: Int -> Int -> Int
```

This allows us to use CommonJS imports, avoiding globals. For example, before haste-commonjs, importing adder from a CommonJS module would require

haskell:

```haskell
foreign import ccall "global_adder" adder :: Int -> Int -> Int
```

javascript:

```javascript
global_adder = require('adder');
```

Specifying Options
------------------

Haste-commonjs uses npm's [package.json](https://docs.npmjs.com/files/package.json) to specify a few Haste options.

### Example (from [material-ui-hs](https://github.com/joelburget/material-ui-hs))

```json
{
  "name": "material-ui-hs",
  "version": "0.0.1",
  "haste-options": {
      "in": "Main.hs",
      "out": "mui.js",
      "stubs": ["hs-stubs.js"],
      "conversions": [
          {
              "hsTy": "String",
              "jsTy": "JSString",
              "hsToJs": "fromJS",
              "jsToHs": "toJS"
          }
      ]
  }
}
```

* `in`: The source Haskell file we're transforming into a module
* `out`: Where to put the resulting JavaScript
* `stubs`: Javascript stubs to be included by Haste with `--with-js`.
* `conversions`: haste-commonjs can perform some type conversions automatically for you! Here I'm declaring that Haskell's `String` is equivalent to a JavaScript `JSString`.

`foreign import`
----------------

TODO

```haskell
foreign import commonjs "%1.prob = %2" setProb :: Obj -> Prop -> IO ()
foreign import commonjs 
    "React.createElement(require('material-ui').LeftNav, %1, %2)"
    mui_leftNav :: RawAttrs -> ReactArray -> IO ForeignNode
```
