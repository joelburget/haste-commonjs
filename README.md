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
      "out": "out.js",
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

running
-------

Now that we've specified the options it couldn't be simpler to compile our Haskell to JavaScript:

```shell
> haste-commonjs
```

This uses `package.json` to package `Main.hs` into `out.js`, a CommonJS module:

`out.js`:
```javascript
(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
"use strict";
function mui_leftNav_CImpl (a1,a2) { return React.createElement(require('material-ui').LeftNav, a1, a2); }
function mui_dropDownMenu_CImpl (a1,a2) { return React.createElement(require('material-ui').DropDownMenu, a1, a2); }
...
```

This module consists of:
* The CommonJS wrapper
* Javascript stubs automatically generated from our foreign imports
* The stubs we asked for in `package.json`
* dependencies that we `require`
* The compiled Haskell

Now just drop `out.js` into the page.

Benefits:
* We haven't polluted the page with globals.
* Our dependencies are explicit, not implicit
* This is a first step in working with the JavaScript ecosystem.

`foreign import`
----------------

TODO

```haskell
foreign import commonjs "%1.prob = %2" setProb :: Obj -> Prop -> IO ()
foreign import commonjs 
    "React.createElement(require('material-ui').LeftNav, %1, %2)"
    mui_leftNav :: RawAttrs -> ReactArray -> IO ForeignNode
```
