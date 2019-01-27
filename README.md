# calljj

An esoteric language running with SECD machine. The name was based on a pun of a popular Taiwanese music MV and lisp's callcc. It is bascially [Grass language](http://www.blue.sky.or.jp/grass) with different symbols in use. The reference implementation on the official website doesn't have Haskell, therefore this implementation adds value.

Unlike other esoteric language, Grass language is based on lambda calculus, therefore it would be helpful for beginners to learn lambda calculus. On the other hand, knowing [SECD Machine](https://en.wikipedia.org/wiki/SECD_machine) would be helpful on how to use the most basic structures to implement a functional language.

## Examples

* Print 「姐」

```
姐姊姊姐姐姐姐
```

* Print 1+1 ，And the answer would be encoded as the number of「姐」

```
姐姐姊姊姐 叫我姐姐姐姐姊姊姊姐姐姊姐姐姊姊姊姊姊姊姐姐姐姐姊姐姐 叫我姐姊姊姐姐姐姊姐姐姐姐姊姐姐姐姐姐姐姊姐姐姐姐姐姐姐姐姐
```

## Installation

* With stack

``` bash
git clone https://github.com/MnO2/calljj.git
stack build .
```

* With cabal

``` bash
git clone https://github.com/MnO2/calljj.git
cabal sandbox init
cabal install
```
