# calljj: 叫我姊姊語

lispy 語言中有一個 function 叫做 callcc，而這個 project 的名字是特別取成 calljj 來致敬。

基本上這個語言就是[種菜語](http://www.blue.sky.or.jp/grass/)，只不過是用了不同的 symbol，並且是用 Haskell 實作的。原本的網頁並沒有包含 Haskell 實作。

跟一般的 esoteric language 是基於 Turing Machine 不同。calljj 由於種菜語的設計，是基於 lambda calculus 的，所以對於理解 lambda caculus 的學習是有所幫助。另外實作上的 SECD machine，也讓學習者容易理解實際上怎樣用 closure 還有 stack 以最簡單的方式去實作一個 functional language 。

## Examples

* 印出一個「姐」字

```
姐姊姊姐姐姐姐
```

* 印出 1+1 的答案，用「姐」的數量表示答案

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
