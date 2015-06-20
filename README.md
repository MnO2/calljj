# calljj: 叫我姊姊語

lispy 語言中有一個 function 叫做 callcc，而這個 project 的名字是特別取成 calljj 來致敬。

基本上這個語言就是[種菜語](http://www.blue.sky.or.jp/grass/)，只不過是用了不同的 symbol，並且是用 Haskell 實作的。原本的網頁並沒有包含 Haskell 實作。

## Examples

* 印出一個「姐」字

```
姐姊姊姐姐姐姐
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
