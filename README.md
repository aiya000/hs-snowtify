# :snowman: hs-snowtify :snowman:
snowtify send your result of `stack build` (`stack test`) to notify-daemon :dog2:

![screenshot](screenshot.png)


# :notes: Usage :notes:
```console
$ snowtify test
(`stack test` results is shown, it is like above screenshot)
$ snowtify build
(same as snowtify test, but stack build is executed)
$ snowtify
(same as snowtify build, snowtify run build by default)
```


# :muscle: Example :muscle:
```console
$ dunst &
(dunst is a notify-daemon)
$ cd <some haskell project directory>
$ watchexec -w . 'snotify test'
(`stack test` results is shown after you update some file)
```


# :diamonds: How to install this ? :diamonds:

- This way is never supported now ~~1. Use haskell-stack~~

```console
$ stack install snowtify
```

- 2. Use cabal

```console
$ cabal install snowtify
```
