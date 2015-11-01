# A library for easily querying mixpanel.

## Getting started

After pulling, the first thing you will want to do is create your sandbox with


```bash
cabal sandbox init
```

This will create a sandbox to keep your project dependencies separate from your
system dependencies. It does mean we have to handle a few things differently. 


You would start ghci with 


```bash
cabal repl
```

To get a session with your sandbox dependencies.

Similarly if you want to install hlint you would
do something like the following

```bash
cabal install hlint
cabal exec hlint
```

Since we are running in a sandbox, hlint isn't installed system wide. 
We have to use `cabal exec` to use the `hlint` now installed in our
sandbox.
