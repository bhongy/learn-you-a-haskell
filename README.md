# Learn Haskell

- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com)
- [Finite-State Machines in Haskell](https://wickstrom.tech/archive.html)

## Set up

Follow: https://www.haskell.org/downloads

I chose minimum installer and installed directly to my local machine (macOS). I use Stack for package management.


## Play in ghci

```sh
stack setup
stack ghci

# (compile and) load a file
:load <file>
:l <file>
:l basic # basic.sh

# reload the modules
:reload
:r
```

## Starting a new project

```sh
stack new <project_name> [template]
cd <project_name>
stack build
stack exec <project_name>-exe # run the compiled project executable
```

`stack build` downloads and build (install) dependencies.

## Notes about Stack and Package Management
use [stack](https://docs.haskellstack.org/en/stable/README/) to manage packages (as well as local GHC).

### Adding a new package
- use [Hackage](http://hackage.haskell.org/packages/search) or [Hoogle](https://hoogle.haskell.org/) to search for new packages
- update `dependencies` in `package.yaml`
- run `stack build`
