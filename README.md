# topoi
This is the main monorepo codebase of Topoi programming language.

### Status

We are in the phrase of building the topoi-core, a minimal representation of topoi programming language (that syntax is yet to be decided). This allows us to proof the language semantics and focus on our type system.

### Codebase

<!-- #### Technologies

Here is the the folder structure of this mono repo is:

```
topoi/
├── compiler        # Compiler source code
├── terminal        # Terminal CLI frontend, handle the arguments and config of compiler
```

And we are using **Haskell** to construct our compiler because it is well suited to this domain (compiler). And the
language allows use to make syntax tree manipulation easily. -->

### Install and running it

First you need to get the source code

```git clone git@github.com:topoi-lang/topoi.git```

### Installation
If you are using [stack](https://docs.haskellstack.org/en/stable/README/). Then you can just run

```stack build```

Or

```cabal v2-build```

if you are using cabal to compile Haskell program.

### Code style

We are using [ormolu](https://github.com/tweag/ormolu) to do code formatting. And we too provided the editorconfig.

Please stick to the latest ormolu.

## Issue and Contribution

Please use Github issues for bug reports and requests. To contribute directly, open a pull request on [Github repo](https://github.com/topoi-lang/topoi/issues). Files must be contributed under the MIT license.

If you see something that can be improved, please contribute or contact us!

##### MIT Licensed, see the [LICENSE](https://github.com/topoi-lang/topoi/blob/master/LICENSE) file.
