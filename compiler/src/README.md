# compiler/src

### Folder structure

```
compiler/src
├── App            # This is the application monad
├── Prelude.hs     # This is the custom prelude that share to whole repo
```

## Details description

#### Application environment

Data type for the runtime environment for the whole application is defined in the `App/Env.hs` module.
It contains various fields required for the application processing, like optimization flags and logger.

Environment initialisation is happening in the `Terminal` module.

#### Application monad

Main application monad can be found in the `App/Monad.hs` module.
