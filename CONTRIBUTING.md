Contributions are most welcome. Please follow the following guidelines if you
wish to contribute.

# Module structure

- All modules with private entities that must be exported should go in
  `Network.Pusher.Internal.*`. The public interface of the library should go in
  modules in `Network.Pusher.*`, and for convenience we also re-export everything
  through the top-level `Network.Pusher` module.

# Style guide

- Format Haskell files with `ormolu`, Cabal files with `cabal format`, and
  with `prettier` for everything else that can be.
- Limit line length to 80 characters.
- Module names should be singular, e.g. `Error` instead of `Errors`.
- Record fields should be prefixed with the constructor name in lower case.
  E.g. `appID` in the `Credentials` struct becomes `credentialsAppID`. This is
  to prevent the module namespace becoming polluted with record field getter
  functions.
- Avoid language extensions. They increase barrier to entry for others.
- Import everything either explicitly or qualified. The one exception is data
  constructors and record fields.
