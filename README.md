# adl-demo-hs

Simple haskell demo showing how [ADL][adl] can be used to define the schema of the configuration file for a hypothetical server.

# Building

The ADL generated code is checked into the repo so, assuming you have the [haskell stack tooling][stack] installed, it can be built with just `stack build`. However, for the purposes of experimentation you are likely to want to change the example [config adl definition][config], in which case you'll need the `adlc` tool on your path. Follow the adl [installation instructions][adl-install]. Then to build this demo:

```
./scripts/generate-adl.sh
stack buld
```

[config]:adl/config.adl
[adl]:https://github.com/timbod7/adl
[adl-install]:https://github.com/timbod7/adl/blob/master/docs/install.md
[stack]:https://docs.haskellstack.org/en/stable/README/
