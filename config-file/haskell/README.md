# A haskell ADL demonstration

Simple haskell demo showing how [ADL][adl] can be used to define the schema of the configuration file for a hypothetical server.

To follow along with this project, you'll need the ADL compiler [installed][adl-install]
and on your shell PATH.

We'll assume that our project is some sort of server which will load a yaml configuration at
startup. Jumping right in, we can specify the config schema in a file [`adl/config.adl`][config]:

```
module config {

struct ServerConfig {
  Int32 port;
  Protocol protocol = "http";
  LogLevel logLevel = "info";
};

union Protocol {
  Void http;
  SslConfiguration https;
};

struct SslConfiguration {
  FilePath certificate;
  FilePath certificateKey;
};

type FilePath = String;

union LogLevel {
  Void error;
  Void warn;
  Void info;
  Void debug;
  Void trace;
};

};
```

Being minimal, our `ServerConfig` has a port, some protocol information, and a logging level. The
port has no default value, so is required in the configuration. The other fields are optional,
with the given defaults being used in their absence. Note the protocol field is a union (aka a sum type).
If it is `http` then no other information is required.  However, if the protocol is `https` then
paths for ssl certificate details are required. The full syntax and meaning of ADL is in
the [language documentation][adl-language].

We've specified the data type for the server configuration, and we could now run the compiler to
generate the corresponding haskell types and support code. The compiler does its best to generate
idiomatic code in the target languages, but additional language specific information can improve
the generated code. [ADL annotations][adl-annotations] are used for this. Such annotations can be
included in-line in the adl source code, though this get a little noisy when annotations are included
for multiple targets - it gets hard to see the core type definitions themselves in a sea of annotations.

Hence ADL has a standard pattern for language specific annotations: such annotations for an ADL file
x.adl are kept in the file x.adl-lang. Hence the adl compiler, when reading `config.adl` to generate
haskell code, will look for and include the adl file `config.adl-hs` for haskell related annotations.

In this example, [`config.adl-hs`][config-hs] is straightforward:

```
module config {

import adlc.config.haskell.*;

annotation ServerConfig HaskellFieldPrefix "sc_";
annotation Protocol HaskellFieldPrefix "p_";
annotation SslConfiguration HaskellFieldPrefix "ssl_";
annotation LogLevel HaskellFieldPrefix "log_";
};
```

Recent language extensions notwithstanding, haskell's record system is somewhat primitive (try
a google search for "haskell record problem"). A key issue is that record field names need to
be unique in their containing module. To ensure this, by default, the haskell ADL code generator
prefixes each field with its type name. Hence the `ServerConfig` declaration would generate:

```
data ServerConfig = ServerConfig
    { serverConfig_port :: Data.Int.Int32
    , serverConfig_protocol :: Protocol
    , serverConfig_logLevel :: LogLevel
    }
```

Whilst this guarantees that the generated code will compile, those field names are unwieldy.
Hence the [HaskellFieldPrefix][] annotation allows a custom (or no) prefix to be used. With
the above `config.adl-hs` annotations, we get a more friendly:

```
data ServerConfig = ServerConfig
    { sc_port :: Data.Int.Int32
    , sc_protocol :: Protocol
    , sc_logLevel :: LogLevel
    }
```

With the ADL written it's time to run the ADL compiler to generate the haskell code:

```
adlc haskell \
  --outputdir src \
  --package ADL \
  --rtpackage ADL.Core \
  --include-rt \
  --searchdir adl \
  adl/*.adl
```

The `--include-rt` and `--rtpackage` arguments tell the code generator to include the
runtime support files, making the generated code self contained. See the
haskell backend [documentation][adl-haskell] for details.

I generally check the generated code into the source repository. Whilst this approach
has some drawbacks, it has benefits too:

  - you don't need the ADL compiler installed to build the package
  - you can build with your off-the shelf standard build system ([cabal][], [cargo][], [tsc][] etc)
    
The main downside is that changing the source ADL requires explicitly rerunning
the ADL compiler. In most projects I have a [`scripts/generate-adl.sh`][generate] script to
automate this step. Of course, if your build system is up to it, you may wish
to generate the ADL derived code on demand.

We can now write some haskell code!

ADL's core serialization schema is json (a alternate binary scheme is planned). In the
generated haskell, every ADL value is an instance of the [AdlValue][] type class, and then
the library has helper functions to automate deserialization:

```
adlFromByteString :: AdlValue a => LBS.ByteString -> ParseResult a
adlFromJsonFile :: AdlValue a => FilePath -> IO (ParseResult a)
decodeAdlParseResult :: AdlValue a => T.Text -> ParseResult a -> Either T.Text a
```

If one wished to have a configuration file in json format, the latter two functions
are sufficient to read and parse such a file.  But json is less than ideal for human
written configuration, due to its lack of support for comments, and its rigid syntax.
The ADL core doesn't have yaml support, but conveniently the haskell [Data.Yaml][] package can parse
yaml into json values, which the ADL core can then parse into ADL values. This is the
approach we will take, and we write a yaml specific function to load an arbitrary
ADL value:

```
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Yaml as Y
import ADL.Core(runJsonParser, decodeAdlParseResult, AdlValue(..), ParseResult(..))

adlFromYamlFile :: AdlValue a => FilePath -> IO (Either T.Text a)
adlFromYamlFile file = (decodeAdlParseResult from . adlFromYamlByteString) <$> (LBS.readFile file)
  where
    adlFromYamlByteString :: (AdlValue a) => LBS.ByteString -> (ParseResult a)
    adlFromYamlByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
      (Left e) -> ParseFailure ("Invalid yaml:" <> T.pack (Y.prettyPrintParseException e)) []
      (Right jv) -> runJsonParser jsonParser [] jv

    from = " from " <> T.pack file
```

Hopefully this is fairly self explanatory. It:

   - reads the input file contents as a bytestring
   - parses the yaml parser into a in-memory json value
   - parses the in memory json value into an adl value
    
whilst turning parse failures at either level into user friendly error messages.

With this helper function, the scaffolding for our server process is straightforward. We
read an environment variable for the configuration file path, use the `adlFromYamlFile`
written previously, and launch our (dummy) server code.

```
main :: IO ()
main = do
  let configEnvVar = "CONFIG_PATH"
  mEnvPath <- lookupEnv configEnvVar
  case mEnvPath of
    Nothing -> exitWithError (configEnvVar <> " not set in environment")
    (Just envPath) -> do
      eConfig <- adlFromYamlFile envPath
      case eConfig of
        (Left emsg) -> exitWithError (T.unpack emsg)
        (Right config) -> startServer config

exitWithError :: String -> IO ()
exitWithError emsg = do
  hPutStrLn stderr emsg
  exitFailure
  
startServer :: ServerConfig -> IO ()
startServer sc = do
  case sc_protocol sc of
    P_http -> putStrLn ("Starting http server on port " ++ (show (sc_port sc)))
    P_https{} -> putStrLn ("Starting https server on port " ++ (show (sc_port sc)))
  threadDelay 1000000000
```

The simplest configuration yaml specifies just the port, relying on
the ADL defaults for other fields:

```
port: 8080
```

An example that overrides the protocol, and hence must provide additional
information:

```
port: 8443
protocol:
  https:
    certificate: /tmp/certificate.crt
    certificateKey: /tmp/certificate.key
```

The ADL json/yaml serialization schema is straightforward. One point
of note is that ADL unions (like `Protocol` in the example) are serialized
as single element objects. See the [serialisation documentation][adl-serialization]
for details.
          
The parser provides helpful error messages. In the above example
config, if you leave out the last line and fail to set the SSL key, the
error is:

```
Unable to parse a value of type config.ServerConfig from demo-server-example3.yaml:
expected field certificateKey at protocol.https
```


Hopefully this repo gives a simple but useful demonstration of ADL usage from haskell.
It's really only a starting point - the ADL system's value increases dramatically when used
to ensure consist types between systems written in multiple languages. 
      
[config]:adl/config.adl
[config-hs]:adl/config.adl-hs
[generate]:scripts/generate-adl.sh

[adl]:https://github.com/timbod7/adl
[helix]:https://www.helixta.com.au/
[openapi]:https://swagger.io/specification/
[yaml]:https://en.wikipedia.org/wiki/YAML
[adl-install]:https://github.com/timbod7/adl/blob/master/docs/install.md
[adl-demo-hs]:https://github.com/timbod7/adl-demo-hs
[adl-annotations]:https://github.com/timbod7/adl/blob/master/docs/language.md#annotations
[adl-haskell]:https://github.com/timbod7/adl/blob/master/docs/backend-haskell.md
[adl-language]:https://github.com/timbod7/adl/blob/master/docs/language.md
[adl-serialization]:https://github.com/timbod7/adl/blob/master/docs/serialization.md
[HaskellFieldPrefix]:https://github.com/timbod7/adl/blob/master/haskell/compiler/lib/adl/adlc/config/haskell.adl#L3
[AdlValue]:https://github.com/timbod7/adl/blob/master/haskell/runtime/src/ADL/Core/Value.hs#L67
[Data.Yaml]:http://hackage.haskell.org/package/yaml
[cabal]:https://www.haskell.org/cabal/
[cargo]:https://doc.rust-lang.org/cargo/
[tsc]:https://www.typescriptlang.org/docs/handbook/compiler-options.html




[stack]:https://docs.haskellstack.org/en/stable/README/
