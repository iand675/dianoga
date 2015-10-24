{-# LANGUAGE FlexibleInstances #-}
module Dianoga.Minification.JavaScript.ClosureCompiler
( closureCompile
, defaultOptions
, ClosureCompilerOptions(..)
, Env(..)
, CompilationLevel(..)
, Formatting(..)
, LanguageSpec(..)
, WarningClass(..)
, LoggingLevel(..)
, SummaryDetail(..)
, TracerMode(..)
, WarningLevel(..)
, SourceMapFormat(..)
, ModuleName(..)
, Module(..)
, ModuleWrapper(..)
, Define(..)
, SourceMapping(..)
) where

import Data.List
import Development.Shake
import Paths_dianoga

data Env = Browser | Custom
  deriving (Show, Eq)

data CompilationLevel = WhitespaceOnly | Simple | Advanced
  deriving (Show, Eq)

data Formatting = PrettyPrint | PrintInputDelimiter | SingleQuotes
  deriving (Show, Eq)

data LanguageSpec = EcmaScript5
                  | EcmaScript5Strict
                  | EcmaScript6 -- ^ (experimental)
                  | EcmaScript6Strict -- ^ (experimental)
                  | EcmaScript6Typed
                  deriving (Show, Eq)

data WarningClass = AccessControls
                  | AmbiguousFunctionDecl
                  | CheckEventfulObjectDisposal
                  | CheckRegExp
                  | CheckTypes
                  | CheckVars
                  | ConformanceViolations
                  | Const
                  | ConstantProperty
                  | Deprecated
                  | DeprecatedAnnotations
                  | DuplicateMessage
                  | Es3
                  | Es5Strict
                  | ExternsValidation
                  | FileoverviewTags
                  | GlobalThis
                  | InferredConstCheck
                  | InternetExplorerChecks
                  | InvalidCasts
                  | MisplacedTypeAnnotation
                  | MissingGetCssName
                  | MissingProperties
                  | MissingProvide
                  | MissingRequire
                  | MissingReturn
                  | MsgDescriptionsNewCheckTypes
                  | NonStandardJsDocs
                  | ReportUnknownTypes
                  | SuspiciousCode
                  | StrictModuleDepCheck
                  | TypeInvalidation
                  | UndefinedNames
                  | UndefinedVars
                  | UnknownDefines
                  | UnnecessaryCasts
                  | UselessCode
                  | UseOfGoogBase
                  | Visibility
                  | AllWarnings
                  deriving (Show, Eq)

data LoggingLevel = Severe | Warning | Info | Config | Fine | Finer | Finest | Off | All
  deriving (Show, Eq)

data SummaryDetail = NeverPrint | OnIssue | OnCheckTypes | Always
  deriving (Show, Eq)

data TracerMode = TraceAll | TraceRawSize | TraceTimingOnly | TraceOff
  deriving (Show, Eq)

data WarningLevel = WarningQuiet | WarningDefault | WarningVerbose
  deriving (Show, Eq)

data SourceMapFormat = DefaultSourceMapFormat | SourceMapFormatV3
  deriving (Show, Eq)

newtype ModuleName = ModuleName String
  deriving (Show, Eq)

data Module = Module
  { moduleName :: ModuleName
  , moduleNumJsFiles :: Maybe Int
  , moduleDependencies :: [String]
  } deriving (Show, Eq)

data ModuleWrapper = ModuleWrapper ModuleName String
  deriving (Show, Eq)

data Define = Define String String
  deriving (Show, Eq)

data SourceMapping = SourceMapping FilePath String
  deriving (Show, Eq)

data ClosureCompilerOptions = ClosureCompilerOptions
  { angularPass :: Bool
  , charset :: Maybe String
  , checksOnly :: Bool
  , closureEntryPoints :: [String]
  , commonJsEntryModule :: Maybe String
  , commonJsModulePathPrefix :: Maybe String
  , compilationLevel :: Maybe CompilationLevel
  , conformanceConfigs :: Maybe String
  , createRenamingReports  :: Bool
  , createSourceMap :: Maybe FilePath 
  , debug :: Bool
  , define :: [Define]
  , env :: Maybe Env
  , exportLocalPropertyDefinitions :: Bool
  , externs :: [FilePath]
  , extraAnnotationName :: [String]
  , flagfile :: Maybe FilePath
  , formatting :: Maybe Formatting
  , generateExports :: Bool
  , js :: [FilePath]
  , jsModuleRoot :: Maybe String
  , jsOutputFile :: Maybe String
  , jsCompErr :: [WarningClass]
  , jsCompOff :: [WarningClass]
  , jsCompWarning :: [WarningClass]
  , languageIn :: Maybe LanguageSpec
  , languageOut :: Maybe LanguageSpec
  , loggingLevel :: Maybe LoggingLevel
  , manageClosureDependencies :: Bool
  , modules :: [Module]
  , moduleOutputPathPrefix :: Maybe FilePath
  , moduleWrappers :: [ModuleWrapper]
  , onlyClosureDependencies :: Bool
  , outputManifset :: Maybe FilePath
  , outputModuleDependencies :: Maybe FilePath
  , outputWrapper :: Maybe String
  , outputWrapperFile :: [FilePath]
  , polymerPass :: Bool
  , printAst :: Bool
  , printPassGraph :: Bool
  , printTree :: Bool
  , processClosurePrimitives :: Bool
  , processCommonJsModules :: Bool
  , processJqueryPrimitives :: Bool
  , propertyRenamingReport :: Maybe FilePath
  , renamePrefixNamespace :: Maybe String
  , sourceMapFormat :: Maybe SourceMapFormat
  , sourceMapInput :: [SourceMapping]
  , sourceMapLocationMapping :: [SourceMapping]
  , summaryDetailLevel :: Maybe SummaryDetail
  , thirdParty :: Bool
  , tracerMode :: Maybe TracerMode
  , transformAmdModules ::Bool
  , translationsFile :: Maybe FilePath
  , translationsProject :: Maybe String
  , useTypesForOptimization :: Bool
  , variableRenamingReport :: Maybe FilePath
  , warningLevel :: Maybe WarningLevel
  , warningsWhitelistFile :: Maybe FilePath
  } deriving (Show, Eq)

defaultOptions :: ClosureCompilerOptions
defaultOptions = ClosureCompilerOptions
  { angularPass = False
  , charset = Nothing
  , checksOnly = False
  , closureEntryPoints = []
  , commonJsEntryModule = Nothing
  , commonJsModulePathPrefix = Nothing
  , compilationLevel = Nothing
  , conformanceConfigs = Nothing
  , createRenamingReports  = False
  , createSourceMap = Nothing
  , debug = False
  , define = []
  , env = Nothing
  , exportLocalPropertyDefinitions = False
  , externs = []
  , extraAnnotationName = []
  , flagfile = Nothing
  , formatting = Nothing
  , generateExports = False
  , js = []
  , jsModuleRoot = Nothing
  , jsOutputFile = Nothing
  , jsCompErr = []
  , jsCompOff = []
  , jsCompWarning = []
  , languageIn = Nothing
  , languageOut = Nothing
  , loggingLevel = Nothing
  , manageClosureDependencies = False
  , modules = []
  , moduleOutputPathPrefix = Nothing
  , moduleWrappers = []
  , onlyClosureDependencies = False
  , outputManifset = Nothing
  , outputModuleDependencies = Nothing
  , outputWrapper = Nothing
  , outputWrapperFile = []
  , polymerPass = False
  , printAst = False
  , printPassGraph = False
  , printTree = False
  , processClosurePrimitives = False
  , processCommonJsModules = False
  , processJqueryPrimitives = False
  , propertyRenamingReport = Nothing
  , renamePrefixNamespace = Nothing
  , sourceMapFormat = Nothing
  , sourceMapInput = []
  , sourceMapLocationMapping = []
  , summaryDetailLevel = Nothing
  , thirdParty = False
  , tracerMode = Nothing
  , transformAmdModules = False
  , translationsFile = Nothing
  , translationsProject = Nothing
  , useTypesForOptimization = False
  , variableRenamingReport = Nothing
  , warningLevel = Nothing
  , warningsWhitelistFile = Nothing
  }

class AsArgValue a where
  argVal :: a -> String

instance AsArgValue [Char] where
  argVal = id

instance AsArgValue Env where
  argVal x = case x of
    Browser -> "BROWSER"
    Custom -> "CUSTOM"

instance AsArgValue CompilationLevel where
  argVal x = case x of
    WhitespaceOnly -> "WHITESPACE_ONLY"
    Simple -> "SIMPLE"
    Advanced -> "ADVANCED"

instance AsArgValue Formatting where
  argVal x = case x of
    PrettyPrint -> "PRETTY_PRINT"
    PrintInputDelimiter -> "PRINT_INPUT_DELIMITER"
    SingleQuotes -> "SINGLE_QUOTES"

instance AsArgValue LanguageSpec where
  argVal x = case x of
    EcmaScript5 -> "ECMASCRIPT5"
    EcmaScript5Strict -> "ECMASCRIPT5_STRICT"
    EcmaScript6 -> "ECMASCRIPT6"
    EcmaScript6Strict -> "ECMASCRIPT6_STRICT"
    EcmaScript6Typed -> "ECMASCRIPT6_TYPED"

instance AsArgValue WarningClass where
  argVal x = case x of
    AccessControls -> "accessControls"
    AmbiguousFunctionDecl -> "ambiguousFunctionDecl"
    CheckEventfulObjectDisposal -> "checkEventfulObjectDisposal"
    CheckRegExp -> "checkRegExp"
    CheckTypes -> "checkTypes"
    CheckVars -> "checkVars"
    ConformanceViolations -> "conformanceViolations"
    Const -> "const"
    ConstantProperty -> "constantProperty"
    Deprecated -> "deprecated"
    DeprecatedAnnotations -> "deprecatedAnnotations"
    DuplicateMessage -> "duplicateMessage"
    Es3 -> "es3"
    Es5Strict -> "es5Strict"
    ExternsValidation -> "externsValidation"
    FileoverviewTags -> "fileoverviewTags"
    GlobalThis -> "globalThis"
    InferredConstCheck -> "inferredConstCheck"
    InternetExplorerChecks -> "internetExplorerChecks"
    InvalidCasts -> "invalidCasts"
    MisplacedTypeAnnotation -> "misplacedTypeAnnotation"
    MissingGetCssName -> "missingGetCssName"
    MissingProperties -> "missingProperties"
    MissingProvide -> "missingProvide"
    MissingRequire -> "missingRequire"
    MissingReturn -> "missingReturn"
    MsgDescriptionsNewCheckTypes -> "msgDescriptionsnewCheckTypes"
    NonStandardJsDocs -> "nonStandardJsDocs"
    ReportUnknownTypes -> "reportUnknownTypes"
    SuspiciousCode -> "suspiciousCode"
    StrictModuleDepCheck -> "strictModuleDepCheck"
    TypeInvalidation -> "typeInvalidation"
    UndefinedNames -> "undefinedNames"
    UndefinedVars -> "undefinedVars"
    UnknownDefines -> "unknownDefines"
    UnnecessaryCasts -> "unnecessaryCasts"
    UselessCode -> "uselessCode"
    UseOfGoogBase -> "useOfGoogBase"
    Visibility -> "visibility"
    AllWarnings -> "*"

instance AsArgValue LoggingLevel where
  argVal x = case x of
    Severe -> "SEVERE"
    Warning -> "WARNING"
    Info -> "INFO"
    Config -> "CONFIG"
    Fine -> "FINE"
    Finer -> "FINER"
    Finest -> "FINEST"
    Off -> "OFF"
    All -> "ALL"

instance AsArgValue SummaryDetail where
  argVal x = case x of
    NeverPrint -> "0"
    OnIssue -> "1"
    OnCheckTypes -> "2"
    Always -> "3"

instance AsArgValue TracerMode where
  argVal x = case x of
    TraceAll -> "ALL"
    TraceRawSize -> "RAW_SIZE"
    TraceTimingOnly -> "TIMING_ONLY"
    TraceOff -> "OFF"

instance AsArgValue WarningLevel where
  argVal x = case x of
    WarningQuiet   -> "QUIET"
    WarningDefault -> "DEFAULT"
    WarningVerbose -> "VERBOSE"


instance AsArgValue Module where
  argVal (Module (ModuleName n) mcount deps) = n ++ ":" ++ maybe "auto" show mcount ++ formattedDeps deps
    where
      formattedDeps [] = []
      formattedDeps ds = ':' : intercalate "," ds

instance AsArgValue ModuleWrapper where
  argVal (ModuleWrapper (ModuleName n) fmt) = n ++ ":" ++ fmt

instance AsArgValue Define where
  argVal (Define k v) = k ++ "='" ++ escapeSingleQuotes v ++ "'"
    where escapeSingleQuotes = concatMap (\c -> if c == '\'' then "\\'" else [c])

instance AsArgValue SourceMapFormat where
  argVal x = case x of
    DefaultSourceMapFormat -> "DEFAULT"
    SourceMapFormatV3 -> "V3"

instance AsArgValue SourceMapping where
  argVal (SourceMapping src dest) = src ++ "|" ++ dest

class AsArgList a where
  build :: String -> a -> [String]

instance AsArgValue a => AsArgList [a] where
  build str = concatMap (\val -> [str, argVal val])

instance AsArgValue a => AsArgList (Maybe a) where
  build str = maybe [] (\val -> [str, argVal val])

instance AsArgList Bool where
  build str toggle = if toggle then [str] else []

closureCompile :: CmdResult a => [CmdOption] -> ClosureCompilerOptions -> Action a
closureCompile opts args = do
  jarPath <- liftIO $ getDataFileName "external/closure-compiler/compiler.jar"
  command opts "java" . (["-jar", jarPath] ++) $ compilerArgs args

compilerArgs :: ClosureCompilerOptions -> [String]
compilerArgs x = concat
  [ build "--angular_pass" $ angularPass x
  , build "--charset" $ charset x
  , build "--checks-only" $ checksOnly x
  , build "--closure_entry_point" $ closureEntryPoints x
  , build "--common_js_entry_module" $ commonJsEntryModule x
  , build "--common_js_module_path_prefix" $ commonJsModulePathPrefix x
  , build "--compilation_level" $ compilationLevel x
  , build "--conformance_configs" $ conformanceConfigs x
  , build "--create_renaming_reports" $ createRenamingReports  x
  , build "--create_source_map" $ createSourceMap x
  , build "--debug" $ debug x
  , build "--define" $ define x
  , build "--env" $ env x
  , build "--export_local_property_definitions" $ exportLocalPropertyDefinitions x
  , build "--externs" $ externs x
  , build "--extra_annotation_name" $ extraAnnotationName x
  , build "--flagfile" $ flagfile x
  , build "--formatting" $ formatting x
  , build "--generate_exports" $ generateExports x
  , build "--js" $ js x
  , build "--js_module_root" $ jsModuleRoot x
  , build "--js_output_file" $ jsOutputFile x
  , build "--jscomp_err" $ jsCompErr x
  , build "--jscomp_off" $ jsCompOff x
  , build "--jscomp_warning" $ jsCompWarning x
  , build "--language_in" $ languageIn x
  , build "--language_out" $ languageOut x
  , build "--logging_level" $ loggingLevel x
  , build "--manage_closure_dependencies" $ manageClosureDependencies x
  , build "--module" $ modules x
  , build "--module_output_path_prefix" $ moduleOutputPathPrefix x
  , build "--module_wrapper" $ moduleWrappers x
  , build "--only_closure_dependencies" $ onlyClosureDependencies x
  , build "--output_manifest" $ outputManifset x
  , build "--output_module_dependencies" $ outputModuleDependencies x
  , build "--output_wrapper" $ outputWrapper x
  , build "--output_wrapper_file" $ outputWrapperFile x
  , build "--polymer_pass" $ polymerPass x
  , build "--print_ast" $ printAst x
  , build "--print_pass_graph" $ printPassGraph x
  , build "--print_tree" $ printTree x
  , build "--process_closure_primitives" $ processClosurePrimitives x
  , build "--process_common_js_modules" $ processCommonJsModules x
  , build "--process_jquery_primitives" $ processJqueryPrimitives x
  , build "--property_renaming_report" $ propertyRenamingReport x
  , build "--rename_prefix_namespace" $ renamePrefixNamespace x
  , build "--source_map_format" $ sourceMapFormat x
  , build "--source_map_input" $ sourceMapInput x
  , build "--source_map_location_mapping" $ sourceMapLocationMapping x
  , build "--summary_detail_level" $ summaryDetailLevel x
  , build "--third_party" $ thirdParty x
  , build "--tracer_mode" $ tracerMode x
  , build "--transform_amd_modules" $ transformAmdModules x
  , build "--translations_file" $ translationsFile x
  , build "--translations_project" $ translationsProject x
  , build "--use_types_for_optimization" $ useTypesForOptimization x
  , build "--variable_renaming_report" $ variableRenamingReport x
  , build "--warning_level" $ warningLevel x
  , build "--warnings_whitelist_file" $ warningsWhitelistFile x
  ]
