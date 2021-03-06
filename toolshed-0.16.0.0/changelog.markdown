# 2010-11-18 Dr. Alistair Ward <toolshed at functionalley dot eu>

## 0.9.0.0
	* First version of the package.
## 0.10.0.0
	* Created **src/ToolShed/** sub-directory & then modified module-names accordingly.
	* Removed the module **Pair**, & relocated its only function to the sole caller in the package **Bridge**.
	* Added a makefile.
	* Qualified identifiers used in error-messages.
## 0.10.0.1
	* Tested with ghc-7.0.1, & conditionally replaced use of the package **parallel** with **deepseq**, in the module **TimePure**.
## 0.10.1.0
	* Added `ToolShed.Arithmetic.nCr`, **ToolShed.Options** & **ToolShed.Package**.
## 0.10.2.0
	* Renamed package from **ToolShed** to **toolshed**, for compatibility with Debian's *.deb*-format.
## 0.11.0.0
	* Added manually controlled **llvm** flag to the *.cabal*-file.
	* Removed the module **ToolShed.Package**, for which **Distribution.Package.PackageIdentifier** is a suitable alternative.
	* Added **NOINLINE** pragma to each function in module **Unsafe**.
	* Relocated `mean` & `nCr` from **ToolShed.Arithmetic**, to the package **factory**.
	* Added function `ListPlus.groupComparing`.
	* Changed identifier for type-parameters, to better reflect its role.
	* Relocated the remaining rather pointless functions from the module **ToolShed.Arithmetic** & deleted it.
	* Added module **ToolShed.Defaultable**, & derived class `ToolShed.Options.Options` from it.
## 0.11.1.0
	* Added functions `merge` & `mergeBy` to module **ToolShed.ListPlus** & exported a new type-synonym `ToolShed.ListPlus.ChunkLength`.
	* Uploaded to [Hackage](http://hackage.haskell.org/packages/hackage.html).
## 0.12.0.0
	* Added modules **ToolShed.TimeAction** to measure the CPU-seconds required for an IO-action, & **Pair**.
## 0.12.0.1
	* Removed unnecessary type-context `Ord` from `ToolShed.ListPus.mergeBy` & reimplemented `ToolShed.ListPus.merge` in terms of `ToolShed.ListPus.mergeBy`.
	* Added `ToolShed.ListPlus.runLengthEncode` & `ToolShed.ListPlus.runLengthDecode`.
## 0.13.0.0
	* Removed module **ToolShed.Unsafe**.
	* Renamed:
		**ToolShed.ListPlus** to **ToolShed.Data.List**
		**ToolShed.Pair** to **ToolShed.Data.Pair**
		**ToolShed.TimeAction** to **ToolShed.System.TimeAction**
		**ToolShed.TimePure** to **ToolShed.System.TimePure**
	* Migrated **ToolShed.Data.List.RunLengthCode** & associated functions, into a new module **ToolShed.Data.List.Runlength**.
	* Migrated **ToolShed.Data.List.Splits** & associated functions, into a new module **ToolShed.Data.List.Splits**.
	* Replaced function `ToolShed.Data.List.groupComparing` with the more useful `ToolShed.Data.List.equalityBy`, which can be used with either `Data.List.groupBy` or `Data.List.nubBy`.
	* Added instances of `ToolShed.SelfValidate.SelfValidator` for `Data.Maybe.Maybe` `(,)`, `Data.Set.Set`, `Data.Map.Map` & `Data.Array.IArray.Array`.
	* Added instance of `ToolShed.Defaultable.Defaultable` for `(,)`.
	* Added functions `ToolShed.Data.List.showListWith`, `ToolShed.Data.List.permutations`, `ToolShed.Data.List.gatherBy`, `ToolShed.Data.List.findConvergenceBy`, `ToolShed.Data.List.findConvergence`, `ToolShed.Data.List.gather`.
	* Added modules **ToolShed.System.File** & **ToolShed.System.Random**.
	* Added method `ToolShed.SelfValidate.getErrors`, & supporting functions `ToolShed.SelfValidate.getFirstErrors` & `ToolShed.SelfValidate.extractErrors`.
	* Create module **ToolShed.Arbitrary.* **.
	* Added **Main**, **ToolShed.Test.QuickChecks** & **ToolShed.Test.Data.List** to facilitate testing.
	* Removed `ToolShed.Data.Pair.both`.
	` Replaced `System` with `System.Environment` & `System.Exit`.
	* Removed dependency on **haskell98**.
## 0.14.0.0
	* Defined package's name using program's name, in **Main.hs**.
	* Checked package **Base** exports `Data.Tuple.swap`, in **ToolShed.Arbitrary.Array**.
	* Relocated **ToolShed.Arbitrary** to **ToolShed.Test.QuickCheck.Arbitrary**.
	* Added **ToolShed.Test.ReversibleBoundedEnum**, **ToolShed.Test.ReversibleEnum**, **ToolShed.Test.ReversibleIO**, & **ToolShed.Test.SelfValidate**.
	* Amended `ToolShed.System.File.locate` to deal with absolute file-paths.
	* Replaced legacy calls to `Data.Map.fold` & `Data.Set.fold`, with more expicit calls to `Data.Map.foldr` & `Data.Set.foldr` from a more recent version of the **containers** package.
	* Added instance of `ToolShed.SelfValidate.SelfValidator` for `(,,)`.
	* Trapped null list in `ToolShed.System.Random.select`.
	* Added functions `ToolShed.Data.List.nub'` & 'ToolShed.Data.List.hasDuplicates'.
	* Migrated functions `gather`, `gatherBy`, `hasDuplicates` which merely operate on `Data.Foldable.Foldable` types from module **ToolShed.Data.List**, into new module **ToolShed.Data.Foldable**.
## 0.15.0.0
	* Refactored **ToolShed.Test.QuickChecks**.
	* Added type `ToolShed.Data.List.Matches` & function `ToolShed.Data.List.permutationsBy`.
	* Trapped the case of a null list supplied to `ToolShed.Data.List.permutationsBy`.
	* Re-implemented `ToolShed.System.Random.shuffle`, using the Fisher-Yates algorithm; which also improved the efficiency.
	* Trapped special cases in `ToolShed.System.Random.generateSelection`.
	* Added modules **ToolShed.Test.System.Random**, **ToolShed.Data.Triple**, **ToolShed.Data.Quadruple**, **ToolShed.Test.Data.Triple**, & **ToolShed.Test.Data.Quadruple**.
	* Explicitly closed standard-input in the executable.
	* Replaced calls to `error` from inside the IO-monad, with `Control.Monad.fail`.
	* Added an instance for `(,,)`, to **ToolShed.Defaultable.Defaultable**.
	* Corrected version-string in **Main** used in **--version** option.
	* Corrected the output of `Main.main.optDescrList.printVersion`.
## 0.15.0.1
	* Tested with **haskell-platform-2013.2.0.0**.
	* Replaced preprocessor-directives with **build-depends** constraints in the *.cabal*-file.
	* In module **ToolShed.System.File**, replaced `Control.Exception.throw` with `Control.Exception.throwIO`.
	* Replaced all instances of `(<$>)` with `fmap` to avoid ambiguity between **Control.Applicative** & **Prelude** which (from **base-4.8**) also exports this symbol.
## 0.16.0.0
	* Corrected the copyright dates in **Main**.
	* Renamed module **ToolShed.Test.QuickCheck.Arbitrary.ArrayElem** to **ToolShed.Test.QuickCheck.Arbitrary.Array**, overwriting the module originally of that name which pointlessly defined an instance of `Test.QuickCheck.Arbitrary` for an array with an unbounded index.
	* Added **Default-language**-specification to the *.cabal*-file.
	* Added file **README.markdown**.
	* Converted this file to markdown-format.
	* Replaced `System.Exit.exitWith System.Exit.ExitSuccess` with `System.Exit.exitSuccess`.
	* Added module **Test.QuickCheck.Result**.
	* Moved the entry-point to the test-suite from **Main.hs** to **Test.hs** to integrate with **cabal**.
	* Partitioned the source-files into **src-lib** & **src-test** directories, & referenced them individually from the *.cabal*-file to avoid repeated compilation.

