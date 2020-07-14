PWSH = pwsh

.PHONY: build
build: build-ghc-8.6 build-ghc-8.8 build-nightly

.PHONY: build-ghc-8.6
build-ghc-8.6: build-deps-ghc-8.6 src
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror

.PHONY: build-ghc-8.8
build-ghc-8.8: build-deps-ghc-8.8 src
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror

.PHONY: build-nightly
build-nightly: build-deps-nightly src
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror

.PHONY: build-deps-ghc-8.6
build-deps-ghc-8.6: stack-ghc-8.6.yaml package.yaml
	stack --stack-yaml stack-ghc-8.6.yaml build --only-dependencies

.PHONY: build-deps-ghc-8.8
build-deps-ghc-8.8: stack-ghc-8.8.yaml package.yaml
	stack --stack-yaml stack-ghc-8.8.yaml build --only-dependencies

.PHONY: build-deps-nightly
build-deps-nightly: stack-nightly.yaml package.yaml
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --only-dependencies

.PHONY: test
test: test-db-ghc-8.6 test-db-ghc-8.8 test-db-nightly

.PHONY: test-db-ghc-8.6
test-db-ghc-8.6: build-ghc-8.6 src
	stack --stack-yaml stack-ghc-8.6.yaml build --ghc-options -Werror relational-query-postgresql-pure:test:db

.PHONY: test-db-ghc-8.8
test-db-ghc-8.8: build-ghc-8.8 src
	stack --stack-yaml stack-ghc-8.8.yaml build --ghc-options -Werror relational-query-postgresql-pure:test:db

.PHONY: test-db-nightly
test-db-nightly: build-nightly src
	stack --stack-yaml stack-nightly.yaml --resolver nightly build --ghc-options -Werror relational-query-postgresql-pure:test:db

.PHONY: format
format:
	$(PWSH) -Command '& { Get-ChildItem -Path src, test-db -Include *.hs -Recurse | ForEach-Object { stack exec -- stylish-haskell --inplace $$_ } }'
	stack exec -- stylish-haskell --inplace Setup.hs

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content .\Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"

.PHONY: clean
clean:
	stack --stack-yaml stack-ghc-8.6.yaml clean
	stack --stack-yaml stack-ghc-8.8.yaml clean
	stack --stack-yaml stack-nightly.yaml clean

.PHONY: clean-full
clean-full:
	stack --stack-yaml stack-ghc-8.6.yaml clean --full
	stack --stack-yaml stack-ghc-8.8.yaml clean --full
	stack --stack-yaml stack-nightly.yaml clean --full
