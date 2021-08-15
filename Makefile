# This file is part of the Watson Conversation Service OCaml API project.
#
# Copyright 2016-2017 IBM Corporation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


DUNE ?= dune

all: build

build:
	$(DUNE) build @install

lock: ## Generate a lock file
	opam lock -y .
	
test:
	$(DUNE) runtest

doc:
	$(DUNE) build @doc

webdoc: doc
	cp -rf _build/default/_doc/ docs/
	rm -f docs/index.html

indent:
	@find . \( -name '*.ml' -exec ocp-indent -i \{\} + \)
	@find . \( -name '*.mli' -exec ocp-indent -i \{\} + \)

install:
	$(DUNE) install

uninstall:
	$(DUNE) uninstall

clean:
	rm -rf _build \
		wcs-lib.install \
		wcs.install

cleanall: clean
	rm -f *~ */*~ \
		wcs-cli/.merlin \
		wcs-lib/.merlin \
		examples/.merlin

realcleanall: cleanall
	rm -f bin/wcs

check:  ## Build and check
	dune build @check

.PHONY: all build test doc clean cleanall check
