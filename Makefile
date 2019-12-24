## Makefile

# Copyright (C) 2019  Naoya Yamashita

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

all:

REPO_USER    := emacsorphanage
PACKAGE_NAME := helm-swoop
REPO_NAME    := helm-swoop

EMACS        ?= emacs
ELS          := $(shell cask files)

GIT_HOOKS    := pre-commit

##################################################

.PHONY: all help build test clean

all: help

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Show this message)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Compile Elisp files and test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `cask`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

%.elc: %.el .cask
	cask exec $(EMACS) -Q --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

##############################

build: $(ELS:%.el=%.elc)

test: build
	cask exec buttercup -L .

clean:
	rm -rf $(ELS:%.el=%.elc) .cask
