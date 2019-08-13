## Makefile

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.

all:

TOP          := $(dir $(lastword $(MAKEFILE_LIST)))

UUID         := $(shell ((uuidgen > /dev/null 2>&1 && uuidgen) || echo $$) | cut -c -7)

UBUNTU_EMACS := 24.4 24.5 25.1
ALPINE_EMACS := 25.3 26.1 26.2
DOCKER_EMACS := $(UBUNTU_EMACS:%=ubuntu-min-%) $(ALPINE_EMACS:%=alpine-min-%)

DEPENDS      := helm async popup

EMACS        ?= emacs
BATCH        := $(EMACS) -Q --batch -L $(TOP) $(DEPENDS:%=-L ./%/)

TESTFILE     := helm-swoop-test.el
ELS          := helm-swoop.el

CORTELS      := $(TESTFILE) cort-test.el

##################################################

.PHONY: all git-hook build check allcheck test clean-soft clean

all: git-hook build

##############################

git-hook:
	cp -a dev/git-hooks/* .git/hooks/

##############################

build: $(ELS:%.el=%.elc)

%.elc: %.el $(DEPENDS)
	$(BATCH) $(DEPENDS:%=-L %/) -f batch-byte-compile $<

##############################

fix-indent:
	$(EMACS) $(DEPENDS:%=-L %/) --batch --eval "\
(progn \
  (require 'cl-macs) \
  (require 'helm) \
  (setq indent-tabs-mode nil) \
  (find-file (nth 0 command-line-args-left)) \
  (indent-region (point-min) (point-max)) \
  (princ (buffer-string)))" helm-swoop.el > helm-swoop-tmp.el
	mv helm-swoop-tmp.el helm-swoop.el

##############################
#
#  docker one-time test (on top level)
#

check: build
	$(BATCH) -l $(TESTFILE) -f cort-test-run

##############################
#
#  docker multi Emacs version test (on independent environment)
#

allcheck: $(DOCKER_EMACS:%=.make/verbose-${UUID}-emacs-test--%)
	@echo ""
	@cat $^ | grep =====
	@rm -rf $^

.make/verbose-%: .make $(DEPENDS)
	docker run -itd --name $* conao3/emacs:$(shell echo $* | sed "s/.*--//") /bin/sh
	docker cp . $*:/test
	docker exec $* sh -c "cd test && make clean-soft && make check -j" | tee $@
	docker rm -f $*

##############################
#
#  docker silent `allcheck' job
#

test: $(DOCKER_EMACS:%=.make/silent-${UUID}-emacs-test--%)
	@echo ""
	@cat $^ | grep =====
	@rm -rf $^

.make/silent-%: .make $(DEPENDS)
	docker run -itd --name $* conao3/emacs:$(shell echo $* | sed "s/.*--//") /bin/sh > /dev/null
	@docker cp . $*:/test
	@docker exec $* sh -c "cd test && make clean-soft && make check -j" > $@ || ( docker rm -f $*; cat $@ || false )
	@docker rm -f $* > /dev/null

.make:
	mkdir $@

##############################

clean-soft:
	rm -rf $(ELS:%.el=%.elc) .make

clean:
	rm -rf $(ELS:%.el=%.elc) $(DEPENDS) .make

##############################

helm:
	curl -L https://github.com/emacs-helm/helm/archive/master.tar.gz > $@.tar.gz
	mkdir $@ && tar xf $@.tar.gz -C $@ --strip-components 1
	rm -rf $@.tar.gz

async:
	curl -L https://github.com/jwiegley/emacs-async/archive/master.tar.gz > $@.tar.gz
	mkdir $@ && tar xf $@.tar.gz -C $@ --strip-components 1
	rm -rf $@.tar.gz

popup:
	curl -L https://github.com/auto-complete/popup-el/archive/master.tar.gz > $@.tar.gz
	mkdir $@ && tar xf $@.tar.gz -C $@ --strip-components 1
	rm -rf $@.tar.gz
